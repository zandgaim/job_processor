defmodule JobHttpHandlerIntegrationTest do
  use ExUnit.Case

  setup_all do
    # Start Cowboy server
    dispatch =
      :cowboy_router.compile([
        {:_,
         [
           {"/", :job_http_handler, []}
         ]}
      ])

    {:ok, _pid} =
      :cowboy.start_clear(:http, [{:port, 8081}], %{env: %{dispatch: dispatch}})

    on_exit(fn ->
      :cowboy.stop_listener(:http)
    end)

    :ok
  end

  @tasks %{
    "tasks" => [
      %{"name" => "task-1", "command" => "echo 'A'"},
      %{"name" => "task-2", "command" => "echo 'B'", "requires" => ["task-1"]}
    ]
  }

  defp gun_post(path, body, headers) do
    {:ok, conn_pid} = :gun.open(~c"localhost", 8081)
    {:ok, _protocol} = :gun.await_up(conn_pid)

    stream_ref = :gun.post(conn_pid, path, headers, body)

    {:response, :nofin, status, resp_headers} = :gun.await(conn_pid, stream_ref)
    {:data, :fin, resp_body} = :gun.await(conn_pid, stream_ref)

    :gun.close(conn_pid)
    {status, resp_headers, resp_body}
  end

  defp gun_get(path) do
    {:ok, conn_pid} = :gun.open(~c"localhost", 8081)
    {:ok, _protocol} = :gun.await_up(conn_pid)

    stream_ref = :gun.get(conn_pid, path)

    {:response, :nofin, status, resp_headers} = :gun.await(conn_pid, stream_ref)
    {:data, :fin, resp_body} = :gun.await(conn_pid, stream_ref)

    :gun.close(conn_pid)
    {status, resp_headers, resp_body}
  end

  test "GET before any POST returns 404" do
    :job_storage.clear()
    {404, _resp_headers, body} = gun_get("/")
    decoded = :jsx.decode(body, return_maps: true)

    assert decoded["error"] == "No script stored yet"
  end

  test "POST tasks returns sorted tasks, then GET returns bash script" do
    json_body = :jsx.encode(@tasks)
    headers = [{<<"content-type">>, <<"application/json">>}]

    # First POST /
    {200, _resp_headers, body} = gun_post("/", json_body, headers)
    decoded = :jsx.decode(body, return_maps: true)

    assert Map.has_key?(decoded, "tasks")
    assert length(decoded["tasks"]) == 2
    assert hd(decoded["tasks"])["name"] == "task-1"

    # Then GET should return generated bash script
    {200, _resp_headers, body} = gun_get("/")

    script = :jsx.decode(body, return_maps: true)

    assert script =~ "echo 'A'"
    assert script =~ "echo 'B'"
    assert String.starts_with?(script, "#!/usr/bin/env bash")
  end

  test "POST with invalid JSON returns 400" do
    headers = [{<<"content-type">>, <<"application/json">>}]

    {400, _resp_headers, body} = gun_post("/", "{invalid-json}", headers)
    decoded = :jsx.decode(body, return_maps: true)

    assert decoded["error"] == "Invalid JSON"
  end

  test "POST with missing tasks key returns 400" do
    headers = [{<<"content-type">>, <<"application/json">>}]
    bad_payload = %{"foo" => "bar"}

    {400, _resp_headers, body} = gun_post("/", :jsx.encode(bad_payload), headers)
    decoded = :jsx.decode(body, return_maps: true)

    assert decoded["error"] == "Bad Request"
  end

  test "POST with cyclic dependency returns 500" do
    headers = [{<<"content-type">>, <<"application/json">>}]

    tasks = %{
      "tasks" => [
        %{"name" => "a", "command" => "echo 'A'", "requires" => ["b"]},
        %{"name" => "b", "command" => "echo 'B'", "requires" => ["a"]}
      ]
    }

    {500, _resp_headers, body} = gun_post("/", :jsx.encode(tasks), headers)
    decoded = :jsx.decode(body, return_maps: true)

    assert decoded["error"] == "Internal Server Error"
  end
end
