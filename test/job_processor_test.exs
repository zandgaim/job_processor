defmodule JobProcessorTest do
  use ExUnit.Case

  test "task execution order and command correctness" do
    request = %{
      "tasks" => [
        %{"name" => "task-4", "command" => "rm /tmp/file1", "requires" => ["task-2", "task-3"]},
        %{"name" => "task-2", "command" => "cat /tmp/file1", "requires" => ["task-3"]},
        %{"name" => "task-1", "command" => "touch /tmp/file1"},
        %{
          "name" => "task-3",
          "command" => "echo 'Hello World!' > /tmp/file1",
          "requires" => ["task-1"]
        }
      ]
    }

    {:ok, result} = :job_processor.process(request)

    expected_result = %{
      "tasks" => [
        %{"command" => "touch /tmp/file1", "name" => "task-1"},
        %{
          "command" => "echo 'Hello World!' > /tmp/file1",
          "name" => "task-3"
        },
        %{"command" => "cat /tmp/file1", "name" => "task-2"},
        %{"command" => "rm /tmp/file1", "name" => "task-4"}
      ]
    }

    assert expected_result == result
  end

  test "empty tasks list returns error" do
    request = %{"tasks" => []}
    assert {:error, :bad_request} = :job_processor.process(request)
  end

  test "missing tasks key returns error" do
    request = %{"foo" => "bar"}
    assert {:error, :bad_request} = :job_processor.process(request)
  end

  test "cyclic dependency returns exit" do
    request = %{
      "tasks" => [
        %{"name" => "a", "command" => "echo 'A'", "requires" => ["b"]},
        %{"name" => "b", "command" => "echo 'B'", "requires" => ["a"]}
      ]
    }

    assert {:error, :server_error} = :job_processor.process(request)
  end
end
