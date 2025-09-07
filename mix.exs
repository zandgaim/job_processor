defmodule JobProcessor.MixProject do
  use Mix.Project

  def project do
    [
      app: :job_processor,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      compilers: [:erlang, :elixir] ++ Mix.compilers(),
      erlc_paths: ["src"]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {JobProcessor.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:cowboy, "~> 2.10"},
      {:jsx, "~> 3.1"},
      {:gun, "~> 2.0", hex: :gun, only: :test},
      {:meck, "~> 0.9", only: :test},
      {:finch, "~> 0.16"},
      {:req, "~> 0.4"}
    ]
  end
end
