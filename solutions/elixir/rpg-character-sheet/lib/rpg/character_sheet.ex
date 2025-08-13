defmodule RPG.CharacterSheet do
  @moduledoc """
  RPG Character Sheet
  """

  @doc """
  Welcome the new player
  """
  @spec welcome() :: :ok
  def welcome(),
    do: IO.puts("Welcome! Let's fill out your character sheet together.")

  @doc """
  Ask for the character's name
  """
  @spec ask_name() :: String.t()
  def ask_name(),
    do:
      IO.gets("What is your character's name?\n")
      |> String.trim()

  @doc """
  Ask for the character's class
  """
  @spec ask_class() :: String.t()
  def ask_class(),
    do:
      IO.gets("What is your character's class?\n")
      |> String.trim()

  @doc """
  Ask for the character's level
  """
  @spec ask_level() :: integer()
  def ask_level(),
    do:
      IO.gets("What is your character's level?\n")
      |> String.trim()
      |> String.to_integer()

  @doc """
  Combine previous steps into one
  """
  @spec run() :: %{:name => String.t(), :class => String.t(), :level => integer()}
  def run(),
    do:
      if(welcome() == :ok,
        do:
          IO.inspect(
            %{
              name: ask_name(),
              class: ask_class(),
              level: ask_level()
            },
            label: "Your character"
          )
      )
end
