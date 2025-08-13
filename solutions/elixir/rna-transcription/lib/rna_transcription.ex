defmodule RnaTranscription do
  @doc """
  Transcribes a character list representing DNA nucleotides to RNA

  ## Examples

  iex> RnaTranscription.to_rna('ACTG')
  'UGAC'
  """
  @spec to_rna([char]) :: [char]
  def to_rna(dna), do: transcribe(dna, [])

  @doc false
  @spec transcribe([char], [char]) :: [char]
  defp transcribe([], final), do: final

  defp transcribe([nucleotide | rest], final) do
    case nucleotide do
      ?G -> transcribe(rest, final ++ 'C')
      ?C -> transcribe(rest, final ++ 'G')
      ?T -> transcribe(rest, final ++ 'A')
      ?A -> transcribe(rest, final ++ 'U')
    end
  end
end
