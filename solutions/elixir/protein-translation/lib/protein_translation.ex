defmodule ProteinTranslation do
  @error_rna "invalid RNA"
  @error_codon "invalid codon"
  @stop_codon "STOP"

  @doc """
  Given an RNA string, return a list of proteins specified by codons, in order.
  """
  @spec of_rna(String.t()) :: {:ok, list(String.t())} | {:error, String.t()}
  def of_rna(rna) do
    result = parse_proteins(rna)
    if List.last(result) == @error_codon, do: {:error, @error_rna}, else: {:ok, result}
  end

  @doc false
  @spec parse_proteins(String.t()) :: list(String.t())
  defp parse_proteins(""), do: []
  defp parse_proteins(<<codon::binary-size(3), rest::binary>>) do
    case of_codon(codon) do
      {:ok, @stop_codon} -> []
      {:ok, protein} -> [protein | parse_proteins(rest)]
      {:error, text} -> [text]
    end
  end
  defp parse_proteins(_), do: [@error_codon]

  @doc """
  Given a codon, return the corresponding protein

  UGU -> Cysteine
  UGC -> Cysteine
  UUA -> Leucine
  UUG -> Leucine
  AUG -> Methionine
  UUU -> Phenylalanine
  UUC -> Phenylalanine
  UCU -> Serine
  UCC -> Serine
  UCA -> Serine
  UCG -> Serine
  UGG -> Tryptophan
  UAU -> Tyrosine
  UAC -> Tyrosine
  UAA -> STOP
  UAG -> STOP
  UGA -> STOP
  """
  @spec of_codon(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def of_codon(codon) do
    cond do
      codon == "AUG" -> {:ok, "Methionine"}
      codon == "UGG" -> {:ok, "Tryptophan"}
      codon in ["UGU", "UGC"] -> {:ok, "Cysteine"}
      codon in ["UUA", "UUG"] -> {:ok, "Leucine"}
      codon in ["UUU", "UUC"] -> {:ok, "Phenylalanine"}
      codon in ["UAU", "UAC"] -> {:ok, "Tyrosine"}
      codon in ["UCU", "UCC", "UCA", "UCG"] -> {:ok, "Serine"}
      codon in ["UAA", "UAG", "UGA"] -> {:ok, @stop_codon}
      true -> {:error, @error_codon}
    end
  end
end
