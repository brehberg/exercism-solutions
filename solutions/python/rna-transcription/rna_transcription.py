"""RNA Transcription"""

DNA2RNA = {"G": "C", "C": "G", "T": "A", "A": "U"}


def to_rna(dna_strand):
    """Given a DNA strand, return its RNA Complement Transcription."""

    return "".join([DNA2RNA[nucleotide] for nucleotide in dna_strand])
