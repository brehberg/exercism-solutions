CLASS zcl_rna_transcription DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    METHODS
      transcribe
        IMPORTING
          strand        TYPE string
        RETURNING
          VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    " mapping of each DNA nucleotide to its RNA complement
    TYPES:
      BEGIN OF mapping,
        dna TYPE c LENGTH 1,
        rna TYPE c LENGTH 1,
      END OF mapping.
    CLASS-DATA dna_to_rna TYPE HASHED TABLE OF mapping WITH UNIQUE KEY dna.
ENDCLASS.


CLASS zcl_rna_transcription IMPLEMENTATION.

  METHOD transcribe.
    DATA(offset) = 0.
    DO strlen( strand ) TIMES.
        result = result && VALUE #( dna_to_rna[ dna = strand+offset(1) ]-rna OPTIONAL ).
        offset += 1.
    ENDDO.
  ENDMETHOD.

  METHOD class_constructor.
    dna_to_rna = VALUE #(
      ( dna = 'G' rna = 'C' )
      ( dna = 'C' rna = 'G' )
      ( dna = 'T' rna = 'A' )
      ( dna = 'A' rna = 'U' )
    ).
  ENDMETHOD.

ENDCLASS.