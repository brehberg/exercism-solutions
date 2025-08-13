CLASS zcl_crypto_square DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS encode
      IMPORTING plain_text         TYPE string
      RETURNING VALUE(crypto_text) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS normalize
      IMPORTING input         TYPE string
      RETURNING VALUE(result) TYPE string.

ENDCLASS.



CLASS zcl_crypto_square IMPLEMENTATION.

  METHOD encode.

    DATA(clean) = normalize( plain_text ).
    DATA(max) = strlen( clean ).
    CHECK max > 0.

    DATA(cols) = ceil( sqrt( max ) ).
    DATA(rows) = ceil( max / cols ).
    " add spaces to fill partial rows plus extra blank row
    max = cols * ( rows  + 1 ) - 1.
    clean = |{ clean WIDTH = max PAD = ` ` }|.

    crypto_text = REDUCE #(
       INIT str = ``
       FOR start = 0 WHILE start < cols
       FOR n = start THEN n + cols WHILE n < max
       NEXT str = str && clean+n(1) ).

  ENDMETHOD.

  METHOD normalize.
    result = to_lower( input ).
    REPLACE ALL OCCURRENCES OF REGEX '\W' IN result WITH ''.
  ENDMETHOD.

ENDCLASS.