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
    DATA(rows) = max / cols.
    DATA(start) = 0.

    WHILE start < cols.
      DATA(n) = start.
      DO rows TIMES.
        crypto_text = crypto_text && COND #(
            WHEN n < max THEN clean+n(1) ELSE ` ` ).
        n += cols.
      ENDDO.
      start += 1.
      IF start < cols.
        crypto_text = crypto_text && ` `.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.

  METHOD normalize.
    result = to_lower( input ).
    REPLACE ALL OCCURRENCES OF REGEX '\W' IN result WITH ''.
  ENDMETHOD.

ENDCLASS.