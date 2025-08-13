CLASS zcl_rle DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS encode IMPORTING input         TYPE string
                   RETURNING VALUE(result) TYPE string.
    METHODS decode IMPORTING input         TYPE string
                   RETURNING VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS encode_chunk
      IMPORTING n             TYPE i
                c             TYPE string
      RETURNING VALUE(result) TYPE string.
    METHODS decode_match
      IMPORTING input         TYPE string
      RETURNING VALUE(result) TYPE string.

ENDCLASS.


CLASS zcl_rle IMPLEMENTATION.

  METHOD encode.
    CHECK input IS NOT INITIAL.

    DATA(done) = strlen( input ) - 1.
    DATA(n) = 0.
    DATA(count) = 1.
    DATA(last) = input(1).

    WHILE n < done.
      n += 1.

      IF input+n(1) = last.
        count += 1.
        CONTINUE.
      ENDIF.

      result = |{ result }{ encode_chunk( n = count c = last ) }|.
      count = 1.
      last = input+n(1).
    ENDWHILE.

    result = |{ result }{ encode_chunk( n = count c = last ) }|.
  ENDMETHOD.

  METHOD encode_chunk.
    result = COND #( WHEN n = 1 THEN c ELSE |{ n }{ c }| ).
  ENDMETHOD.

  METHOD decode.

    DATA(n) = 0.
    DO.
      n += 1.
      DATA(substr) = match( val = input
                            regex = '\d*.'
                            occ = n ).
      IF substr IS INITIAL.
        RETURN.
      ENDIF.
      result = |{ result }{ decode_match( substr ) }|.
    ENDDO.

  ENDMETHOD.

  METHOD decode_match.
    DATA(offset) = strlen( input ) - 1.
    DATA(c) = input+offset(1).
    DATA(n) = COND #( WHEN offset = 0 THEN 1 ELSE input(offset) ).
    result = repeat( val = c occ = n ).
  ENDMETHOD.

ENDCLASS.