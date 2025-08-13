CLASS zcl_isogram DEFINITION PUBLIC.

  PUBLIC SECTION.
    METHODS is_isogram
      IMPORTING
        VALUE(phrase)        TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_isogram IMPLEMENTATION.

  METHOD is_isogram.
    " An isogram is a word or phrase without a repeating
    " letter, however spaces and hyphens are allowed to
    " appear multiple times.
    DATA(clean) = to_lower( phrase ).
    DATA(offset) = 0.

    DO strlen( clean ) - 1 TIMES.
      DATA(current) = clean+offset(1).
      offset += 1.

      IF current NA ' -' AND clean+offset CA current.
        RETURN.
      ENDIF.
    ENDDO.

    result = abap_true.
  ENDMETHOD.

ENDCLASS.
