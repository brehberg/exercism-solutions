CLASS zcl_atbash_cipher DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS decode
      IMPORTING
        cipher_text TYPE string
      RETURNING
        VALUE(plain_text)  TYPE string .
    METHODS encode
      IMPORTING
        plain_text        TYPE string
      RETURNING
        VALUE(cipher_text) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA plain TYPE string.
    DATA cipher TYPE string.
    DATA group_size TYPE i.
    METHODS replace_text
      IMPORTING
        input_text TYPE string
        from_text TYPE string
        new_text TYPE string
      RETURNING
        VALUE(output)  TYPE string .      
ENDCLASS.


CLASS zcl_atbash_cipher IMPLEMENTATION.

  METHOD decode.
    plain_text = replace_text(
        input_text = cipher_text
        from_text = cipher
        new_text = plain
    ).
  ENDMETHOD.

  METHOD encode.
    cipher_text = replace_text(
        input_text = plain_text
        from_text = plain
        new_text = cipher
    ).
    DATA(offset) = group_size.
    DATA(splits) = ( strlen( cipher_text ) - 1 ) DIV group_size.
    DO splits TIMES.
      cipher_text = |{ cipher_text(offset) } { cipher_text+offset }|.
      offset = offset + group_size + 1.
    ENDDO.
  ENDMETHOD.

  METHOD replace_text.
    DATA(input) = condense( val = to_lower( input_text ) to = `` ).
    DATA offset TYPE i.
    DO strlen( input ) TIMES.
      DATA position TYPE i.
      position = find( val = from_text sub = input+offset(1) ).
      IF position <> -1.
        output = output && new_text+position(1).
      ENDIF.
      offset = offset + 1.
    ENDDO.
  ENDMETHOD.

  METHOD constructor.
    plain = 'abcdefghijklmnopqrstuvwxyz1234567890'.
    cipher = 'zyxwvutsrqponmlkjihgfedcba123456790'.
    group_size = 5.
  ENDMETHOD.
ENDCLASS.