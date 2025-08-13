CLASS zcl_atbash_cipher DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    METHODS decode
      IMPORTING
        cipher_text       TYPE string
      RETURNING
        VALUE(plain_text) TYPE string .
    METHODS encode
      IMPORTING
        plain_text         TYPE string
      RETURNING
        VALUE(cipher_text) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA plain TYPE string.
    CLASS-DATA cipher TYPE string.
    CLASS-DATA group_size TYPE i.
    METHODS replace_text
      IMPORTING
        input_text    TYPE string
        from_text     TYPE string
        new_text      TYPE string
      RETURNING
        VALUE(output) TYPE string .
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

    " CipherText is written out in groups of fixed length, the traditional
    " group size being 5 letters, and punctuation is excluded. This is to
    " make it harder to guess things based on word boundaries.
    DATA(offset) = group_size.
    DATA(splits) = ( strlen( cipher_text ) - 1 ) DIV group_size.
    DO splits TIMES.
      cipher_text = |{ cipher_text(offset) } { cipher_text+offset }|.
      offset = offset + group_size + 1.
    ENDDO.
  ENDMETHOD.

  METHOD replace_text.
    DATA(input) = condense( val = to_lower( input_text ) to = `` ).
    DATA(offset) = 0.
    DO strlen( input ) TIMES.
      DATA(position) = find( val = from_text sub = input+offset(1) ).
      IF position <> -1.
        output = output && new_text+position(1).
      ENDIF.
      offset = offset + 1.
    ENDDO.
  ENDMETHOD.

  METHOD class_constructor.
    plain = 'abcdefghijklmnopqrstuvwxyz1234567890'.
    cipher = 'zyxwvutsrqponmlkjihgfedcba123456790'.
    group_size = 5.
  ENDMETHOD.
ENDCLASS.