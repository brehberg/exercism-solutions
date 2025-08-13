CLASS zcl_anagram DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS anagram
      IMPORTING
        input         TYPE string
        candidates    TYPE string_table
      RETURNING
        VALUE(result) TYPE string_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES sorted_chars TYPE SORTED TABLE OF c WITH NON-UNIQUE KEY table_line.
    DATA: base_word  TYPE string,
          base_chars TYPE sorted_chars.
    METHODS is_anagram
      IMPORTING candidate   TYPE string
      RETURNING VALUE(flag) TYPE abap_bool.
    METHODS determine_chars
      IMPORTING word         TYPE string
      RETURNING VALUE(chars) TYPE sorted_chars.

ENDCLASS.


CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    base_word = to_upper( input ).
    base_chars = determine_chars( base_word ).

    LOOP AT candidates INTO DATA(word).
      CHECK is_anagram( to_upper( word ) ).
      APPEND word TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_anagram.
    flag = boolc( base_word <> candidate
              AND base_chars = determine_chars( candidate ) ).
  ENDMETHOD.

  METHOD determine_chars.
    chars = VALUE #(
      FOR i = 0 UNTIL i = strlen( word )
      ( CONV #( word+i(1) ) ) ).
  ENDMETHOD.

ENDCLASS.