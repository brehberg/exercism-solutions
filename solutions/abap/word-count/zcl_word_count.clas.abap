CLASS zcl_word_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF return_structure,
        word  TYPE string,
        count TYPE i,
      END OF return_structure,
      return_table TYPE STANDARD TABLE OF return_structure WITH KEY word.
    METHODS count_words
      IMPORTING
        !phrase       TYPE string
      RETURNING
        VALUE(result) TYPE return_table .

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS clean
      IMPORTING input         TYPE string
      RETURNING VALUE(result) TYPE string.
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    SPLIT clean( phrase ) AT space INTO TABLE DATA(words).

    LOOP AT words INTO DATA(word).
      COLLECT VALUE return_structure(
        word = word
        count = 1 ) INTO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD clean.
    result = to_lower( input ).
    REPLACE ALL OCCURRENCES OF '''' IN result WITH ``.
    REPLACE ALL OCCURRENCES OF '\n' IN result WITH ` `.
    REPLACE ALL OCCURRENCES OF REGEX '[^\w]' IN result WITH ` `.
    result = condense( result ).
  ENDMETHOD.

ENDCLASS.