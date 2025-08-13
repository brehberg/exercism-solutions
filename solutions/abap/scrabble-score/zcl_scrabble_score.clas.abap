CLASS zcl_scrabble_score DEFINITION PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
    METHODS score
      IMPORTING
        input         TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: 
      BEGIN OF point_value,
        letter TYPE c LENGTH 1,
        value  TYPE i,
      END OF point_value.
    DATA:
      point_values TYPE SORTED TABLE OF point_value WITH UNIQUE KEY letter.

ENDCLASS.


CLASS zcl_scrabble_score IMPLEMENTATION.
  METHOD score.
    DATA offset TYPE i.
    DATA current TYPE point_value.
    DATA word TYPE string.
    word = to_upper( input ).
    DO strlen( word ) TIMES.
      READ TABLE point_values INTO current WITH KEY letter = word+offset(1) BINARY SEARCH.
      result = result + current-value.
      offset = offset + 1.
    ENDDO.
  ENDMETHOD.

  METHOD constructor.
    DATA record TYPE point_value.
    record-value = 1.
    record-letter = 'A'.
    INSERT record INTO point_values.
    record-letter = 'E'.
    INSERT record INTO point_values.
    record-letter = 'I'.
    INSERT record INTO point_values.
    record-letter = 'O'.
    INSERT record INTO point_values.
    record-letter = 'U'.
    INSERT record INTO point_values.
    record-letter = 'L'.
    INSERT record INTO point_values.
    record-letter = 'N'.
    INSERT record INTO point_values.
    record-letter = 'R'.
    INSERT record INTO point_values.
    record-letter = 'S'.
    INSERT record INTO point_values.
    record-letter = 'T'.
    INSERT record INTO point_values.
    record-value = 2.
    record-letter = 'D'.
    INSERT record INTO point_values.
    record-letter = 'G'.
    INSERT record INTO point_values.
    record-value = 3.
    record-letter = 'B'.
    INSERT record INTO point_values.
    record-letter = 'C'.
    INSERT record INTO point_values.
    record-letter = 'M'.
    INSERT record INTO point_values.
    record-letter = 'P'.
    INSERT record INTO point_values.
    record-value = 4.
    record-letter = 'F'.
    INSERT record INTO point_values.
    record-letter = 'H'.
    INSERT record INTO point_values.
    record-letter = 'V'.
    INSERT record INTO point_values.
    record-letter = 'W'.
    INSERT record INTO point_values.
    record-letter = 'Y'.
    INSERT record INTO point_values.
    record-value = 5.
    record-letter = 'K'.
    INSERT record INTO point_values.
    record-value = 8.
    record-letter = 'J'.
    INSERT record INTO point_values.
    record-letter = 'X'.
    INSERT record INTO point_values.
    record-value = 10.
    record-letter = 'Q'.
    INSERT record INTO point_values.
    record-letter = 'Z'.
    INSERT record INTO point_values.
  ENDMETHOD.

ENDCLASS.