CLASS zcl_scrabble_score DEFINITION PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
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
    CLASS-DATA:
      points TYPE HASHED TABLE OF point_value WITH UNIQUE KEY letter.

ENDCLASS.


CLASS zcl_scrabble_score IMPLEMENTATION.

  METHOD score.
    DATA(word) = to_upper( input ).
    DATA(offset) = 0.
    DO strlen( word ) TIMES.
      result += VALUE #( points[ letter = word+offset(1) ]-value OPTIONAL ).
      offset += 1.
    ENDDO.
  ENDMETHOD.

  METHOD class_constructor.
    points = VALUE #(
      value = 1
      ( letter = 'A')
      ( letter = 'E')
      ( letter = 'I')
      ( letter = 'O')
      ( letter = 'U')
      ( letter = 'L')
      ( letter = 'N')
      ( letter = 'R')
      ( letter = 'S')
      ( letter = 'T')
      value = 2
      ( letter = 'D')
      ( letter = 'G')
      value = 3
      ( letter = 'B')
      ( letter = 'C')
      ( letter = 'M')
      ( letter = 'P')
      value = 4
      ( letter = 'F')
      ( letter = 'H')
      ( letter = 'V')
      ( letter = 'W')
      ( letter = 'Y')
      value = 5
      ( letter = 'K')
      value = 8
      ( letter = 'J')
      ( letter = 'X')
      value = 10
      ( letter = 'Q')
      ( letter = 'Z')
      ).
  ENDMETHOD.

ENDCLASS.