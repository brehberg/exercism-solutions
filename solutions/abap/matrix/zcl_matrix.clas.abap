CLASS zcl_matrix DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES integertab TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    METHODS matrix_row
      IMPORTING
        string        TYPE string
        index         TYPE i
      RETURNING
        VALUE(result) TYPE integertab.
    METHODS matrix_column
      IMPORTING
        string        TYPE string
        index         TYPE i
      RETURNING
        VALUE(result) TYPE integertab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES matrixtab TYPE STANDARD TABLE OF integertab WITH EMPTY KEY.
    METHODS parse_matrix
      IMPORTING input         TYPE string
      RETURNING VALUE(matrix) TYPE matrixtab.
ENDCLASS.



CLASS zcl_matrix IMPLEMENTATION.

  METHOD matrix_row.
    DATA(matrix) = parse_matrix( string ).
    result = VALUE #( matrix[ index ] OPTIONAL ).
  ENDMETHOD.

  METHOD matrix_column.
    DATA(matrix) = parse_matrix( string ).
    result = VALUE #( FOR row IN matrix ( row[ index ] ) ).
  ENDMETHOD.

  METHOD parse_matrix.
    SPLIT input AT '\n' INTO TABLE DATA(rows).
    LOOP AT rows INTO DATA(row).
      SPLIT row AT space INTO TABLE DATA(values).
      APPEND CONV #( values ) TO matrix.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.