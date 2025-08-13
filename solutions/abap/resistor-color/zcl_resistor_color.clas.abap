CLASS zcl_resistor_color DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS resistor_color
      IMPORTING
        color_code   TYPE string
      RETURNING
        VALUE(value) TYPE i.
  PRIVATE SECTION.
    DATA color_values TYPE STANDARD TABLE OF string WITH KEY table_line.
ENDCLASS.

CLASS zcl_resistor_color IMPLEMENTATION.

  METHOD resistor_color.
    DATA input_color TYPE string.
    input_color = to_lower( color_code ).
    READ TABLE color_values TRANSPORTING NO FIELDS WITH KEY table_line = input_color.
    value = sy-tabix - 1.
  ENDMETHOD.

  METHOD constructor.
    APPEND 'black' TO color_Values.
    APPEND 'brown' TO color_Values.
    APPEND 'red' TO color_Values.
    APPEND 'orange' TO color_Values.
    APPEND 'yellow' TO color_Values.
    APPEND 'green' TO color_Values.
    APPEND 'blue' TO color_Values.
    APPEND 'violet' TO color_Values.
    APPEND 'grey' TO color_Values.
    APPEND 'white' TO color_Values.
  ENDMETHOD.
  
ENDCLASS.