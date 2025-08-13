CLASS zcl_resistor_color DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    METHODS resistor_color
      IMPORTING
        color_code   TYPE string
      RETURNING
        VALUE(value) TYPE i.
  PRIVATE SECTION.
    CLASS-DATA color_values TYPE STANDARD TABLE OF string WITH KEY table_line.
ENDCLASS.

CLASS zcl_resistor_color IMPLEMENTATION.

  METHOD resistor_color.
    value = line_index( color_values[ table_line = to_lower( color_code ) ] ) - 1.
  ENDMETHOD.

  METHOD class_constructor.
    " Better Be Right Or Your Great Big Values Go Wrong
    color_values = VALUE #(
      ( `black` ) ( `brown` ) ( `red` ) ( `orange` ) ( `yellow` )
      ( `green` ) ( `blue` ) ( `violet` ) ( `grey` ) ( `white` ) ).
  ENDMETHOD.

ENDCLASS.