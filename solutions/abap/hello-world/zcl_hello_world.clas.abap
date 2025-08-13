CLASS zcl_hello_world DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS hello RETURNING VALUE(out) TYPE string.
ENDCLASS.

CLASS zcl_hello_world IMPLEMENTATION.

  METHOD hello.
    out = 'Hello, World!'.
  ENDMETHOD.

ENDCLASS.