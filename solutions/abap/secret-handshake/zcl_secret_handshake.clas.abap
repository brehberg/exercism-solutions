CLASS zcl_secret_handshake DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_commands
      IMPORTING code            TYPE i
      RETURNING VALUE(commands) TYPE string_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES hex_code TYPE x LENGTH 1.
    DATA my_code TYPE hex_code.
    DATA my_commands TYPE string_table.
    
    METHODS secret
      IMPORTING num    TYPE hex_code
                action TYPE string.
    METHODS reverse_table
      IMPORTING input TYPE string_table
      RETURNING VALUE(out) TYPE string_table.

ENDCLASS.


CLASS zcl_secret_handshake IMPLEMENTATION.

  METHOD get_commands.
    my_code = CONV #( code ).

    " If the following bits are set, include the corresponding action
    " in your list of commands, in order from lowest to highest.
    "    0b00001 = wink
    "    0b00010 = double blink
    "    0b00100 = close your eyes
    "    0b01000 = jump
    "    0b10000 = Reverse the order of the secret handshake
    secret( num = CONV #( '01' ) action = 'wink' ).
    secret( num = CONV #( '02' ) action = 'double blink' ).
    secret( num = CONV #( '04' ) action = 'close your eyes' ).
    secret( num = CONV #( '08' ) action = 'jump' ).
    secret( num = CONV #( '10' ) action = 'reverse' ).

    commands = my_commands.
  ENDMETHOD.

  METHOD secret.
    CHECK my_code BIT-AND num = num.
    IF action = 'reverse'.
       my_commands = reverse_table( my_commands ).
    ELSE.
      APPEND action TO my_commands.
    ENDIF.
  ENDMETHOD.

  METHOD reverse_table.
    LOOP AT input INTO DATA(row).
      INSERT row INTO out INDEX 1.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.