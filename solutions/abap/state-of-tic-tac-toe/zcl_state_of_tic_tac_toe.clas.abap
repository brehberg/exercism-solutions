CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES player_type TYPE c LENGTH 1.
    "! E.g., ( ( `XOO` ) ( ` X ` ) ( `  X` ) )
    TYPES board_type TYPE TABLE OF string INITIAL SIZE 3.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE `Ongoing game`,
                 draw         TYPE string VALUE `Draw`,
                 win          TYPE string VALUE `Win`,
               END OF state_enum.

    CLASS-METHODS class_constructor.

    "! @parameter state | Possible values are enumerated in state_enum
    "! @raising cx_parameter_invalid | Board is invalid
    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES integers TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    TYPES integers_table TYPE STANDARD TABLE OF integers WITH EMPTY KEY.
    CLASS-DATA magic_square TYPE integers_table.

    TYPES: BEGIN OF board_state,
             one  TYPE integers,
             two  TYPE integers,
             none TYPE integers,
           END OF board_state.

    METHODS convert
      IMPORTING board         TYPE board_type
      RETURNING VALUE(result) TYPE board_state.
    METHODS winner
      IMPORTING magic_values TYPE integers
      RETURNING VALUE(won)   TYPE abap_bool.
    METHODS combinations
      IMPORTING n             TYPE i
                values        TYPE integers
      RETURNING VALUE(result) TYPE integers_table.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    " The given board string will be converted into magic square values.
    " For example:
    "  ( `XOO` ) ( `X  ` ) ( `X  ` )
    " becomes
    "  one = [2, 9, 4], two = [7, 6], none = [5, 1, 3, 8]
    DATA(board_state) = convert( board ).
    DATA(winner_one) = winner( board_state-one ).
    DATA(winner_two) = winner( board_state-two ).

    IF lines( board_state-one ) < lines( board_state-two ).
      " Wrong turn order: O started
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ELSEIF lines( board_state-one ) > lines( board_state-two ) + 1.
      " Wrong turn order: X went twice
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ELSEIF winner_one = abap_true AND winner_two = abap_true.
      " Impossible board: game should have ended after the game was won
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    state = COND #(
      WHEN winner_one = abap_true OR winner_two = abap_true
      THEN state_enum-win
      WHEN lines( board_state-none ) = 0
      THEN state_enum-draw
      ELSE state_enum-ongoing_game ).
  ENDMETHOD.

  METHOD convert.
    " Determine the "magic square" values for each player's moves
    LOOP AT board INTO DATA(board_row).
      DATA(row) = sy-tabix.

      DO strlen( board_row ) TIMES.
        DATA(col) = sy-index - 1.
        DATA(value) = board_row+col(1).

        FIELD-SYMBOLS <target> TYPE integers.
        ASSIGN COMPONENT COND i(
          WHEN value = player_enum-one THEN 1
          WHEN value = player_enum-two THEN 2
          ELSE 3 ) OF STRUCTURE result TO <target>.
        APPEND magic_square[ row ][ col + 1 ] TO <target>.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.

  METHOD winner.
    " Check if any combination of three moves adds up to 15
    CHECK lines( magic_values ) >= 3.
    LOOP AT combinations( n = 3 values = magic_values ) INTO DATA(combo).
      CHECK 15 = REDUCE i( INIT sum = 0 FOR n IN combo NEXT sum = sum + n ).
      won = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD combinations.
    " Generate all possible sublists with n elements from given values list
    CHECK n <= lines( values ).
    IF n = 1.
      result = VALUE #( FOR value IN values ( VALUE #( ( value ) ) ) ).
      RETURN.
    ENDIF.

    DATA(rest) = VALUE integers( ( LINES OF values FROM 2 ) ).

    result = REDUCE #(
      INIT list = VALUE #( )
      FOR combo IN combinations( n = n - 1 values = rest )
      NEXT list = VALUE #( BASE list
        ( VALUE #( ( values[ 1 ] ) ( LINES OF combo ) ) ) ) ).

    result = VALUE #( BASE result
      ( LINES OF combinations( n = n values = rest ) ) ).
  ENDMETHOD.

  METHOD class_constructor.
    magic_square = VALUE #(
      ( VALUE #( ( 2 ) ( 7 ) ( 6 ) ) )
      ( VALUE #( ( 9 ) ( 5 ) ( 1 ) ) )
      ( VALUE #( ( 4 ) ( 3 ) ( 8 ) ) )
    ).
  ENDMETHOD.
ENDCLASS.
