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
    TYPES integers TYPE STANDARD TABLE OF i
        WITH EMPTY KEY INITIAL SIZE 3.
    TYPES integers_table TYPE STANDARD TABLE OF integers
        WITH EMPTY KEY INITIAL SIZE 3.
    CLASS-DATA possible_rows TYPE integers_table.

    TYPES player_moves TYPE STANDARD TABLE OF player_type
        WITH EMPTY KEY INITIAL SIZE 9.
    TYPES: BEGIN OF board_state,
             player_one_count TYPE i,
             player_two_count TYPE i,
             moves            TYPE player_moves,
           END OF board_state.

    METHODS convert
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE board_state.
    METHODS winner
      IMPORTING player     TYPE player_type
                moves      TYPE player_moves
      RETURNING VALUE(won) TYPE abap_bool.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    " The given board string will be converted into single list of player moves
    " For example:
    "  ( `XOO` ) ( `X  ` ) ( `X  ` )
    " becomes
    "  player_one_count = 3, player_two_count = 2, moves = [X,O,O,X,_,_,X,_,_]
    DATA(board_state) = convert( board ).
    DATA(winner_one) = winner( moves = board_state-moves player = player_enum-one ).
    DATA(winner_two) = winner( moves = board_state-moves player = player_enum-two ).

    IF board_state-player_one_count < board_state-player_two_count.
      " Wrong turn order: Player two started
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ELSEIF board_state-player_one_count > board_state-player_two_count + 1.
      " Wrong turn order: Player one went twice
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ELSEIF winner_one = abap_true AND winner_two = abap_true.
      " Impossible board: game should have ended after the game was won
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    state = COND #(
      WHEN winner_one = abap_true OR winner_two = abap_true
      THEN state_enum-win
      WHEN board_state-player_one_count + board_state-player_two_count = 9
      THEN state_enum-draw
      ELSE state_enum-ongoing_game ).
  ENDMETHOD.

  METHOD convert.
    " Populate the board state for each player's moves
    LOOP AT board INTO DATA(board_row).
      DATA(offset) = 0.
      DO strlen( board_row ) TIMES.
        DATA(value) = board_row+offset(1).
        APPEND value TO state-moves.

        IF value = player_enum-one.
          state-player_one_count += 1.
        ELSEIF value = player_enum-two.
          state-player_two_count += 1.
        ENDIF.

        offset += 1.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.

  METHOD winner.
    CHECK lines( moves ) = 9.
    " Check if any wining combination of three moves
    LOOP AT possible_rows INTO DATA(row).
      DATA(i) = row[ 1 ].
      DATA(j) = row[ 2 ].
      DATA(k) = row[ 3 ].
      CHECK moves[ i ] = moves[ j ]
        AND moves[ j ] = moves[ k ]
        AND moves[ k ] = player.
      won = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD class_constructor.
    possible_rows = VALUE #(
      " horizontal
      ( VALUE #( ( 1 ) ( 2 ) ( 3 ) ) )
      ( VALUE #( ( 4 ) ( 5 ) ( 6 ) ) )
      ( VALUE #( ( 7 ) ( 8 ) ( 9 ) ) )
      " vertical
      ( VALUE #( ( 1 ) ( 4 ) ( 7 ) ) )
      ( VALUE #( ( 2 ) ( 5 ) ( 8 ) ) )
      ( VALUE #( ( 3 ) ( 6 ) ( 9 ) ) )
      " diagonal
      ( VALUE #( ( 1 ) ( 5 ) ( 9 ) ) )
      ( VALUE #( ( 3 ) ( 5 ) ( 7 ) ) )
    ).
  ENDMETHOD.
ENDCLASS.
