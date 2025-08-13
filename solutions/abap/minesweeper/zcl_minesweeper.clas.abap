CLASS zcl_minesweeper DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS annotate
      IMPORTING
        !input        TYPE string_table
      RETURNING
        VALUE(result) TYPE string_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS bomb TYPE string VALUE '*'.
    DATA input_data TYPE string_table.
    METHODS count_bombs
      IMPORTING line         TYPE string
                pos          TYPE i
      RETURNING VALUE(count) TYPE i.
    METHODS check_position
      IMPORTING row          TYPE i
                col          TYPE i
      RETURNING VALUE(value) TYPE string.

ENDCLASS.


CLASS zcl_minesweeper IMPLEMENTATION.

  METHOD annotate.
    input_data = input.

    result = VALUE #(
      FOR line IN input INDEX INTO row
      " build output string for each row by checking every column
      ( REDUCE #( INIT out TYPE string
          FOR col = 0 WHILE col < strlen( line )
          NEXT out = out && COND #(
            WHEN line+col(1) = bomb THEN bomb
            ELSE check_position( row = row col = col ) )
     ) ) ).

  ENDMETHOD.

  METHOD check_position.
    DATA(max) = lines( input_data ).
    DATA(line) = input_data[ row ].

    " extract the lines above and below the current position
    DATA(prev) = COND #( WHEN row > 1 THEN input_data[ row - 1 ] ).
    DATA(next) = COND #( WHEN row < max THEN input_data[ row + 1 ] ).

    " return the total value of bombs found for current position
    DATA(sum) = count_bombs( line = line pos = col )
              + count_bombs( line = prev pos = col )
              + count_bombs( line = next pos = col ).
    value = COND #( WHEN sum = 0 THEN ` ` ELSE |{ sum }| ).
  ENDMETHOD.

  METHOD count_bombs.
    CHECK line IS NOT INITIAL.
    DATA(max) = strlen( line ).
    DATA(mid) = line+pos(1).

    " extract the characters to the left and right of current position
    DATA(prev) = COND #( LET n = pos - 1 IN WHEN n >= 0 THEN line+n(1) ).
    DATA(next) = COND #( LET n = pos + 1 IN WHEN n < max THEN line+n(1) ).

    " return the sum of bombs found on this line for current position
    count = COND #( WHEN prev = bomb THEN 1 )
          + COND #( WHEN mid = bomb THEN 1 )
          + COND #( WHEN next = bomb THEN 1 ).
  ENDMETHOD.

ENDCLASS.