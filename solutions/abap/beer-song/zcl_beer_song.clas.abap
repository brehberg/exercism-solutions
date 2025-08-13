CLASS zcl_beer_song DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS recite
      IMPORTING
        !initial_bottles_count TYPE i
        !take_down_count       TYPE i
      RETURNING
        VALUE(result)          TYPE string_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS verse
      IMPORTING number        TYPE i
      RETURNING VALUE(result) TYPE string_table.
    METHODS verse_last
      RETURNING VALUE(result) TYPE string_table.
    METHODS verse_template
      IMPORTING bottle_count1 TYPE string
                bottle_count2 TYPE string
                one_word      TYPE string DEFAULT `one`
      RETURNING VALUE(result) TYPE string_table.
ENDCLASS.



CLASS zcl_beer_song IMPLEMENTATION.

  METHOD recite.
    result = VALUE #(
      BASE VALUE #( ( LINES OF verse( initial_bottles_count ) ) )
      FOR n = 1 WHILE n < take_down_count
      ( ) ( LINES OF verse( initial_bottles_count - n ) ) ).
  ENDMETHOD.

  METHOD verse.
    result = SWITCH #( number
      WHEN 0 THEN verse_last( )
      WHEN 1 THEN verse_template(
        bottle_count1 = `1 bottle`
        bottle_count2 = `no more bottles`
        one_word = `it` )
      WHEN 2 THEN verse_template(
        bottle_count1 = `2 bottles`
        bottle_count2 = `1 bottle` )
      ELSE verse_template(
        bottle_count1 = |{ number } bottles|
        bottle_count2 = |{ number - 1 } bottles| ) ).
  ENDMETHOD.

  METHOD verse_template.
    result = VALUE #(
     ( |{ bottle_count1 } of beer on the wall, | &
       |{ bottle_count1 } of beer.| )
     ( |Take { one_word } down and pass it around, | &
       |{ bottle_count2 } of beer on the wall.| )
    ).
  ENDMETHOD.

  METHOD verse_last.
    result = VALUE #(
     ( `No more bottles of beer on the wall, ` &
       `no more bottles of beer.` )
     ( `Go to the store and buy some more, ` &
       `99 bottles of beer on the wall.` )
    ).
  ENDMETHOD.

ENDCLASS.