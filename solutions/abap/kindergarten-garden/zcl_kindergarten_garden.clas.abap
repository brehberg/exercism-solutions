CLASS zcl_kindergarten_garden DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
    METHODS plants
      IMPORTING
        diagram        TYPE string
        student        TYPE string
      RETURNING
        VALUE(results) TYPE string_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA students TYPE string_table.

    TYPES: BEGIN OF student_plants,
             name   TYPE string,
             plants TYPE string_table,
           END OF student_plants.
    TYPES student_plants_tab TYPE SORTED TABLE OF student_plants WITH UNIQUE KEY name.

    METHODS group_plants_by_students
      IMPORTING diagram       TYPE string
      RETURNING VALUE(result) TYPE student_plants_tab.
    METHODS decode_plant
      IMPORTING !code       TYPE string
      RETURNING VALUE(name) TYPE string.

ENDCLASS.


CLASS zcl_kindergarten_garden IMPLEMENTATION.

  METHOD plants.
    DATA(garden) = group_plants_by_students( diagram ).
    DATA(plants) = VALUE #( garden[ name = student ]-plants OPTIONAL ).
    results = VALUE #( FOR code IN plants ( decode_plant( code ) ) ).
  ENDMETHOD.

  METHOD group_plants_by_students.
    SPLIT diagram AT `\n` INTO DATA(row1) DATA(row2).
    DATA(max) = nmin( val1 = strlen( row1 ) val2 = strlen( row2 ) ) DIV 2.

    result = VALUE #(
      FOR n = 0 WHILE n < max
      LET pos = n * 2 pos2 = pos + 1 IN
      ( name = VALUE #( students[ n + 1 ] OPTIONAL )
        plants = VALUE #(
          ( row1+pos(1) ) ( row1+pos2(1) )
          ( row2+pos(1) ) ( row2+pos2(1) )
      ) ) ).
  ENDMETHOD.

  METHOD decode_plant.
    name = SWITCH #( code
      WHEN 'G' THEN `grass`
      WHEN 'C' THEN `clover`
      WHEN 'R' THEN `radishes`
      WHEN 'V' THEN `violets` ).
  ENDMETHOD.

  METHOD constructor.
    students = VALUE #(
      ( `Alice` ) ( `Bob` ) ( `Charlie` ) ( `David` )
      ( `Eve` ) ( `Fred` ) ( `Ginny` ) ( `Harriet` )
      ( `Ileana` ) ( `Joseph` ) ( `Kincaid` ) ( `Larry`) ).
  ENDMETHOD.

ENDCLASS.