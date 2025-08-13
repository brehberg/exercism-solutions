CLASS zcl_triangle DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      is_equilateral
        IMPORTING
          side_a        TYPE f
          side_b        TYPE f
          side_c        TYPE f
        RETURNING
          VALUE(result) TYPE abap_bool
        RAISING
          cx_parameter_invalid,
      is_isosceles
        IMPORTING
          side_a        TYPE f
          side_b        TYPE f
          side_c        TYPE f
        RETURNING
          VALUE(result) TYPE abap_bool
        RAISING
          cx_parameter_invalid,
      is_scalene
        IMPORTING
          side_a        TYPE f
          side_b        TYPE f
          side_c        TYPE f
        RETURNING
          VALUE(result) TYPE abap_bool
        RAISING
          cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS
      is_valid
        IMPORTING
          side_a TYPE f
          side_b TYPE f
          side_c TYPE f
        RAISING
          cx_parameter_invalid.
ENDCLASS.


CLASS zcl_triangle IMPLEMENTATION.

  METHOD is_equilateral.
    is_valid( side_a = side_a side_b = side_b side_c = side_c ).
    result = boolc( side_a = side_b AND side_b = side_c ).
  ENDMETHOD.

  METHOD is_isosceles.
    is_valid( side_a = side_a side_b = side_b side_c = side_c ).
    result = boolc( side_a = side_b OR
                    side_b = side_c OR
                    side_a = side_c ).
  ENDMETHOD.

  METHOD is_scalene.
    is_valid( side_a = side_a side_b = side_b side_c = side_c ).
    result = boolc( NOT is_isosceles(
        side_a = side_a
        side_b = side_b
        side_c = side_c ) ).
  ENDMETHOD.

  METHOD is_valid.
    " all side lengths must be positive
    IF side_a <= 0 OR side_b <= 0 OR side_c <= 0.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " side lengths cannot violate triangle inequality
    IF side_a + side_b < side_c OR
       side_b + side_c < side_a OR
       side_a + side_c < side_b .
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.