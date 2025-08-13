CLASS zcl_collatz_conjecture DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS ret_steps IMPORTING num          TYPE i
                      RETURNING VALUE(steps) TYPE i
                      RAISING   cx_parameter_invalid.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_collatz_conjecture IMPLEMENTATION.

  METHOD ret_steps.
    IF num < 1.
      " invalid starting value, must be a positive integer
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          parameter = 'NUM'.
    ENDIF.

    DATA(n) = num.
    WHILE n > 1.
      IF n MOD 2 = 0.
        n = n / 2.
      ELSE.
        n = 3 * n + 1.
      ENDIF.
      steps += 1.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.