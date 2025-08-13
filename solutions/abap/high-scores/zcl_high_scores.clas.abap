CLASS zcl_high_scores DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES integertab TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    METHODS constructor
      IMPORTING
        scores TYPE integertab.

    METHODS list_scores
      RETURNING
        VALUE(result) TYPE integertab.

    METHODS latest
      RETURNING
        VALUE(result) TYPE i.

    METHODS personalbest
      RETURNING
        VALUE(result) TYPE i.

    METHODS personaltopthree
      RETURNING
        VALUE(result) TYPE integertab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA scores_list TYPE integertab.

ENDCLASS.


CLASS zcl_high_scores IMPLEMENTATION.

  METHOD constructor.
    me->scores_list = scores.
  ENDMETHOD.

  METHOD list_scores.
    result = scores_list.
  ENDMETHOD.

  METHOD latest.
    CHECK scores_list IS NOT INITIAL.
    result = scores_list[ lines( scores_list ) ].
  ENDMETHOD.

  METHOD personalbest.
    DATA(sorted_scores) = scores_list.
    SORT sorted_scores BY table_line DESCENDING.
    result = VALUE #( sorted_scores[ 1 ] OPTIONAL ).
  ENDMETHOD.

  METHOD personaltopthree.
    DATA(sorted_scores) = scores_list.
    SORT sorted_scores BY table_line DESCENDING.
    result = VALUE #( FOR score IN sorted_scores TO 3 ( score ) ).
  ENDMETHOD.

ENDCLASS.