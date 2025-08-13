CLASS zcl_acronym DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS parse IMPORTING phrase         TYPE string
                  RETURNING VALUE(acronym) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_acronym IMPLEMENTATION.

  METHOD parse.
    DATA(clean) = to_upper( phrase ).
    REPLACE ALL OCCURRENCES OF '_' IN clean WITH ``.
    REPLACE ALL OCCURRENCES OF '-' IN clean WITH ` `.
    CONDENSE clean.

    SPLIT clean AT space INTO TABLE DATA(words).
    acronym = REDUCE #(
      INIT str = ``
      FOR word IN words
      NEXT str = str && word(1) ).
  ENDMETHOD.

ENDCLASS.