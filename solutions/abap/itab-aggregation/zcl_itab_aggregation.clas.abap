CLASS zcl_itab_aggregation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES group TYPE c LENGTH 1.
    TYPES: BEGIN OF initial_numbers_type,
             group  TYPE group,
             number TYPE i,
           END OF initial_numbers_type,
           initial_numbers TYPE STANDARD TABLE OF initial_numbers_type WITH EMPTY KEY.

    TYPES: BEGIN OF aggregated_data_type,
             group   TYPE group,
             count   TYPE i,
             sum     TYPE i,
             min     TYPE i,
             max     TYPE i,
             average TYPE f,
           END OF aggregated_data_type,
           aggregated_data TYPE STANDARD TABLE OF aggregated_data_type WITH EMPTY KEY.

    METHODS perform_aggregation
      IMPORTING
        initial_numbers        TYPE initial_numbers
      RETURNING
        VALUE(aggregated_data) TYPE aggregated_data.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_itab_aggregation IMPLEMENTATION.

  METHOD perform_aggregation.

    LOOP AT initial_numbers REFERENCE INTO DATA(record)
      GROUP BY ( group = record->group
                 size = GROUP SIZE
                ) INTO DATA(key).

      DATA(aggregate) = REDUCE aggregated_data_type(
        INIT result = VALUE #(
            group = key-group
            count = key-size
            min = cl_abap_math=>max_int4
            max = cl_abap_math=>min_int4 )
        FOR member IN GROUP key
        NEXT result = VALUE #( BASE result
          sum = result-sum + member-number
          min = nmin( val1 = result-min val2 = member-number )
          max = nmax( val1 = result-max val2 = member-number )
        ) ).

      APPEND VALUE #( BASE aggregate
        average = aggregate-sum / aggregate-count
      ) TO aggregated_data.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
