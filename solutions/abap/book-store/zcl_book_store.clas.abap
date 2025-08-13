CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! ID of book to buy from 1 to 5
    TYPES book_id TYPE i.

    TYPES basket_type TYPE SORTED TABLE OF book_id
      WITH NON-UNIQUE KEY table_line.

    TYPES total TYPE p LENGTH 3 DECIMALS 2.

    "! @parameter basket | E.g., buying two copies of the first book
    "!                           and one copy of the second book
    "!                           is equivalent to ( ( 1 ) ( 1 ) ( 2 ) )
    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

    CLASS-METHODS class_constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF price_per_book_bundle,
             size  TYPE i,
             price TYPE total,
           END OF price_per_book_bundle.
    CLASS-DATA bundle_costs TYPE SORTED TABLE OF price_per_book_bundle
      WITH UNIQUE KEY size INITIAL SIZE 5.

    TYPES: BEGIN OF book_bundle,
             size  TYPE i,
             books TYPE basket_type,
           END OF book_bundle.
    TYPES book_bundles TYPE STANDARD TABLE OF book_bundle WITH EMPTY KEY.

    METHODS add_book_to_bundle
      IMPORTING book_id      TYPE book_id
      CHANGING  book_bundles TYPE book_bundles.

    TYPES: BEGIN OF bundle_count,
             size  TYPE i,
             count TYPE i,
           END OF bundle_count.
    TYPES bundle_counts TYPE SORTED TABLE OF bundle_count
      WITH UNIQUE KEY size INITIAL SIZE 5.

    METHODS count_books_per_bundle
      IMPORTING book_bundles  TYPE book_bundles
      RETURNING VALUE(result) TYPE bundle_counts.

ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD class_constructor.
    CONSTANTS cost TYPE total VALUE '8.00'.
    DATA discounts TYPE TABLE OF total WITH EMPTY KEY INITIAL SIZE 5.
    discounts = VALUE #(
      ( CONV #( 0 ) )  " one copy of any of the five books costs $8
      ( CONV #( 5 ) )  " buy two different books, get a 5% discount
      ( CONV #( 10 ) ) " buy 3 different books, get a 10% discount
      ( CONV #( 20 ) ) " buy 4 different books, get a 20% discount
      ( CONV #( 25 ) ) " buy all 5, get a 25% discount
     ).

    " compute the total bundle cost for each discount level
    bundle_costs = VALUE #(
      FOR discount IN discounts INDEX INTO books
      ( size = books
        price = CONV total( books * cost * ( 1 - discount / 100 ) ) ) ).
  ENDMETHOD.

  METHOD calculate_total.
    DATA bundles TYPE book_bundles.

    LOOP AT basket INTO DATA(book).
      add_book_to_bundle(
        EXPORTING book_id = book
        CHANGING book_bundles = bundles ).
    ENDLOOP.

    total = REDUCE #(
      INIT sum = VALUE total( )
      FOR row IN count_books_per_bundle( bundles )
      NEXT sum = sum + row-count * bundle_costs[ size = row-size ]-price ).

  ENDMETHOD.

  METHOD add_book_to_bundle.
    " Add book to the first bundle that doesn't already have it
    LOOP AT book_bundles REFERENCE INTO DATA(bundle).
      CHECK NOT line_exists( bundle->books[ table_line = book_id ] ).
      bundle->size += 1.
      INSERT book_id INTO TABLE bundle->books.
      RETURN.
    ENDLOOP.

    " No existing group found for this book, so start a new one
    APPEND VALUE #(
      size = 1
      books = VALUE #( ( book_id ) )
      ) TO book_bundles.
  ENDMETHOD.

  METHOD count_books_per_bundle.
    result = VALUE #( FOR cost IN bundle_costs ( size = cost-size ) ).

    LOOP AT book_bundles INTO DATA(bundle).
      result[ size = bundle-size ]-count += 1.
    ENDLOOP.

    " optimize discount on 4 book bundles for each pair of 3 and 5 bundles
    WHILE result[ size = 3 ]-count > 0 AND result[ size = 5 ]-count > 0.
      result[ size = 4 ]-count += 2.
      result[ size = 3 ]-count -= 1.
      result[ size = 5 ]-count -= 1.
    ENDWHILE.
  ENDMETHOD.
  
ENDCLASS.