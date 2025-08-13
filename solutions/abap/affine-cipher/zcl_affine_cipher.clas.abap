CLASS zcl_affine_cipher DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF key,
             a TYPE i,
             b TYPE i,
           END OF key.

    CLASS-METHODS class_constructor.

    METHODS:
      encode IMPORTING phrase        TYPE string
                       key           TYPE key
             RETURNING VALUE(cipher) TYPE string
             RAISING   cx_parameter_invalid,
      decode IMPORTING cipher        TYPE string
                       key           TYPE key
             RETURNING VALUE(phrase) TYPE string
             RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA letters TYPE string.
    CLASS-DATA numbers TYPE string.
    CLASS-DATA length_m TYPE i.
    CLASS-DATA group_size TYPE i.
    METHODS
      normalize
        IMPORTING input         TYPE string
        RETURNING VALUE(result) TYPE string.
    METHODS
      greatest_common_divisor
        IMPORTING n          TYPE i
                  d          TYPE i
        RETURNING VALUE(gcd) TYPE i.
    METHODS
      modular_multiplicative_inverse
        IMPORTING input_a    TYPE i
        RETURNING VALUE(mmi) TYPE i.

ENDCLASS.



CLASS zcl_affine_cipher IMPLEMENTATION.

  METHOD encode.
    " Values a and m must be coprime (their only common factor is 1)
    IF greatest_common_divisor( n = key-a d = length_m ) <> 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    DATA(plain) = normalize( phrase ).
    DATA(n) = 0.
    DO strlen( plain ) TIMES.
      " Numeric digits are valid input but they are not encrypted.
      IF plain+n(1) CO numbers.
        cipher = cipher && plain+n(1).
        n = n + 1.
        CONTINUE.
      ENDIF.

      " The encryption function is: E(x) = (ai + b) mod m
      "  i is the letter's index from 0 to the length of the alphabet - 1
      "  m is the length of the alphabet. For the Roman alphabet m is 26.
      "  a and b are integers which make the encryption key
      DATA(index) = find( val = letters sub = plain+n(1) ).
      DATA(x) = ( key-a * index + key-b ) MOD length_m.

      cipher = cipher && letters+x(1).
      n = n + 1.
    ENDDO.

    " CipherText is written out in groups of fixed length, the traditional
    " group size being 5 letters, and punctuation is excluded. This is to
    " make it harder to guess things based on word boundaries.
    DATA(offset) = group_size.
    DATA(splits) = ( strlen( cipher ) - 1 ) DIV group_size.
    DO splits TIMES.
      cipher = |{ cipher(offset) } { cipher+offset }|.
      offset = offset + group_size + 1.
    ENDDO.
  ENDMETHOD.

  METHOD decode.
    " Values a and m must be coprime (their only common factor is 1)
    IF greatest_common_divisor( n = key-a d = length_m ) <> 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    DATA(input) = condense( val = cipher to = `` ).
    DATA(n) = 0.
    DO strlen( input ) TIMES.
      " Numeric digits are valid input but they are not encrypted.
      IF input+n(1) CO numbers.
        phrase = phrase && input+n(1).
        n = n + 1.
        CONTINUE.
      ENDIF.

      " The decryption function is: D(y) = (a^-1)(y - b) mod m
      "  y is the numeric value of an encrypted letter, i.e., y = E(x)
      "  a^-1 is the modular multiplicative inverse (MMI) of a mod m
      DATA(y) = find( val = letters sub = input+n(1) ).
      DATA(mmi) = modular_multiplicative_inverse( key-a ).
      DATA(index) = mmi * ( y - key-b ) MOD length_m.

      phrase = phrase && letters+index(1).
      n = n + 1.
    ENDDO.
  ENDMETHOD.

  METHOD modular_multiplicative_inverse.
    " Determine the modulo inverse of a with respect to m using extended
    " Euclid Algorithm assuming a and m are coprimes, i.e. gcd(a, m) = 1
    CHECK length_m <> 1.

    DATA(a) = input_a.
    DATA(m) = length_m.
    DATA(x) = 1.
    DATA(y) = 0.

    WHILE a > 1.
      " q is the quotient
      DATA(q) = a div m.
      DATA(t) = m.

      " m is remainder now, process as Euclid's algo
      m = a mod m.
      a = t.
      t = y.

      " update y and x
      y = x - q * y.
      x = t.
    ENDWHILE.

    " make x positive if necessary
    mmi = COND #( WHEN x < 0 THEN x + length_m ELSE x ).
  ENDMETHOD.

  METHOD greatest_common_divisor.
    CHECK n > 0 AND d > 0.
    DATA(a) = n.
    DATA(b) = d.
    DATA(c) = 0.

    WHILE b > 0.
      c = a - ( a DIV b ) * b.
      a = b.
      b = c.
    ENDWHILE.

    gcd = a.
  ENDMETHOD.

  METHOD normalize.
    result = to_lower( input ).
    REPLACE ALL OCCURRENCES OF REGEX '\W' IN result WITH ''.
    CONDENSE result NO-GAPS.
  ENDMETHOD.

  METHOD class_constructor.
    numbers = '0123456789'.
    letters = 'abcdefghijklmnopqrstuvwxyz'.
    length_m = strlen( letters ).
    group_size = 5.
  ENDMETHOD.

ENDCLASS.