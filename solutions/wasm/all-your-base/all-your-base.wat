(module
  (memory (export "mem") 1)

  (global $outputEnd i32 (i32.const 4032)) ;; right most position for output buffer

  ;; Status codes returned as res[2]
  (global $ok i32 (i32.const 0))
  (global $inputHasWrongFormat i32 (i32.const -1))
  (global $wrongInputBase i32 (i32.const -2))
  (global $wrongOutputBase i32 (i32.const -3))

  ;; Returns offset and length of resulting u32[] and a return status code
  (func (export "convert") (param $arrOffset i32) (param $arrLength i32) 
    (param $inputBase i32) (param $outputBase i32) (result i32 i32 i32)

    ;; check input base and output base are both >= 2 and
    ;; that all digits in the input sequence are valid
    (local $rc i32)
    (local.tee $rc (call $validateArgs (local.get $arrOffset) 
      (local.get $arrLength) (local.get $inputBase) (local.get $outputBase)))
    (if (i32.ne (global.get $ok))
      (return (global.get $outputEnd) (i32.const 0) (local.get $rc)))

    (call $inToDec
      (i32.const 0) ;; initialize value accumulator for integer result
      (local.get $arrOffset) 
      (local.get $inputBase)
      (i32.sub (local.get $arrLength) (i32.const 1))
    )
    (call $decToOut (local.get $outputBase) (global.get $outputEnd) (i32.const 1))
    (return (global.get $ok))
  )
  ;; convert sequence of digits in input base to whole integer value
  (func $inToDec (param $value i32) (param $offset i32)
      (param $base i32) (param $pos i32) (result i32)
    (if (i32.lt_s (local.get $pos) (i32.const 0)) (return (local.get $value)))

    ;; calculate the integer value of the current digit: digit * base ^ pos
    (call $getValue (local.get $offset) (local.get $base) (local.get $pos))

    ;; add value of digit to result accumulator and call for next position
    (call $inToDec
      (i32.add (local.get $value))
      (i32.add (local.get $offset) (i32.const 4))
      (local.get $base)
      (i32.sub (local.get $pos) (i32.const 1))      
    )
  )
  ;; convert whole integer value to sequence of digits in output base
  (func $decToOut (param $value i32) (param $base i32)
      (param $arrPos i32) (param $arrLen i32) (result i32 i32)

    ;; store the right most digit of the output sequence
    (i32.store (local.get $arrPos) (i32.rem_u (local.get $value) (local.get $base)))
    (if (i32.lt_u (local.get $value) (local.get $base))
      (return (local.get $arrPos) (local.get $arrLen)))

    ;; reduce integer value and call for next position to the left
    (call $decToOut
      (i32.div_u (local.get $value) (local.get $base))
      (local.get $base)
      (i32.sub (local.get $arrPos) (i32.const 4))
      (i32.add (local.get $arrLen) (i32.const 1))
    )
  )
  ;; read next digit in memory buffer and determine its positional value
  (func $getValue (param $offset i32) (param $base i32) (param $pos i32) (result i32)
    (call $exp (local.get $base) (local.get $pos))
    (i32.mul (i32.load (local.get $offset)))  
  )
  ;; return error code if any arguments are invalid, otherwise return ok code
  (func $validateArgs (param $offset i32) (param $length i32) 
    (param $inputBase i32) (param $outputBase i32) (result i32)

    (if (i32.lt_s (local.get $inputBase) (i32.const 2)) (return (global.get $wrongInputBase)))
    (if (i32.lt_s (local.get $outputBase) (i32.const 2)) (return (global.get $wrongOutputBase)))
    (if (i32.eqz (local.get $length)) (return (global.get $inputHasWrongFormat)))
    (if (i32.and  ;; length must be 1 or no leading zero is allowed
      (i32.ne (local.get $length) (i32.const 1))
      (i32.eqz (i32.load (local.get $offset))))
      (return (global.get $inputHasWrongFormat)))
    (if (call $invalidDigit (local.get $offset) (local.get $length) (local.get $inputBase))
      (return (global.get $inputHasWrongFormat)))
    (return (global.get $ok))    
  )
  ;; return 1 if any invalid digit is found, otherwise 0
  (func $invalidDigit (param $offset i32) (param $length i32) (param $base i32) (result i32)
    (local $pos i32) (local $end i32) (local $digit i32)

    (local.tee $pos (local.get $offset))
    (local.set $end (i32.add (i32.mul (local.get $length)) (i32.const 4)))        
    (loop $check
      (local.set $digit (i32.load (local.get $pos)))
      (if (i32.or
        (i32.lt_s (local.get $digit) (i32.const 0))      ;; digit must be non-negative and
        (i32.ge_s (local.get $digit) (local.get $base))) ;; must be less than input base
        (return (i32.const 1)))
      (local.tee $pos (i32.add (local.get $pos) (i32.const 4)))
      (br_if $check (i32.lt_u (local.get $end)))
    )
    (i32.const 0)
  )
  ;; returns value of integer num raised to the given power
  (func $exp (param $num i32) (param $power i32) (result i32)
    (if (i32.eqz (local.get $power)) (return (i32.const 1)))
    (call $exp (local.get $num)
      (i32.sub (local.get $power) (i32.const 1)))
    (i32.mul (local.get $num))
  )
)
