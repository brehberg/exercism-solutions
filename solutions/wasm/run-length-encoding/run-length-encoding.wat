(module
  (memory (export "mem") 1)
  (global $outputOffset i32 (i32.const 320))  ;; input string uses bytes 64-319 of linear memory

  ;;
  ;; Encode a string using run-length encoding
  ;;
  ;; @param {i32} inputOffset - The offset of the input string in linear memory
  ;; @param {i32} inputLength - The length of the input string in linear memory
  ;;
  ;; @returns {(i32,i32)} - The offset and length of the encoded string in linear memory
  ;;
  (func (export "encode") (param $inputOffset i32) (param $inputLength i32) (result i32 i32)
    (local $write i32) (local $count i32) (local $previous i32)
    (local $i i32) (local $end i32) (local $current i32)

    (if (i32.eqz (local.get $inputLength))
      (then (return (global.get $outputOffset) (i32.const 0))))

    (local.set $write (global.get $outputOffset))
    (local.set $count (i32.const 1))
    (local.set $previous (i32.load8_u (local.get $inputOffset)))

    ;; Loop through the input string and encode each chunk
    (local.set $i (i32.add (local.get $inputOffset) (i32.const 1)))
    (local.set $end (i32.add (local.get $inputOffset) (local.get $inputLength)))
    (loop $encodeInput
      (local.set $current (i32.load8_u (local.get $i)))

      (if (i32.eq (local.get $current) (local.get $previous))
        (then (local.set $count (i32.add (local.get $count) (i32.const 1))))
        (else
          (local.set $write (call $encodeChunk (local.get $write) (local.get $previous) (local.get $count)))          
          (local.set $previous (local.get $current))
          (local.set $count (i32.const 1))
        )
      )
      (local.tee $i (i32.add (local.get $i) (i32.const 1)))
      (br_if $encodeInput (i32.lt_u (local.get $end)))
    )

    ;; Encode the final chunk of characters
    (local.set $write (call $encodeChunk (local.get $write) (local.get $previous) (local.get $count)))
    (return (global.get $outputOffset) (i32.sub (local.get $write) (global.get $outputOffset)))
  )

  ;;
  ;; Decode a string using run-length encoding
  ;;
  ;; @param {i32} inputOffset - The offset of the string in linear memory
  ;; @param {i32} inputLength - The length of the string in linear memory
  ;;
  ;; returns {(i32,i32)} - The offset and length of the decoded string in linear memory
  ;;
  (func (export "decode") (param $inputOffset i32) (param $inputLength i32) (result i32 i32)
    (local $write i32) (local $count i32) (local $char i32)
    (local $i i32) (local $end i32)

    (if (i32.eqz (local.get $inputLength))
      (then (return (global.get $outputOffset) (i32.const 0))))

    (local.set $write (global.get $outputOffset))
    (local.tee $i (local.get $inputOffset))
    (local.set $end (i32.add (local.get $inputLength)))
    (loop $decodeInput
      (local.set $char (i32.load8_u (local.get $i)))

      (if (i32.eqz (call $isDigit (local.get $char)))
        (then (local.set $count (i32.const 1))) ;; default count to 1 if not a digit
        (else (local.set $count (i32.const 0))  ;; otherwise read digits into count
          (loop $readDigit
            (local.tee $count (i32.mul (local.get $count) (i32.const 10)))
            (local.set $count (i32.add (i32.sub (local.get $char) (i32.const 48))))                  
            (local.tee $i (i32.add (local.get $i) (i32.const 1)))
            (local.set $char (i32.load8_u))
            (br_if $readDigit (call $isDigit (local.get $char)))
          )))
      (local.set $write (call $decodeGroup (local.get $write) (local.get $char) (local.get $count)))
      (local.tee $i (i32.add (local.get $i) (i32.const 1)))
      (br_if $decodeInput (i32.lt_u (local.get $end)))
    )
    (return (global.get $outputOffset) (i32.sub (local.get $write) (global.get $outputOffset)))
  )

  ;;
  ;; Encode a chunk of characters using run-length encoding
  ;;
  ;; @param {i32} offset - The write offset for the output string
  ;; @param {i32} char - The character for the current chunk
  ;; @param {i32} count - The count of characters in the chunk
  ;;
  ;; @returns {i32} - The new write offset for the output string
  ;;
  (func $encodeChunk (param $offset i32) (param $char i32) (param $count i32) (result i32)
    (if (i32.gt_u (local.get $count) (i32.const 1)) 
      (then (local.set $offset (call $writeNumber (local.get $offset) (local.get $count)))))
    (i32.store8 (local.get $offset) (local.get $char))
    (return (i32.add (local.get $offset) (i32.const 1)))
  )

  ;;
  ;; Convert integer count into string of digits
  ;;
  ;; @param {i32} offset - The write offset for the output string
  ;; @param {i32} number - The count of characters in the chunk
  ;;
  ;; @returns {i32} - The new write offset for the output string
  ;;
  (func $writeNumber (param $offset i32) (param $number i32) (result i32)
    (local $digits i32) (local $rem i32)
    ;; determine number of digits in the number
    (local.set $digits (i32.const 0))
    (local.set $rem (local.get $number))
    (loop $digitCount
      (local.set $digits (i32.add (local.get $digits) (i32.const 1)))
      (local.tee $rem (i32.div_u (local.get $rem) (i32.const 10)))
      (br_if $digitCount (i32.gt_u (i32.const 0)))
    )
    (local.set $offset (i32.add (local.get $offset) (local.get $digits)))

    (loop $writeDigit
      (local.tee $offset (i32.sub (local.get $offset) (i32.const 1)))
      ;; take advantage of integer division to determine each digit
      ;;   digit = rem - 10 * (rem / 10)
      (i32.sub (local.get $number) (i32.mul (i32.const 10)
        (i32.div_u (local.get $number) (i32.const 10))))
      ;; store ASCII code for this digit in memory buffer
      (i32.store8 (i32.add (i32.const 48)))

      (local.tee $number (i32.div_u (local.get $number) (i32.const 10)))
      (br_if $writeDigit (i32.gt_u (i32.const 0)))
    )
    (return (i32.add (local.get $offset) (local.get $digits)))
  )

  ;;
  ;; Decode a group of characters using run-length encoding
  ;;
  ;; @param {i32} offset - The write offset for the output string
  ;; @param {i32} char - The character for the current chunk
  ;; @param {i32} count - The count of characters in the chunk
  ;;
  ;; @returns {i32} - The new write offset for the output string
  ;;
  (func $decodeGroup (param $offset i32) (param $char i32) (param $count i32) (result i32)
    (local $end i32)
    (local.set $end (i32.add (local.get $offset) (local.get $count)))
    (loop $writeChar
      (i32.store8 (local.get $offset) (local.get $char))
      (local.tee $offset (i32.add (local.get $offset) (i32.const 1)))
      (br_if $writeChar (i32.lt_u (local.get $end)))
    )
    (return (local.get $end))
  )

  ;; return 1 if ASCII digit, 0 otherwise
  (func $isDigit (param $c i32) (result i32)
    (i32.and  ;; char >= '0' && char <= '9'
      (i32.ge_u (local.get $c) (i32.const 48))
      (i32.le_u (local.get $c) (i32.const 57)))
  )  
)