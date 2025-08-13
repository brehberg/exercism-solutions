(module
  (memory (export "mem") 1)

  (func (export "parse") (param $offset i32) (param $length i32) (result i32 i32)
    (local $pos i32) (local $end i32) (local $char i32)
    (local $out i32) (local $hasSplit i32) 

    (local.tee $pos (local.tee $out (local.get $offset)))
    (local.set $end (i32.add (local.get $length)))
    (local.set $hasSplit (i32.const 1))

    (loop $convert
      (local.set $char (i32.load8_u (local.get $pos)))

      (block $skip
        (if (call $isSplitter (local.get $char))
          (then  ;; set the split flag is word boundary found
            (local.set $hasSplit (i32.const 1))
          )
          (else  ;; skip to next position if no split found
            (br_if $skip (i32.eqz (local.get $hasSplit)))
          )
        )

        (if (call $isLower (local.get $char))
          (then  ;; lowercase after split found, output this char as uppercase
            (i32.store8 (local.get $out) (i32.sub (local.get $char) (i32.const 32)))
          )
          (else (if (call $isUpper (local.get $char))
            (then  ;; uppercase after split found, output this char as itself
              (i32.store8 (local.get $out) (local.get $char))
            )
            (else (br $skip))  ;; skip any other characters
          ))
        )
        ;; increment the output pointer and clear split found flag
        (local.set $out (i32.add (local.get $out) (i32.const 1)))
        (local.set $hasSplit (i32.const 0))
      )
      (local.tee $pos (i32.add (local.get $pos) (i32.const 1)))
      (br_if $convert (i32.lt_u (local.get $end)))
    )
    (return (local.get $offset) (i32.sub (local.get $out) (local.get $offset)))
  )

  ;; return 1 if word splitter, 0 otherwise
  (func $isSplitter (param $c i32) (result i32)
    (i32.or  ;; char == space || char == hyphen
      (i32.eq (local.get $c) (i32.const 32))
      (i32.eq (local.get $c) (i32.const 45)))           
  )  
  ;; return 1 if uppercase, 0 otherwise
  (func $isUpper (param $c i32) (result i32)
    (i32.and  ;; char >= 'A' && char <= 'Z'
      (i32.ge_u (local.get $c) (i32.const 65))
      (i32.le_u (local.get $c) (i32.const 90)))           
  )
  ;; return 1 if lowercase, 0 otherwise
  (func $isLower (param $c i32) (result i32)
    (i32.and  ;; char >= 'a' && char <= 'z'
      (i32.ge_u (local.get $c) (i32.const 97))
      (i32.le_u (local.get $c) (i32.const 122)))
  )
)
