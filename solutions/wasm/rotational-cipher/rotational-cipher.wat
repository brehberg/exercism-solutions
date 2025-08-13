(module
  (memory (export "mem") 1)

  ;; Rotate a string when given a plaintext and amount to shift by.
  (func (export "rotate") (param $textOffset i32) (param $textLength i32) (param $shiftKey i32) (result i32 i32)
    (local $pos i32) (local $end i32) (local $char i32)

    (local.set $pos (local.get $textOffset))
    (local.set $end (i32.add (local.get $textOffset) (local.get $textLength)))
    (loop $rotate
      (local.set $char (i32.load8_u (local.get $pos)))
  
      (if (call $isLower (local.get $char))
        (then 
          (i32.store8 (local.get $pos)
            (call $rot (local.get $char) (local.get $shiftKey) (i32.const 97)))
        )
        (else (if (call $isUpper (local.get $char))          
          (i32.store8 (local.get $pos)
            (call $rot (local.get $char) (local.get $shiftKey) (i32.const 65)))
        ))
      )
      (local.tee $pos (i32.add (local.get $pos) (i32.const 1)))
      (br_if $rotate (i32.lt_u (local.get $end)))
    )    
    (return (local.get $textOffset) (local.get $textLength))
  )
  ;; Each letter is shifted right based on the value of the key.
  (func $rot (param $c i32) (param $n i32) (param $start i32) (result i32)
    ;; start + (c+n-start)%26
    (i32.add (local.get $c) (i32.sub (local.get $n) (local.get $start)))    
    (i32.add (i32.rem_u (i32.const 26)) (local.get $start))
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
