(module
  (memory (export "mem") 1)

  ;; A pangram is a sentence using every letter of the alphabet at least once.
  ;; return 1 if pangram, 0 otherwise 
  (func (export "isPangram") (param $offset i32) (param $length i32) (result i32)    
    (local $pos i32) (local $end i32) (local $char i32) (local $letters i32)

    (local.set $pos (local.get $offset))
    (local.set $end (i32.add (local.get $offset) (local.get $length)))
    (loop $rotate
      (local.set $char (i32.load8_u (local.get $pos)))
  
      (if (i32.and  ;; char >= 'a' && char <= 'z'
        (i32.ge_u (local.get $char) (i32.const 97))
        (i32.le_u (local.get $char) (i32.const 122)))

        (then
          (local.set $letters (call $mark (local.get $letters)
            (i32.sub (local.get $char) (i32.const 97))))
        )
        (else (if (i32.and  ;; char >= 'A' && char <= 'Z'
          (i32.ge_u (local.get $char) (i32.const 65))
          (i32.le_u (local.get $char) (i32.const 90)))
          
          (local.set $letters (call $mark (local.get $letters)
            (i32.sub (local.get $char) (i32.const 65))))
        ))
      )
      (local.tee $pos (i32.add (local.get $pos) (i32.const 1)))
      (br_if $rotate (i32.lt_u (local.get $end)))
    )   

    (return (i32.eq (i32.popcnt (local.get $letters)) (i32.const 26)))
  )

  (func $mark (param $letters i32) (param $c i32) (result i32)
    (i32.shl (i32.const 1) (local.get $c))
    (i32.or (local.get $letters))
  )  
)
