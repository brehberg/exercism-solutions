(module
  (memory (export "mem") 1)

  (func (export "toRna") (param $offset i32) (param $length i32) (result i32 i32)
    (local $i i32) (local $end i32) (local $char i32)
    
    (local.tee $i (local.get $offset))
    (local.set $end (i32.add (local.get $length)))

    (loop $transcribe
      (local.set $char (i32.load8_u (local.get $i)))

      (if (i32.eq (local.get $char) (i32.const 65))
        (then (i32.store8 (local.get $i) (i32.const 85)))  ;; A -> U
        (else (if (i32.eq (local.get $char) (i32.const 67))
          (then (i32.store8 (local.get $i) (i32.const 71)))  ;; C -> G
          (else (if (i32.eq (local.get $char) (i32.const 71))
            (then (i32.store8 (local.get $i) (i32.const 67)))  ;; G -> C
            (else (if (i32.eq (local.get $char) (i32.const 84))
              (then (i32.store8 (local.get $i) (i32.const 65)))  ;; T -> A
              (else (i32.store8 (local.get $i) (i32.const 63)))  ;; ?
            ))
          ))
        ))
      )
      (local.tee $i (i32.add (local.get $i) (i32.const 1)))
      (br_if $transcribe (i32.lt_u (local.get $end)))
    )

    (return (local.get $offset) (local.get $length))
  )
)
