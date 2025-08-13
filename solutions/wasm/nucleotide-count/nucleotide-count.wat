(module
  (memory (export "mem") 1)

  (func (export "countNucleotides") (param $offset i32) (param $length i32) (result i32 i32 i32 i32)  
    (local $A i32)
    (local $C i32)
    (local $G i32)
    (local $T i32)

    (local $i i32)
    (local $char i32)

    (if (i32.eq (local.get $length) (i32.const 0))
      (return (local.get $A) (local.get $C) (local.get $G) (local.get $T))
    )

    (loop $nucleotide_count
      ;; load the next character from memory and compare the result
      (local.set $char (i32.load8_s (i32.add (local.get $i) (local.get $offset))))

      (if (i32.eq (local.get $char) (i32.const 65)) ;; 65 = A
        (then 
          (local.set $A (i32.add (local.get $A) (i32.const 1)))
        )
        (else (if (i32.eq (local.get $char) (i32.const 67)) ;; 67 = C
        (then 
          (local.set $C (i32.add (local.get $C) (i32.const 1)))
        )
        (else (if (i32.eq (local.get $char) (i32.const 71)) ;; 71 = G
        (then 
          (local.set $G (i32.add (local.get $G) (i32.const 1)))
        )
        (else (if (i32.eq (local.get $char) (i32.const 84)) ;; 84 = T
        (then 
          (local.set $T (i32.add (local.get $T) (i32.const 1)))
        )
        (else
          (return (i32.const -1) (i32.const -1) (i32.const -1) (i32.const -1))
        )
      )))))))

      (local.set $i (i32.add (local.get $i) (i32.const 1))) ;; add one to $i
       ;; if $i is equal to $length then branch to end of nucleotide loop
      (br_if $nucleotide_count (i32.ne (local.get $i) (local.get $length)))
    )    

    (return (local.get $A) (local.get $C) (local.get $G) (local.get $T))
  )
)
