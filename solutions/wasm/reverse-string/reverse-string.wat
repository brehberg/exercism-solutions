(module
  (memory (export "mem") 1)
 
  (func (export "reverseString") (param $offset i32) (param $length i32) (result i32 i32)
    (local $i i32) (local $end i32) (local $mid i32) 
    (local $j i32) (local $char i32)

    (local.tee $i (local.get $offset))
    ;; end = offset + length
    (local.tee $end (i32.add (local.get $length)))
    ;; mid = end - (length / 2 + 1)
    (i32.div_u (local.get $length) (i32.const 2))
    (local.set $mid (i32.sub (i32.add (i32.const 1))))

    (loop $swap
      ;; j = end - i - 1 + offset
      (i32.sub (i32.sub (local.get $end) (local.get $i)) (i32.const 1))
      (local.set $j (i32.add (local.get $offset)))

      (local.set $char (i32.load8_u (local.get $i)))
      (i32.store8 (local.get $i) (i32.load8_u (local.get $j)))
      (i32.store8 (local.get $j) (local.get $char))

      (local.tee $i (i32.add (local.get $i) (i32.const 1)))
      (br_if $swap (i32.le_u (local.get $mid)))
    )      
    (return (local.get $offset) (local.get $length))
  )
)
