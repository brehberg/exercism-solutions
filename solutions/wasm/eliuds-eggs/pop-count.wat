(module
  (func (export "eggCount") (param $number i32) (result i32)
    (local $count i32) (local $i i32)

    ;; count the amount of 1s and return the result
    (loop $shift
      (i32.and (local.get $number) (i32.const 1))
      (local.set $count (i32.add (local.get $count)))

      (local.set $number (i32.shr_u (local.get $number) (i32.const 1)))

      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br_if $shift (i32.ne (local.get $i) (i32.const 32)))
    )    

    (return (local.get $count))
  )
)
