(module
  ;; Given a number n, return the number of steps required to reach 1.
  (func (export "steps") (param $n i32) (result i32)
    (local $count i32)

    (if (i32.le_s (local.get $n) (i32.const 0))
      (return (i32.const -1))
    )
    (if (i32.le_s (local.get $n) (i32.const 1))
      (return (i32.const 0))
    )

    (loop $reduce
      (local.set $count (i32.add (local.get $count) (i32.const 1)))

      (if (i32.eqz (i32.rem_u (local.get $n) (i32.const 2)))
        (then  ;; If n is even, divide n by 2
          (local.set $n (i32.div_s (local.get $n) (i32.const 2)))
        )
        (else  ;; If n is odd, multiply n by 3 and add 1
          (i32.mul (local.get $n) (i32.const 3))
          (local.set $n (i32.add (i32.const 1)))
        )
      )

      (br_if $reduce (i32.ne (local.get $n) (i32.const 1)))
    )
    
    (return (local.get $count))
  )
)