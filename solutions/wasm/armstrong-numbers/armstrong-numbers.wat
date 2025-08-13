(module
  ;; returns 1 if armstrong number, 0 otherwise
  (func (export "isArmstrongNumber") (param $candidate i32) (result i32)
    (local $count i32) (local $rem i32) (local $sum i32)

    (if (i32.eqz (local.get $candidate)) (return (i32.const 1)))

    ;; determine number of digits in the number
    (local.set $rem (local.get $candidate))
    (loop $digits
      (local.set $count (i32.add (local.get $count) (i32.const 1)))
      (local.tee $rem (i32.div_u (local.get $rem) (i32.const 10)))
      (br_if $digits (i32.gt_u (i32.const 0)))
    )
  
    (local.set $rem (local.get $candidate))
    (loop $calculate
      ;; take advantage of integer division to determine each digit
      ;;   digit = rem - 10 * (rem / 10)
      (i32.sub (local.get $rem) (i32.mul (i32.const 10)
        (i32.div_u (local.get $rem) (i32.const 10))))
    
      ;; calculate to sum of digits each to the power of digit count
      ;;   sum  = sum + digit ^ count
      (local.set $sum (i32.add (call $exp (local.get $count)) (local.get $sum)))

      (local.tee $rem (i32.div_u (local.get $rem) (i32.const 10)))
      (br_if $calculate (i32.gt_u (i32.const 0)))
    )
    (i32.eq (local.get $candidate) (local.get $sum))
  )

  (func $exp (param $base i32) (param $power i32) (result i32)
    (if (i32.eqz (local.get $power)) (return (i32.const 1)))
    (call $exp (local.get $base)
      (i32.sub (local.get $power) (i32.const 1)))
    (i32.mul (local.get $base))
  )
)
