(module

  ;; Calculate the integer square root of a positive integer
  (func (export "squareRoot") (param $radicand i32) (result i32)
    (local $n i32) (local $step i32)

    (local.set $n (local.get $radicand))
    (local.set $step (i32.const -1))
    (loop $calc
      (local.set $step (i32.add (local.get $step) (i32.const 1)))
      ;; next n = n - (step * 2 + 1)
      (local.tee $n (i32.sub (local.get $n)
        (i32.add (i32.mul (local.get $step) (i32.const 2)) (i32.const 1))))
      (br_if $calc (i32.gt_u (i32.const 0)))
    )

    (return (i32.add (local.get $step) (i32.const 1)))
  )
)
