(module
  (func $squareOfSum (export "squareOfSum") (param $max i32) (result i32)
    (local $sum i32)
    ;; Calculate square of sum from 1 to a given end number
    ;;      y = (x * (x+1) / 2)^2"
    (i32.add (local.get $max) (i32.const 1))
    (local.set $sum (i32.div_s (i32.mul (local.get $max)) (i32.const 2)))
    
    (i32.mul (local.get $sum) (local.get $sum))
  )

  (func $sumOfSquares (export "sumOfSquares") (param $max i32) (result i32)
    (local $sum i32)
    ;; "Calculate sum of squares from 1 to a given end number
    ;;      y = (x * (x+1) * (2x+1)) / 6"
    (i32.add (local.get $max) (i32.const 1))
    (local.set $sum (i32.mul (local.get $max)))

    (i32.add (i32.mul (i32.const 2) (local.get $max)) (i32.const 1))
    (local.set $sum (i32.mul (local.get $sum)))

    (i32.div_s (local.get $sum) (i32.const 6))
  )

  (func (export "difference") (param $max i32) (result i32)
    (i32.sub (call $squareOfSum (local.get $max)) (call $sumOfSquares (local.get $max)))
  )
)
