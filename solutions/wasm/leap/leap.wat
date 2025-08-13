(module
  ;; Returns 1 if leap year, 0 otherwise
  (func (export "isLeap") (param $year i32) (result i32)  
    (if (call $divisibleBy (local.get $year) (i32.const 100))
      (then (return (call $divisibleBy (local.get $year) (i32.const 400))))
      (else (return (call $divisibleBy (local.get $year) (i32.const 4))))
    )
    (return (i32.const 0))
  )
  ;; Returns 1 if x is evenly divisible by n
  (func $divisibleBy (param $x i32) (param $n i32) (result i32)
    (i32.eqz (i32.rem_u (local.get $x) (local.get $n)))
  )
)
