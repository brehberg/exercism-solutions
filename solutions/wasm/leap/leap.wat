(module
  ;; Returns 1 if leap year, 0 otherwise
  (func (export "isLeap") (param $year i32) (result i32)  
    ;; Occurs on every year that is evenly divisible by 4
    ;; except every year that is evenly divisible by 100
    ;; unless the year is also evenly divisible by 400.
    (i32.and (call $isDivisibleBy (local.get $year) (i32.const 4))
        (i32.or (i32.eqz (call $isDivisibleBy (local.get $year) (i32.const 100)))
            (call $isDivisibleBy (local.get $year) (i32.const 400))))
  )
  ;; Returns 1 if x is evenly divisible by n
  (func $isDivisibleBy (param $x i32) (param $n i32) (result i32)
    (i32.eqz (i32.rem_u (local.get $x) (local.get $n)))
  )
)
