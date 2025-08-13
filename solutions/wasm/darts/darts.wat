(module
  (func (export "score") (param $x f32) (param $y f32) (result i32)
    
    (call $square (local.get $x)) ;; call the square function on `x`    
    (call $square (local.get $y)) ;; call the square function on `y`

    ;; add both numbers, calc the square root, round up, convert to i32
    (call $points (i32.trunc_f32_s (f32.ceil (f32.sqrt (f32.add)))))
  )

  (func $points (param $dist i32) (result i32)
    (block $missed (block $outer (block $middle (block $inner 
      (br_table
        $inner  ;; dist == 0
        $inner  ;; dist == 1
        $middle ;; dist == 2
        $middle ;; dist == 3
        $middle ;; dist == 4
        $middle ;; dist == 5
        $outer  ;; dist == 6
        $outer  ;; dist == 7
        $outer  ;; dist == 8
        $outer  ;; dist == 9
        $outer  ;; dist == 10
        $missed ;; else
      (local.get $dist)))
    (return (i32.const 10))) ;; Score for inner circle
    (return (i32.const 5)))  ;; Score for middle circle
    (return (i32.const 1)))  ;; Score for outer circle
    (return (i32.const 0)))  ;; Score for missed target

  (func $square (param $n f32) (result f32)
    (f32.mul (local.get $n) (local.get $n)) ;; multiply one number by itself
  )
)
