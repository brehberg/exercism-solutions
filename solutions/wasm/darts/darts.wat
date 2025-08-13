(module
  (func (export "score") (param $x f32) (param $y f32) (result i32)
    local.get $x
    call $square ;; call the square function on `x`
    local.get $y
    call $square ;; call the square function on `y`

    f32.add  ;; add up both numbers
    f32.sqrt ;; calculate the square root
    f32.ceil ;; round up
    i32.trunc_f32_s ;; convert from f32 to signed i32
    call $points    ;; call the points function
  )

  (func $points (param $d i32) (result i32)
    (block
      (block
        (block
          (block (local.get $d)
                (br_table
                          0   ;; d == 0 => (inner circle)
                          0   ;; d == 1 => (inner circle)
                          1   ;; d == 2 => (middle circle)
                          1   ;; d == 3 => (middle circle)
                          1   ;; d == 4 => (middle circle)
                          1   ;; d == 5 => (middle circle)
                          2   ;; d == 6 => (outer circle)
                          2   ;; d == 7 => (outer circle)
                          2   ;; d == 8 => (outer circle)
                          2   ;; d == 9 => (outer circle)
                          2   ;; d == 10 => (outer circle)
                          3)) ;; else => (missed target)
          (i32.const 10) ;; Score for inner circle
          (return))
        (i32.const 5) ;; Score for middle circle
        (return))
      (i32.const 1) ;; Score for outer circle
      (return))
    (i32.const 0) ;; Score for missed target  
  )

  (func $square (param $n f32) (result f32)
    ;; load `n` onto the stack twice
    local.get $n
    local.get $n
    f32.mul ;; multiply one number by the other
  )
)
