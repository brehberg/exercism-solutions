(module

  ;; Square returns how many grains were on a given square
  ;; squareNum is signed
  ;; Result is unsigned
  (func $square (export "square") (param $squareNum i32) (result i64)

    (if ;; check if `squareNum` is less than 1
      (i32.lt_s (local.get $squareNum) (i32.const 1))
      (return (i64.const 0))
    )

    (if ;; check if `squareNum` is greater than 64
      (i32.gt_s (local.get $squareNum) (i32.const 64))
      (return (i64.const 0))
    )

    (i64.shl (i64.const 1) ;; perform a bitwise left-shift
      (i64.extend_i32_s    ;; sign-extend from i32 to i64
        (i32.sub (local.get $squareNum) (i32.const 1))  
      )
    ) 
  )

  ;; Total returns the total number of grains on the chessboard
  ;; Result is unsigned
  (func (export "total") (result i64)
    (i64.const -1)
  )
)
