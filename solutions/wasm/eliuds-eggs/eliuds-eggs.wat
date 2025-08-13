(module
  ;;
  ;; count the number of 1 bits in the binary representation of a number
  ;;
  ;; @param {i32} number - the number to count the bits of
  ;;
  ;; @returns {i32} the number of 1 bits in the binary representation of the number
  ;;
  (func (export "eggCount") (param $number i32) (result i32)
    (local $count i32) (local $i i32)

    ;; count the amount of 1s and return the result
    (loop $shift
      (i32.shr_u (local.get $number) (local.get $i))
      (i32.and (i32.const 1))
      (local.set $count (i32.add (local.get $count)))

      (local.tee $i (i32.add (local.get $i) (i32.const 1)))
      (br_if $shift (i32.ne (i32.const 32)))
    )

    (return (local.get $count))
  )
)

