(module
  (memory (export "mem") 1)
 
  ;; Returns the index for a needle in the haystack using the binary search algorithm.
  ;; It returns -1 if the needle is not found. Assumes size of i32
  (func (export "find") (param $base i32) (param $nelems i32) (param $needle i32) (result i32)
    (local $low i32) (local $mid i32) (local $high i32) (local $value i32)

    (i32.mul (i32.const 4) (i32.sub (local.get $nelems) (i32.const 1)))
    (local.tee $low (local.get $base))
    (local.set $high (i32.add))  ;; high = base + 4*(nelems - 1)

    (loop $search      
      (local.tee $mid (i32.div_u (i32.add (local.get $low) (local.get $high)) (i32.const 2)))
      (local.tee $mid (i32.sub (i32.rem_u (local.get $mid) (i32.const 4))))
      (local.tee $value (i32.load))  ;; mid = (low + high)/2 adjusted to 4-byte boundary

      (if (i32.eq (local.get $needle))
        (then  ;; index = (mid - base)/4
          (return (i32.div_u (i32.sub (local.get $mid) (local.get $base)) (i32.const 4)))
        )
        (else (if (i32.gt_s (local.get $value) (local.get $needle))
          (then (local.set $high (i32.sub (local.get $mid) (i32.const 4))))
          (else (local.set $low (i32.add (local.get $mid) (i32.const 4))))
        ))
      )
      (br_if $search (i32.le_u (local.get $low) (local.get $high)))
    )
    (i32.const -1)
  )
)
