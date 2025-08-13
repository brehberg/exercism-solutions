(module
  (memory (export "mem") 1)

  (func (export "compute") 
    (param $firstOffset i32) (param $firstLength i32) 
    (param $secondOffset i32) (param $secondLength i32) 
    (result i32)
    
    (local $count i32) (local $i i32)

    (if (i32.ne (local.get $firstLength) (local.get $secondLength)) 
      (return (i32.const -1)) ;; different lengths not allowed
    )

    (loop $compare
      (i32.load8_u (i32.add (local.get $firstOffset) (local.get $i)))
      (i32.load8_u (i32.add (local.get $secondOffset) (local.get $i)))
      (if (i32.ne) (local.set $count (i32.add (local.get $count) (i32.const 1))))

      (local.tee $i (i32.add (local.get $i) (i32.const 1)))
      (br_if $compare (i32.lt_u (local.get $firstLength)))
    )
        
    (return (local.get $count))
  )
)
