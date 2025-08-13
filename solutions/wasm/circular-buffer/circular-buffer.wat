(module
  (memory (export "mem") 1 4) ;; a WebAssembly page is 65536 bytes, so up to 16384 i32s

  (global $intSize i32 (i32.const 4))       ;; 4 bytes per int32
  (global $memSize i32 (i32.const 0x4000))  ;; 16384 ints per page
  (global $maxSize i32 (i32.const 0x10000)) ;; 65536 ints (4 pages)

  (global $head (mut i32) (i32.const -1))
  (global $tail (mut i32) (i32.const -1))
  (global $size (mut i32) (i32.const 0))
  
  ;; returns 0 on success or -1 on error 
  (func (export "init") (param $newCapacity i32) (result i32)
    (if (i32.or  ;; newCapacity is a number between 0 and 65536
      (i32.lt_s (local.get $newCapacity) (i32.const 0))
      (i32.gt_s (local.get $newCapacity) (global.get $maxSize)))
      (return (i32.const -1))
    )
    (global.set $size (i32.mul (local.get $newCapacity) (global.get $intSize)))
    (memory.grow (i32.div_s (i32.sub (local.get $newCapacity) (i32.const 1)) (global.get $memSize)))
    (i32.sub (i32.const 1))
  )

  (func $clear (export "clear")
    (global.set $head (i32.const -1))
    (global.set $tail (i32.const -1))
  )

  ;; returns 0 on success or -1 on error 
  (func $write (export "write") (param $elem i32) (result i32)
    (if (call $isFull) (return (i32.const -1)))

    (if (call $isEmpty)
      (then 
        (global.set $head (i32.const 0))
        (global.set $tail (i32.const 0))
      )
      (else
        (global.set $tail (call $incr (global.get $tail)))
      )
    )
    (i32.store (global.get $tail) (local.get $elem))
    (i32.const 0)
  )

  ;; returns 0 on success or -1 on error 
  (func (export "forceWrite") (param $elem i32) (result i32)
    (if (call $isFull)
      (then
        (global.set $tail (global.get $head))
        (global.set $head (call $incr (global.get $head)))
        (i32.store (global.get $tail) (local.get $elem))
      )
      (else
        (return (call $write (local.get $elem)))
      )
    )
    (i32.const 0)
  )

  ;; Returns Go-style error handling tuple (i32, i32)
  ;; The first element of the return tuple is the returned value or -1 on error 
  ;; The second element should be 0 on success or -1 on error
  (func (export "read") (result i32 i32)
    (local $result i32)
    (if (call $isEmpty) (return (i32.const -1) (i32.const -1)))

    (local.set $result (i32.load (global.get $head)))

    (if (i32.eq (global.get $head) (global.get $tail))
      (then ;; final element was removed from buffer
        (call $clear)
      )
      (else
        (global.set $head (call $incr (global.get $head)))
      )
    )
    (return (local.get $result) (i32.const 0))
  )

  (func $isEmpty (result i32)
    (i32.lt_s (global.get $head) (i32.const 0))
  )
  (func $isFull (result i32)
    (i32.eq (global.get $head) (call $incr (global.get $tail)))
  )  
  (func $incr (param $n i32) (result i32)
    (i32.add (local.get $n) (global.get $intSize))
    (i32.rem_u (global.get $size))
  )
)
