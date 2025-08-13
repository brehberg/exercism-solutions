(module
  (memory (export "mem") 1)

  (data (i32.const 200) "One for you, one for me.")

  ;; global variables to describe the data parts in linear memory
  (global $outPos i32 (i32.const 8))  
  (global $dataPos i32 (i32.const 200))
  (global $dataLen i32 (i32.const 24))
  (global $firstPart i32 (i32.const 8))    ;; length of "One for "
  (global $lastPart i32 (i32.const 13))    ;; length of ", one for me."
  (global $partLen (mut i32) (i32.const 0))
  (global $namePos (mut i32) (i32.const 0))
  (global $lastPos (mut i32) (i32.const 0))

  (func (export "twoFer") (param $offset i32) (param $length i32) (result i32 i32)
    (local $outLen i32)

    (if (i32.eqz (local.get $length))
      (then
        (local.tee $outLen (global.get $dataLen))
        (call $strCopy (global.get $dataPos) (global.get $outPos))
      )
      (else
        (local.set $outLen (i32.add (global.get $partLen) (local.get $length)))
        ;; copy the first part to output section
        (call $strCopy (global.get $firstPart) (global.get $dataPos) (global.get $outPos))
        ;; copy the input data to output section
        (call $strCopy (local.get $length) (local.get $offset) (global.get $namePos))
        ;; copy the last part to output section
        (call $strCopy (global.get $lastPart) (global.get $lastPos)
          (i32.add (global.get $namePos) (local.get $length))
        )
      )
    )

    (return (global.get $outPos) (local.get $outLen))
  )

  ;; Called each time the module is initialized to populate additional globals
  (func $initialize
    (global.set $partLen (i32.add (global.get $firstPart) (global.get $lastPart)))
    (global.set $namePos (i32.add (global.get $outPos) (global.get $firstPart)))
    (global.set $lastPos (i32.sub (i32.add (global.get $dataPos) (global.get $dataLen)) (global.get $lastPart)))
  )
  (start $initialize)

  (func $strCopy (param $len i32) (param $from i32) (param $to i32)
    (local $pos i32)

    (loop $copy
      (i32.store8 (i32.add (local.get $to) (local.get $pos))
        (i32.load8_u (i32.add (local.get $from) (local.get $pos)))
      )
      (local.tee $pos (i32.add (local.get $pos) (i32.const 1)))
      (br_if $copy (i32.ne (local.get $len)))
    )
  )
)
