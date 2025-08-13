(module
  (memory (export "mem") 1)

  (data (i32.const 100) "PlingPlangPlong")
  (data (i32.const 116) "PlingPlong")

  ;; global variables to describe the data parts in linear memory
  (global $plingPos i32 (i32.const 100))
  (global $plangPos i32 (i32.const 105))
  (global $plongPos i32 (i32.const 110))  
  (global $oneLen i32 (i32.const 5))  
  (global $twoLen i32 (i32.const 10))
  (global $allLen i32 (i32.const 15))
  (global $plingplongPos i32 (i32.const 116))
  (global $numEnd i32 (i32.const 95))

  (func (export "convert") (param $input i32) (result i32 i32)
    (local $result i32) (local $rem i32) (local $numPos i32)

    ;; result is (n%3==0) + 2*(n%5==0) + 4*(n%7==0) - 1
    (local.tee $result (i32.const -1))
    (call $divisibleBy (local.get $input) (i32.const 3))
    (local.tee $result (i32.add))
    (call $divisibleBy (local.get $input) (i32.const 5))
    (local.tee $result (i32.add (i32.mul (i32.const 2))))
    (call $divisibleBy (local.get $input) (i32.const 7))
    (local.set $result (i32.add (i32.mul (i32.const 4))))

    (block $number (block $plingplangplong
      (block $plangplong (block $plingplong (block $plingplang
        (block $plong (block $plang (block $pling
          (local.get $result) (br_table            
            $pling       ;; 0 = has 3 as a factor
            $plang       ;; 1 = has 5 as a factor
            $plingplang  ;; 2 = has 3 and 5 as factors
            $plong       ;; 3 = has 7 as a factor
            $plingplong  ;; 4 = has 3 and 7 as factors
            $plangplong  ;; 5 = has 5 and 7 as factors
            $plingplangplong ;; has 3, 5, and 7 as factors
            $number))    ;; does not have 3, 5, or 7 as factors
    (return (global.get $plingPos) (global.get $oneLen)))
    (return (global.get $plangPos) (global.get $oneLen)))
    (return (global.get $plongPos) (global.get $oneLen)))
    (return (global.get $plingPos) (global.get $twoLen)))
    (return (global.get $plingplongPos) (global.get $twoLen)))
    (return (global.get $plangPos) (global.get $twoLen)))
    (return (global.get $plingPos) (global.get $allLen)))

    ;; convert integer into string of digits
    (local.set $numPos (global.get $numEnd))
    (local.set $rem (local.get $input))    
    (loop $convert
      (local.tee $numPos (i32.sub (local.get $numPos) (i32.const 1)))

      ;; digit = rem - 10 * (rem / 10)
      (i32.sub (local.get $rem) (i32.mul (i32.const 10)
        (i32.div_u (local.get $rem) (i32.const 10))))

      ;; store ASCII code for this digit in memory buffer      
      (i32.store8 (i32.add (i32.const 48)))
     
      (local.tee $rem (i32.div_u (local.get $rem) (i32.const 10)))
      (br_if $convert (i32.gt_u (i32.const 0)))
    )

    (return (local.get $numPos)
      (i32.sub (global.get $numEnd) (local.get $numPos)))
  )
  ;; Returns 1 if x is evenly divisible by n
  (func $divisibleBy (param $x i32) (param $n i32) (result i32)
    (i32.eqz (i32.rem_u (local.get $x) (local.get $n)))
  )
)
