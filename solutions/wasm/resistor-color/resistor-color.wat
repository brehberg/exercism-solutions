(module
  (memory (export "mem") 1)

  ;; Better Be Right Or Your Great Big Values Go Wrong
  (data (i32.const 200) "black,brown,red,orange,yellow,green,blue,violet,grey,white,")

  (global $colorsPos i32 (i32.const 200))
  (global $colorsLen i32 (i32.const 58))
  (global $colorsEnd (mut i32) (i32.const 0))

  ;; Return buffer of comma separated colors
  ;; black,brown,red,orange,yellow,green,blue,violet,grey,white
  (func (export "colors") (result i32 i32)
    (return (global.get $colorsPos) (global.get $colorsLen))
  )

  ;; Called each time a module is initialized
  ;; Can be used to populate globals similar to a constructor
  (func $initialize
    (global.set $colorsEnd (i32.add (i32.const 1)
      (i32.add (global.get $colorsPos) (global.get $colorsLen))))
  )
  (start $initialize)

  ;; Given a valid resistor color, returns the associated value 
  (func (export "colorCode") (param $offset i32) (param $len i32) (result i32)
    (local $pos i32) (local $char i32) (local $i i32)
    (local $match i32) (local $value i32)

    (local.set $pos (global.get $colorsPos))
    (local.set $i (local.get $offset))
    (local.set $match (i32.const 1))  ;; assume match, then check if true

    (loop $findMatch
      (local.tee $char (i32.load8_u (local.get $pos)))

      (if (i32.eq (i32.const 44)) ;; 44 = (comma separator)
        (then  ;; adjust match variables to check next color
          (if (local.get $match) (return (local.get $value)))
          (local.set $i (local.get $offset))
          (local.set $match (i32.const 1))
          (local.set $value (i32.add (local.get $value) (i32.const 1)))
        )  
        (else  ;; check if current color matches input          
          (local.set $match (i32.eq (local.get $char)
            (i32.load8_u (local.get $i))))
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
        )
      )  

      (local.tee $pos (i32.add (local.get $pos) (i32.const 1)))
      (br_if $findMatch (i32.lt_u (global.get $colorsEnd)))
    )
    (return (i32.const -1))
  )
)
