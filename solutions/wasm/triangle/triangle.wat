(module
  ;; Functions to determine if a triangle is equilateral, isosceles, or scalene.
    
  (func $isEqu (export "isEquilateral") (param f32 f32 f32) (result i32)
    (call $valid (local.get 0) (local.get 1) (local.get 2))
    (if (i32.eqz) (return (i32.const 0)))
    
    (i32.and
      (f32.eq (local.get 0) (local.get 1)) ;; a == b
      (f32.eq (local.get 1) (local.get 2)) ;; b == c
    )
  )

  (func $isIso (export "isIsosceles") (param f32 f32 f32) (result i32)
    (call $valid (local.get 0) (local.get 1) (local.get 2))
    (if (i32.eqz) (return (i32.const 0)))

    (i32.or (i32.or
      (f32.eq (local.get 0) (local.get 1)) ;; a == b
      (f32.eq (local.get 1) (local.get 2)) ;; b == c
      (f32.eq (local.get 0) (local.get 2)) ;; a == c
    ))
  )

  (func (export "isScalene") (param f32 f32 f32) (result i32)
    (call $valid (local.get 0) (local.get 1) (local.get 2))
    (if (i32.eqz) (return (i32.const 0)))

    (i32.eqz (i32.or
      (call $isEqu (local.get 0) (local.get 1) (local.get 2))
      (call $isIso (local.get 0) (local.get 1) (local.get 2))
    ))    
  )

  ;; Returns 1 if the sides represent a valid triangle  
  (func $valid (param f32 f32 f32) (result i32)
    ;; all side lengths must be positive
    (i32.and (i32.and 
      (f32.gt (local.get 0) (f32.const 0)) ;; a > 0
      (f32.gt (local.get 1) (f32.const 0)) ;; b > 0
      (f32.gt (local.get 2) (f32.const 0)) ;; c > 0
    ))
    (if (i32.eqz) (return (i32.const 0)))

    ;; side lengths cannot violate triangle inequality
    (i32.and (i32.and
      (f32.ge (f32.add (local.get 0) (local.get 1)) (local.get 2)) ;; a+b >= c
      (f32.ge (f32.add (local.get 1) (local.get 2)) (local.get 0)) ;; b+c >= a
      (f32.ge (f32.add (local.get 0) (local.get 2)) (local.get 1)) ;; a+c >= b
    ))
    (if (i32.eqz) (return (i32.const 0)))

    (i32.const 1)
  )
)
