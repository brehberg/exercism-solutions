def divisible_by(n): .year % n == 0;

(.|divisible_by(4)) and
    (.|divisible_by(100)|not) or
    (.|divisible_by(400))
