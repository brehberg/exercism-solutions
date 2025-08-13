def eggs:
  recurse(if . <= 0 then empty
          else ./2|floor end) %2;

def count(seq): reduce seq as $i (0; .+$i);

.number|count(eggs)
