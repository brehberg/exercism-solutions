def validate: 
  if . < 1 then "Only positive integers are allowed"|halt_error end;

# Generate the hailstone sequence as a stream for counting
def hailstone:
  recurse(if . <= 1 then empty
          elif .%2 == 0 then ./2
          else 3*.+1 end);

def count(seq): reduce seq as $_ (0; .+1);

def steps:
  .
  | validate
  | count(hailstone)-1;
