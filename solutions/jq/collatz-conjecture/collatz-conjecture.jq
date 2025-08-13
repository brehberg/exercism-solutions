# Generate the hailstone sequence as a stream to save space (and time) when counting
def hailstone:
  recurse(if . > 1 then
            if .%2 == 0 then ./2|floor else 3*.+1 end
          else empty
          end);

def count(sequence): reduce sequence as $i (0; .+1);

def steps:
  if . < 1 then 
    "Only positive integers are allowed"|halt_error 
  else 
    .|count(hailstone)-1
  end;
