package KindergartenGarden;

use v5.40;

use Exporter qw<import>;
our @EXPORT_OK = qw<plants>;

sub plants ( $diagram, $student ) {
    my %plants = (
        C => 'clover',
        G => 'grass',
        R => 'radishes',
        V => 'violets',
    );

    my @students = split(',', "Alice,Bob,Charlie,David,Eve,Fred,Ginny,Harriet,Ileana,Joseph,Kincaid,Larry");

    my $offset;
    for my $i (0 .. $#students) {
        if ($students[$i] eq $student) {
            $offset = $i * 2;
            last;
        }
    }
    
    my @result;
    foreach (split('\n', $diagram)) {
        push(
            @result,
            $plants{ substr($_, $offset, 1) },
            $plants{ substr($_, $offset+1, 1) }
        );
    }

    return \@result;
}

1;
