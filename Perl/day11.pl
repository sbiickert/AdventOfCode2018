#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
}

use lib $directory;

package main;

use Modern::Perl;
use autodie;
use Data::Dumper;
#use Storable 'dclone';
#use AOC::Grid qw(Grid2D);

my $INPUT = 2187;
my $SIZE = 300;

say "Advent of Code 2018, Day 11: Chronal Charge";

# Testing
# say calc_power_level(8,3,5);
# say calc_power_level(57,122,79);
# say calc_power_level(39,217,196);
# say calc_power_level(71,101,153);

my @grid = ();
for (my $y = 1; $y <= $SIZE; $y++) {
	for (my $x = 1; $x <= $SIZE; $x++) {
		$grid[$y][$x] = calc_power_level($INPUT,$x,$y);
	}
}

solve_part_one(\@grid);
solve_part_two(\@grid);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @content;
	
	while (my $line = <$input>) {
		chomp $line;
		push(@content, $line);
	}
	
	close $input;
	
	return @content;
}

sub solve_part_one {
	my $grid = shift;
	my ($power, $coords) = find_max_power($grid, 3);
	say "Part One:";
	say "Max power $power at $coords with square size 3.";

}


sub solve_part_two {
	my $grid = shift;
	my $max_power = -1000;
	my $max_coords = '';
	my $max_s = 0;
	
	say "Part Two:";
	for (my $s = 3; $s <= $SIZE; $s++) {
		my ($power, $coords) = find_max_power($grid, $s);
		if ($power > $max_power) {
			$max_power = $power;
			$max_coords = $coords;
			$max_s = $s;
		}
		say "$s: $max_power at $max_coords with square size $max_s";
	}
	say "Max power $max_power at $max_coords with square size $max_s.";
}

sub find_max_power {
	my ($grid_ref, $sqr_size) = @_;
	my @grid = @{$grid_ref};
	
	my $max_power = -1000;
	my $max_coords = '';
	for (my $y = 1; $y <= $SIZE-$sqr_size; $y++) {
		my $power = 0;
		for (my $x = 1; $x <= $SIZE-$sqr_size; $x++) {
			if ($x == 1) {
				for (my $a = 0; $a < $sqr_size; $a++) {
					for (my $b = 0; $b < $sqr_size; $b++) {
						$power += $grid[$y+$a][$x+$b];
					}
				}
			}
			else {
				my $b = -1;
				for (my $a = 0; $a < $sqr_size; $a++) {
					$power -= $grid[$y+$a][$x+$b];
				}
				$b = $sqr_size-1;
				for (my $a = 0; $a < $sqr_size; $a++) {
					$power += $grid[$y+$a][$x+$b];
				}
			}
			if ($power > $max_power) {
				$max_power = $power;
				$max_coords = "$x,$y";
			}
		}
	}	
	
	return ($max_power, $max_coords);
}

sub calc_power_level {
	my ($serial_num, $x, $y) = @_;
	
	my $rack_id = $x + 10;
	my $power_level = $rack_id * $y;
	$power_level += $serial_num;
	$power_level *= $rack_id;
	$power_level = get_hundreds_digit($power_level);
	$power_level -= 5;
	return $power_level;
}

sub get_hundreds_digit {
	my $number = shift;
	$number = int($number / 100);
	return $number % 10;
}