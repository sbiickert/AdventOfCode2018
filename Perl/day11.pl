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

sub solve_part_one {
	my $grid = shift;
	my ($power, $coords) = find_max_power($grid, 3);
	say "Part One:";
	say "Max power $power at $coords with square size 3.";

}


sub solve_part_two {
	my $grid = shift;
	my %grids = (1 => $grid);
	
	my $max_power = -1000;
	my $max_coords = '';
	my $max_s = 0;
	
	say "Part Two:";
	for (my $s = 2; $s <= $SIZE; $s++) {
		my $largest_factor = 0;
		for (my $f = $s-1; $f > 0; $f--) {
			if ($s % $f == 0) {
				$largest_factor = $f;
				last;
			}
		}
		
		my ($power, $coords, $g);
		#say "Largest factor of $s is $largest_factor";
		if ($largest_factor == 1) {
			# Square size is a prime number
			($power, $coords, $g) = find_max_power($grids{1}, $s);
		}
		else {
			# Square size has a factor that we've already summed.
			# Grab the sums from the previous work.
			($power, $coords, $g) = find_factored_max_power($grids{$largest_factor}, $s, $largest_factor);
		}
		
		$grids{$s} = $g;
		
		if ($power > $max_power) {
			$max_power = $power;
			$max_coords = $coords;
			$max_s = $s;
		}
		#say "$s: $max_power at $max_coords with square size $max_s";
	}
	
	say "Max power $max_power at $max_coords with square size $max_s.";
}

sub find_max_power {
	my ($grid_ref, $sqr_size) = @_;
	my @grid = @{$grid_ref};
	
	my $max_power = -1000;
	my $max_coords = '';
	my @out_grid = ();
	
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
			$out_grid[$y][$x] = $power;
			if ($power > $max_power) {
				$max_power = $power;
				$max_coords = "$x,$y";
			}
		}
	}	
	
	return ($max_power, $max_coords, \@out_grid);
}

sub find_factored_max_power {
	my ($grid_ref, $sqr_size, $factor) = @_;
	my @grid = @{$grid_ref};
	
	my $max_power = -1000;
	my $max_coords = '';
	my @out_grid = ();

	for (my $y = 1; $y <= $SIZE-$sqr_size; $y++) {
		for (my $x = 1; $x <= $SIZE-$sqr_size; $x++) {
			my $power = 0;
			
			for (my $fy = 0; $fy < $sqr_size; $fy += $factor) {
				for (my $fx = 0; $fx < $sqr_size; $fx += $factor) {
					$power += $grid[$y+$fy][$x+$fx];
				}
			}
			$out_grid[$y][$x] = $power;
			if ($power > $max_power) {
				$max_power = $power;
				$max_coords = "$x,$y";
			}
		}
	}
		
	return ($max_power, $max_coords, \@out_grid);
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