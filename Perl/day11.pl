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
use AOC::Grid qw(Grid2D);

my $INPUT = 2187;
my $SIZE = 300;

say "Advent of Code 2018, Day 11: Chronal Charge";

# Testing
# say calc_power_level(8,3,5);
# say calc_power_level(57,122,79);
# say calc_power_level(39,217,196);
# say calc_power_level(71,101,153);

solve_part_one();
#solve_part_two(@input);


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
	my $grid = Grid2D->new('width' => $SIZE, 'height' => $SIZE, 'default' => 0);
	for (my $y = 1; $y <= $SIZE; $y++) {
		for (my $x = 1; $x <= $SIZE; $x++) {
			$grid->set(calc_power_level($INPUT,$x,$y), $y, $x);
		}
	}
	
	my $max_power = -1000;
	my $max_coords = '';
	for (my $y = 1; $y <= $SIZE-3; $y++) {
		for (my $x = 1; $x <= $SIZE-3; $x++) {
			my $power = 0;
			for (my $a = 0; $a < 3; $a++) {
				for (my $b = 0; $b < 3; $b++) {
					$power += $grid->get($y+$a, $x+$b);
				}
			}
			if ($power > $max_power) {
				$max_power = $power;
				$max_coords = "$x,$y";
				say "New highest: power $power at $max_coords";
			}
		}
	}	
}

sub solve_part_two {
	my @input = @_;
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