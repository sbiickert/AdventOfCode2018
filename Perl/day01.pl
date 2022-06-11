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
use Storable 'dclone';
#use AOC::Geometry qw(Point2D Line2D);

my $INPUT_PATH = '../input';
#my $INPUT_FILE = '01.test.txt';
my $INPUT_FILE = '01.challenge.txt';
my @input = parse_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2018, Day 1: Chronal Calibration";

solve_part_one(@input);
solve_part_two(@input);


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
	my @input = @_;
	my $value = 0;
	
	for my $drift (@input) {
		$drift =~ s/\+//;
		#say "$value + $drift = " . ($value + $drift);
		$value += $drift;
	}
	
	say "Part One: The final drift is $value.";
}

sub solve_part_two {
	my @input = @_;
	my $value = 0;
	my %frequencies = ($value => 1);
	my $i = 0;
	
	while (1) {
		if ($i > $#input) {
			$i = 0;
		}
		my $drift = $input[$i];
		$drift =~ s/\+//;
		$value += int($drift);
		say "$value";
		if (exists($frequencies{$value})) {
			last;
		}
		$frequencies{$value} += 1;
		$i++
	}
	say "Part Two: The first frequency found twice is $value.";
}
