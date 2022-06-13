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
#use AOC::Geometry qw(Point2D Line2D);

my $INPUT_PATH = '../input';
my $INPUT_FILE = '05.test.txt';
#my $INPUT_FILE = '05.challenge.txt';
my $input = parse_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2018, Day 05: Alchemical Reduction";

solve_part_one($input);
#solve_part_two($input);


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
	
	return $content[0]; # polymer is single line of text
}

sub solve_part_one {
	my $polymer = shift;
	#say $polymer;
	my @units = split('', $polymer);
	my $changed = 1;
	
	while ($changed) {
		$changed = 0;
		my @newunits = ();
		for (my $i = 0; $i <= $#units; $i++) {
			if (($i < $#units) && ($units[$i] ne $units[$i+1]) && (lc($units[$i]) eq lc($units[$i+1]))) {
				# Collapse. Jump over second character.
				$i++;
				$changed = 1;
			}
			else {
				push(@newunits, $units[$i]);
			}
		}
		@units = @newunits;
		#say join('', @units);
		#say scalar @units;
	}
	
	say "Part One:";
	say "The length of the polymer after reactions is " . (scalar @units);
}

sub solve_part_two {
	my @input = @_;
}
