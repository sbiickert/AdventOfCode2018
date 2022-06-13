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
#my $INPUT_FILE = '05.test.txt';
my $INPUT_FILE = '05.challenge.txt';
my $input = parse_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2018, Day 05: Alchemical Reduction";

solve_part_one($input);
solve_part_two($input);


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
	my $reacted = react3($polymer);
	#say $reacted;
	say "Part One:";
	say "The length of the polymer after reactions is " . (length($reacted));
}

sub solve_part_two {
	my $polymer = shift;
	my $best_letter = '';
	my $shortest = 50000;
	
	for my $letter ('a' .. 'z') {
		my $test = $polymer;
		$test =~ s/$letter//gi;
		my $result = react3($test);
		#say "$letter: $result";
		my $result_length = length($result);
		if ($result_length < $shortest) {
			$shortest = $result_length;
			$best_letter = $letter;
		}
	}
	
	say "Part Two:";
	say "The best letter to remove is $best_letter, resulting in a polymer with length $shortest";
}

/*
	I am leaving in react1 and react2, both successful algorithms, but slow.
*/

# Builds a copy of the polymer in a new array on each loop.
# Slow in three areas: (1) the conditional, (2) pushing onto @newunits, (3) @units = @newunits
sub react1 {
	my $polymer = shift;
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
	}
	return join('', @units);
}

# Tried using a mask (for units no longer in the polymer) to avoid mem copying
# Conditional is still there (slow). So much math in Perl is slow, apparently.
# Was faster to run than react1, but profiling was super slow.
sub react2 {
	my $polymer = shift;
	my @units = split('', $polymer);
	my @mask = (1) x scalar(@units);
	my $changed = 1;
	
	while ($changed) {
		$changed = 0;
		for (my $i = 0; $i <= $#units; $i++) {
			next if ($mask[$i] == 0);
			my $offset = 1;
			while (($i+$offset <= $#units) && ($mask[$i+$offset] == 0)) {
				$offset++;
			}
			my $j = $i+$offset;
			next if ($j > $#units);
			
			if (($i < $#units) && ($units[$i] ne $units[$j]) && (lc($units[$i]) eq lc($units[$j]))) {
				# Collapse. Jump over second character.
				#say "letter at $i is $units[$i]. letter at $j is $units[$j]";
				$mask[$i] = 0;
				$mask[$i+$offset] = 0;
				$i += $offset + 1;
				$changed = 1;
			}
		}
		#say join('', @units);
		#say join('', @mask);
	}
	
	my @reacted = ();
	for (my $i = 0; $i <= $#units; $i++) {
		if ($mask[$i]) {
			push(@reacted, $units[$i]);
		}
	}
	return join('', @reacted);
}

# Probably should have started here. Using regex to remove the pairs.
sub react3 {
	my $polymer = shift;
	my $len = 100000;
	
	while ($len != length($polymer)) {
		$len = length($polymer);
		for my $l ('a' .. 'z') {
			my $u = uc($l);
			$polymer =~ s/$l$u//g;
			$polymer =~ s/$u$l//g;
		}
		#say $polymer;
	}
	return $polymer;
}