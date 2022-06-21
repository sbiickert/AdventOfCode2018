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
#my $INPUT_FILE = '12.test.txt';
my $INPUT_FILE = '12.challenge.txt';

my @input = parse_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2018, Day 12: Subterranean Sustainability";


solve_part_one(@input);
solve_part_two(@input);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my $init_state = <$input>;
	$init_state =~ m/([#\.]+)/;
	$init_state = $1;
	
	<$input>; #blank line
	
	my @live; my @die;
	
	while (my $line = <$input>) {
		chomp $line;
		$line =~ m/(^[#\.]+)/;
		my $pattern = $1;

		if ($line =~ m/#$/) {
			push(@live, $pattern);
		}
		else {
			push(@die, $pattern);
		}
	}
	
	close $input;
	
	return ($init_state, @live);
}

sub solve_part_one {
	my ($state, @rules) = @_;
	
	say "Part One:";
	my $score = process($state, \@rules, 20);
	
	say "Total score is $score";
}

sub solve_part_two {
	my ($state, @rules) = @_;
	
	say "Part Two:";
	my $first_thousand_score = process($state, \@rules, 1000);
	my $second_thousand_score = process($state, \@rules, 2000);
	my $score_per_thousand = $second_thousand_score - $first_thousand_score;

	my $score = 50000000 * $score_per_thousand; 
	$score = $score - $score_per_thousand + $first_thousand_score;
	say "Total score for fifty billion iterations is $score";
	
	my @iterations = (1000, 2000, 3000);
	for my $i (@iterations) {
		my $score = process($state, \@rules, $i);
	}
}

sub process {
	my ($state, $rules_ref, $iterations) = @_;
	my @rules = @{$rules_ref};
	
	# Pad with '.'
	my $left_offset = -5;
	$state = ('.' x abs($left_offset)) . $state . ('.' x 100);
	#say "0: $state";
	
	for (my $iter = 1; $iter <= $iterations; $iter++) {
		my $new_state = '.' x length($state); # assume dead
		
		for my $rule (@rules) {
			my $i = 0;
			$i = index($state, $rule, $i);
			while ($i > 0) {
				# Matched rule at $i. The pot we are considering is in the middle at $i+2
				substr($new_state, $i+2, 1, '#');
				$i = index($state, $rule, ++$i);
			}
		}
		
		$state = $new_state;
		if ($state =~ m/^\.{20}/) {
			$state =~ s/^\.{5}//;
			$state .= '.' x 5;
			$left_offset += 5;
		}
		#say "$iter: $state";
	}
	
	my $score = 0;
	my @chars = split('', $state);
	for (my $i = 0; $i <= $#chars; $i++) {
		$score += ($i + $left_offset) if ($chars[$i] eq '#');
	}
	return $score;
}