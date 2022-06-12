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
#my $INPUT_FILE = '02.test.txt';
my $INPUT_FILE = '02.challenge.txt';
my @input = parse_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2018, Day 02: Inventory Management System";

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
	my @doubles = ();
	my @triples = ();
	
	for my $id (@input) {
		my %counts = ();
		my $is_double = 0;
		my $is_triple = 0;
		my @letters = split('', $id);
		for my $letter (@letters) {
			$counts{$letter}++;
		}
		for my $letter (keys(%counts)) {
			$is_double = 1 if ($counts{$letter} == 2);
			$is_triple = 1 if ($counts{$letter} == 3);
		}
		push(@doubles, $id) if ($is_double);
		push(@triples, $id) if ($is_triple);
	}
	say "Part One:";
	say "Number of doubles: " . scalar(@doubles);
	say "Number of triples: " . scalar(@triples);
	say "Checksum: " . (scalar(@doubles) * scalar(@triples));
}

sub solve_part_two {
	my @input = @_;
	
	for (my $i = 0; $i < $#input; $i++) {
		for (my $j = $i+1; $j <= $#input; $j++) {
			my $common = id_cmp($input[$i], $input[$j]);
			if ($common) {
				say "Part Two:";
				say "IDs differing by one letter in the same spot:\n$input[$i]\n$input[$j]";
				say "$common are the common letters.";
				return;
			}
		}
	}
}

sub id_cmp {
	my ($a, $b) = @_;
	my @letters_a = split('', $a);
	my @letters_b = split('', $b);
	my @common = ();
	
	for (my $i = 0; $i <= $#letters_a; $i++) {
		if ($letters_a[$i] eq $letters_b[$i]) {
			push(@common, $letters_a[$i]);
		}
	}
	if (scalar(@common) == scalar(@letters_a)-1) {
		return join('', @common);
	}
	return '';
}