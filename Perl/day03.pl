#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
}

use lib $directory;

use Modern::Perl;
use autodie;
use Data::Dumper;
#use Storable 'dclone';
#use AOC::Geometry qw(Point2D Line2D);

package Claim;
	use Moose;
	use List::Util qw(min max);
	
	has 'id' =>		(is => 'ro', isa => 'Int');
	has 'xmin' =>	(is => 'ro', isa => 'Int');
	has 'xmax' =>	(is => 'ro', isa => 'Int');
	has 'ymin' =>	(is => 'ro', isa => 'Int');
	has 'ymax' =>	(is => 'ro', isa => 'Int');
	
	sub overlaps {
		my ($self, $other) = @_;
		my $ox = ($self->xmin <= $other->xmax) && ($self->xmax >= $other->xmin);
		my $oy = ($self->ymin <= $other->ymax) && ($self->ymax >= $other->ymin);
		return $ox && $oy;
	}
	
	sub overlap_inches {
		my ($self, $other) = @_;
		my @result = ();
		if ($self->overlaps($other)) {
			my $xmin = max($self->xmin, $other->xmin);
			my $xmax = min($self->xmax, $other->xmax);
			my $ymin = max($self->ymin, $other->ymin);
			my $ymax = min($self->ymax, $other->ymax);
			
			for (my $x = $xmin; $x <= $xmax; $x++) {
				for (my $y = $ymin; $y <= $ymax; $y++) {
					push(@result, $x . ',' . $y);
				}
			}
		}
		return @result;
	}
	
	no Moose;
__PACKAGE__->meta->make_immutable;


package main;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = '03.test.txt';
my $INPUT_FILE = '03.challenge.txt';
my @input = parse_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2018, Day 03: No Matter How You Slice It";

solve_part_one(@input);
solve_part_two(@input);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @content;
	
	while (my $line = <$input>) {
		chomp $line;
		
		# Line is #{id} @ {xmin},{ymin}: {w}x{h}
		$line =~ m/#(\d+) @ (\d+),(\d+): (\d+)x(\d+)/;
		my $claim = Claim->new('id' => $1+0,
								'xmin' => $2+0, 'xmax' => $2 + $4 - 1,
								'ymin' => $3+0, 'ymax' => $3 + $5 - 1);
		push(@content, $claim);
	}
	
	close $input;
	return @content;
}

sub solve_part_one {
	my @input = @_;
	my %overlaps = ();
	
	for (my $i = 0; $i < $#input; $i++) {
		for (my $j = $i+1; $j <= $#input; $j++) {
			my @coords = $input[$i]->overlap_inches($input[$j]);
			for my $coord (@coords) {
				$overlaps{$coord} = 1;
			}
		}
	}
	
	say "Part One:";
	say "The total overlapping claim area is " . scalar(keys(%overlaps)) . " square inches.";
}

sub solve_part_two {
	my @input = @_;
	
	for (my $i = 0; $i <= $#input; $i++) {
		my $overlap_count = 0;
		for (my $j = 0; $j <= $#input; $j++) {
			next if ($i == $j);
			if ($input[$i]->overlaps($input[$j])) {
				$overlap_count ++;
				last;
			}
		}
		if ($overlap_count == 0) {
			say "Part Two:";
			say "The only claim ID that does not overlap any others is " . $input[$i]->id;
			last;
		}
	}
}
