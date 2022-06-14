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
use AOC::Geometry qw(Point2D Line2D Bounds2D);
use List::Util qw(max);

my $INPUT_PATH = '../input';
#my $INPUT_FILE = '06.test.txt';
my $INPUT_FILE = '06.challenge.txt';
my @input = parse_input("$INPUT_PATH/$INPUT_FILE");

#my $LIMIT = 32; 	# for test
my $LIMIT = 10000;	# for challenge

say "Advent of Code 2018, Day 06: Chronal Coordinates";

solve_part_one(@input);
solve_part_two(@input);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @content;
	
	while (my $line = <$input>) {
		chomp $line;
		$line =~ m/(\d+), (\d+)/;
		my $pt = Point2D->new('px' => $1+0, 'py' => $2+0);
		push(@content, $pt);
	}
	
	close $input;
	return @content;
}

sub solve_part_one {
	my @points = @_;
	my $bounds = Bounds2D->new('xmin' => $points[0]->px, 'ymin' => $points[0]->py, 
								'xmax' => $points[0]->px, 'ymax' => $points[0]->py);
	for (my $i = 1; $i <= $#points; $i++) {
		$bounds->grow_to_fit($points[$i]);
	}

	my @grid = ();
	for (my $r = $bounds->ymin; $r <= $bounds->ymax; $r++) {
		for (my $c = $bounds->xmin; $c <= $bounds->xmax; $c++) {
			$grid[$r][$c] = '.';
		}
	}
	for (my $i = 0; $i <= $#points; $i++) {
		$grid[$points[$i]->py][$points[$i]->px] = $i;
	}
	
	# Allocate all grid cells
	for (my $r = $bounds->ymin; $r <= $bounds->ymax; $r++) {
		for (my $c = $bounds->xmin; $c <= $bounds->xmax; $c++) {
			my @closest = ();
			my $shortest_md = 100000;
			my $pt = Point2D->new('px' => $c, 'py' => $r);
			for (my $i = 0; $i <= $#points; $i++) {
				# Removed Moose from tightest loop for 4x speed boost
				#my $line = Line2D->new('from' => $pt, 'to' => $points[$i]);
				#my $md = $line->manhattan_distance();
				#
				my $md = manhattan_distance($pt, $points[$i]);
				if ($md < $shortest_md) {
					$shortest_md = $md;
					@closest = ($i);
				}
				elsif ($md == $shortest_md) {
					push(@closest, $i);
				}
			}
			if (scalar(@closest) == 1) {
				$grid[$r][$c] = $closest[0];
			}
		}
	}
	
	#print_grid(@grid);
	
	# All "edge" values are infinite
	my %edges = ();
	for (my $r = $bounds->ymin; $r <= $bounds->ymax; $r++) {
		for (my $c = $bounds->xmin; $c <= $bounds->xmax; $c++) {
			if ($r == $bounds->ymin || $r == $bounds->ymax || $c == $bounds->xmin || $c == $bounds->xmax) {
				$edges{$grid[$r][$c]} = 1;
			}
		}
	}
	
	# Count the non-infinite grid cells
	my %counts = ();
	for (my $r = $bounds->ymin; $r <= $bounds->ymax; $r++) {
		for (my $c = $bounds->xmin; $c <= $bounds->xmax; $c++) {
			my $i = $grid[$r][$c];
			if (!exists( $edges{$i} )) {
				$counts{$i} ++;
			}
		}
	}
	
	say "Part One:";
	say "The largest non-infinite area is " . max(values(%counts));
}

sub solve_part_two {
	my @points = @_;
	my $bounds = Bounds2D->new('xmin' => $points[0]->px, 'ymin' => $points[0]->py, 
								'xmax' => $points[0]->px, 'ymax' => $points[0]->py);
	for (my $i = 1; $i <= $#points; $i++) {
		$bounds->grow_to_fit($points[$i]);
	}
	
	my $count = 0;
	
	# Calculate the sum distance for all grid cells
	for (my $r = $bounds->ymin; $r <= $bounds->ymax; $r++) {
		for (my $c = $bounds->xmin; $c <= $bounds->xmax; $c++) {
			my $total = 0;
			my $pt = Point2D->new('px' => $c, 'py' => $r);
			for (my $i = 0; $i <= $#points; $i++) {
				# Removed Moose from tightest loop for 4x speed boost
				#my $line = Line2D->new('from' => $pt, 'to' => $points[$i]);
				#$total += $line->manhattan_distance();
				#
				$total += manhattan_distance($pt, $points[$i]);
			}
			$count++ if ($total < $LIMIT);			
		}
	}
	
	say "Part Two:";
	say "The number of cells with sum distance less than $LIMIT is " . $count;
}

sub print_grid {
	my @grid = @_;
	
	for my $row (@grid) {
		for my $col (@{$row}) {
			print $col;
		}
		print "\n";
	}
}

sub manhattan_distance {
	my ($p1, $p2) = @_;
	my $dx = $p1->px - $p2->px;
	my $dy = $p1->py - $p2->py;
	return abs($dx) + abs($dy);
}