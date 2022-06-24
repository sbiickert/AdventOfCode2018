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

my @scoreboard = (3,7);
#my $LIMIT = 2018;
my $LIMIT = 580741;

say "Advent of Code 2018, Day 14: Chocolate Charts";

solve_part_one(@scoreboard);
#solve_part_two(@input);


exit( 0 );

sub solve_part_one {
	my @scoreboard = @_;
	my @ptrs = (0,1);
	
	say "Part One:";
	#print_scoreboard(@ptrs, \@scoreboard);
	
	while (scalar(@scoreboard) < $LIMIT + 10) {
		my $sum = $scoreboard[$ptrs[0]] + $scoreboard[$ptrs[1]];
		push(@scoreboard, split('', $sum));
		
		for (my $i = 0; $i <= $#ptrs; $i++) {
			my $next = $ptrs[$i] + 1 + $scoreboard[$ptrs[$i]];
			while ($next >= scalar(@scoreboard)) {
				$next -= scalar(@scoreboard);
			}
			$ptrs[$i] = $next;
		}
		#print_scoreboard(@ptrs, \@scoreboard);
	}
	say "The 10 digits after $LIMIT are " . join('', splice(@scoreboard, $LIMIT, 10));
}

sub solve_part_two {
	my @input = @_;
}

sub print_scoreboard {
	my ($ptr1, $ptr2, $sbref) = @_;
	my @sb = @{$sbref};
	
	my @temp = ();
	for (my $i = 0; $i <= $#sb; $i++) {
		if ($i == $ptr1) {
			push(@temp, "($sb[$i])");
		}
		elsif ($i == $ptr2) {
			push(@temp, "[$sb[$i]]");
		}
		else {
			push(@temp, $sb[$i]);
		}
	}
	say join(' ', @temp);
}