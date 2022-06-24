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
#my $LIMIT = 9; #testing
my $LIMIT = 580741; #challenge input
my $TO_FIND = 580741;

say "Advent of Code 2018, Day 14: Chocolate Charts";

solve_part_one(@scoreboard);
solve_part_two(@scoreboard);


exit( 0 );

sub solve_part_one {
	my @scoreboard = @_;
	my @ptrs = (0,1);
	
	say "Part One:";
	#print_scoreboard(@ptrs, \@scoreboard);
	while (scalar(@scoreboard) < $LIMIT + 10) {
		make_recipes(\@scoreboard, \@ptrs);
		#print_scoreboard(@ptrs, \@scoreboard);
	}
	say "The 10 digits after $LIMIT are " . join('', splice(@scoreboard, $LIMIT, 10));
}

sub solve_part_two {
	my @scoreboard = @_;
	my @ptrs = (0,1);
	
	say "Part Two:";
	#print_scoreboard(@ptrs, \@scoreboard);
	
	my @search_for = split('', $TO_FIND);
	my $search_length = scalar(@search_for);
	
	my $found = 0;
	while (!$found) {
		make_recipes(\@scoreboard, \@ptrs);
		#print_scoreboard(@ptrs, \@scoreboard);
		$found = 1;
		my $offset = scalar(@scoreboard) - $search_length;
		for (my $i = 0; $i < $search_length; $i++) {
			if ($search_for[$i] != $scoreboard[$offset+$i]) {
				$found = 0;
				last;
			}
		}
	}
	my $temp = join('', @scoreboard);
	my $index = index($temp, $TO_FIND);
	say "There are $index digits before $TO_FIND.";
}

sub make_recipes {
	my ($sb, $ptrs) = @_;
	
	my $sum = $$sb[$$ptrs[0]] + $$sb[$$ptrs[1]];
	push(@$sb, split('', $sum));
	
	for (my $i = 0; $i <= $#$ptrs; $i++) {
		my $next = $$ptrs[$i] + 1 + $$sb[$$ptrs[$i]];
		while ($next >= scalar(@$sb)) {
			$next -= scalar(@$sb);
		}
		$$ptrs[$i] = $next;
	}
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