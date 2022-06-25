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
#my $LIMIT = 2018; #testing
my $LIMIT = 580741; #challenge input
my $TO_FIND = 580741;

say "Advent of Code 2018, Day 14: Chocolate Charts";

my $ELF1 = 0;
my $ELF2 = 1;

solve_part_one(@scoreboard);
solve_part_two(@scoreboard);


exit( 0 );

sub solve_part_one {
	my @scoreboard = @_;
	$ELF1 = 0; $ELF2 = 1;
	
	say "Part One:";
	#print_scoreboard(\@scoreboard);
	while (scalar(@scoreboard) < $LIMIT + 10) {
		make_recipes(\@scoreboard);
		#print_scoreboard(\@scoreboard);
	}
	say "The 10 digits after $LIMIT are " . join('', splice(@scoreboard, $LIMIT, 10));
}

sub solve_part_two {
	my @scoreboard = @_;
	$ELF1 = 0; $ELF2 = 1;
	
	say "Part Two:";
	#print_scoreboard(\@scoreboard);
	
	my @search_for = split('', $TO_FIND);
	my $search_length = scalar(@search_for);
	
	my $found = 0;
	while (!$found) {
		make_recipes(\@scoreboard);
		#print_scoreboard(\@scoreboard);
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
	my $sb = shift;
	
	my $sum = $$sb[$ELF1] + $$sb[$ELF2];
	push(@$sb, split('', $sum));
	
	my $sb_length = scalar(@$sb); 
	my $next;
	
	$next = $ELF1 + 1 + $$sb[$ELF1];
	while ($next >= $sb_length) {
		$next -= $sb_length;
	}
	$ELF1 = $next;
	
	$next = $ELF2 + 1 + $$sb[$ELF2];
	while ($next >= $sb_length) {
		$next -= $sb_length;
	}
	$ELF2 = $next;
}

sub print_scoreboard {
	my ($sbref) = @_;
	my @sb = @{$sbref};
	
	my @temp = ();
	for (my $i = 0; $i <= $#sb; $i++) {
		if ($i == $ELF1) {
			push(@temp, "($sb[$i])");
		}
		elsif ($i == $ELF2) {
			push(@temp, "[$sb[$i]]");
		}
		else {
			push(@temp, $sb[$i]);
		}
	}
	say join(' ', @temp);
}