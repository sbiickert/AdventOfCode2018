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
#my $INPUT_FILE = '07.test.txt';
my $INPUT_FILE = '07.challenge.txt';
my @input = parse_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2018, Day 07: The Sum of Its Parts";

my %prereqs = init(@input);

solve_part_one(%prereqs);
solve_part_two(%prereqs);


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
	my %prereqs = @_;
	my @order = ();
	
	while (scalar(keys(%prereqs)) > 0) {
		my @ready_keys = find_keys_with_empty_list(\%prereqs);
		#say "Ready keys: " . join(',', @ready_keys);
		my $key = $ready_keys[0];
		push(@order, $key);
		delete $prereqs{$key};
		for my $other (keys(%prereqs)) {
			my @filtered = grep { $_ ne $key } @{$prereqs{$other}};
			$prereqs{$other} = \@filtered;
		}
		#print Dumper(\%prereqs);
	}
	
	say "Part One:";
	say "The order of instructions is " . join('', @order);
}

sub solve_part_two {
	my %prereqs = @_;
	
	my $base_time = 60;
	my $worker_count = 5;
	if ($INPUT_FILE =~ m/test/) {
		$base_time = 0;
		$worker_count = 2;
	}
	
	my %times = ();
	my $i = 1;
	for my $letter ('A' .. 'Z') {
		$times{$letter} = $base_time + $i;
		$i++;
	}
	
	my $tick = 0;
	my %workers = ('' => 1); # Dummy data to get in the loop.
	my @done_order = ();
	
	while (scalar(keys(%workers)) > 0) {
		# Workers make progress
		for my $letter (keys(%workers)) {
			$workers{$letter}--;
		}
		
		# Look for finished jobs
		for my $letter (keys(%workers)) {
			if ($workers{$letter} == 0) {
				delete $workers{$letter};
				delete $prereqs{$letter};
				push(@done_order, $letter);
				for my $other (keys(%prereqs)) {
					my @filtered = grep { $_ ne $letter } @{$prereqs{$other}};
					$prereqs{$other} = \@filtered;
				}
			}
		}
		
		# Refill workers
		my @ready_keys = find_keys_with_empty_list(\%prereqs);
		for my $letter (@ready_keys) {
			if (scalar(keys(%workers)) < $worker_count && !exists($workers{$letter})) {
				$workers{$letter} = $times{$letter};
			}
		}
		
		#print "$tick: \t";
		#for my $letter (sort keys(%workers)) {
		#	print "$letter $workers{$letter}  \t";
		#}
		#say join('', @done_order);
		
		$tick++;
	}
	
	$tick--; # Last $tick++ was spurious.
	
	say "Part Two:";
	say "The order that the instructions completed is " . join('', @done_order);
	say "The total time taken is $tick seconds.";
}

sub init {
	my (@input) = @_;
	my %result = ();
	
	for my $line (@input) {
		$line =~ m/Step ([A-Z]) [\w ]+ step ([A-Z])/;
		push(@{ $result{$2} }, $1);
		if (!exists($result{$1})) {
			$result{$1} = [];
		}
	}
	#print Dumper(\%result);
	return %result;
}

sub find_keys_with_empty_list {
	my $data_ref = shift;
	my %data = %{$data_ref};
	my @keys = ();
	
	for my $k (keys(%data)) {
		if (scalar @{ $data{$k} } == 0) {
			push(@keys, $k);
		}
	}
	return sort @keys;
}