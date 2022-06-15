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
use List::Util qw(sum);

my $INPUT_PATH = '../input';
#my $INPUT_FILE = '08.test.txt';
my $INPUT_FILE = '08.challenge.txt';
my @input = parse_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2018, Day 08: Memory Maneuver";

solve_part_one(@input);
#solve_part_two(@input);


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
	
	# input is a single line of space-delimited integers
	@content = split(' ', $content[0]);
	
	return @content;
}

sub solve_part_one {
	my @input = @_;
	my %root_node = parse_node(\@input);
	#print Dumper(\%root_node);
	my $sum = sum_metadata(\%root_node);
	
	say "Part One:";
	say "The sum of the metadata is $sum";
}

sub solve_part_two {
	my @input = @_;
}

sub parse_node {
	my $l_ref = shift;
	my %node = ('children' => [], 'metadata' => []);
	my $num_children = shift @{ $l_ref };
	my $num_metadata = shift @{ $l_ref };
	
	for (my $i = 0; $i < $num_children; $i++) {
		my %child = parse_node($l_ref);
		push(@{ $node{'children'} }, \%child);
	}
	
	for (my $i = 0; $i < $num_metadata; $i++) {
		push(@{ $node{'metadata'} }, shift @{ $l_ref });
	}
	
	return %node;
}

sub sum_metadata {
	my $n_ref = shift;
	my %n = %{$n_ref};
	my @m = @{ $n{'metadata'} };
	my $sum = sum(@m);
	my @c = @{ $n{'children'} };
	for my $child (@c) {
		$sum += sum_metadata($child);
	}
	return $sum;
}