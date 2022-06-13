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
my $INPUT_FILE = '<##>.test.txt';
#my $INPUT_FILE = '<##>.challenge.txt';
my @input = parse_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code <##>, Day <##>: <##>";

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
	
	return @content;
}

sub solve_part_one {
	my @input = @_;
}

sub solve_part_two {
	my @input = @_;
}
