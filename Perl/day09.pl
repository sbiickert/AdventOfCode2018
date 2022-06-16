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
use List::Util qw(max);


package Marble;
	use Moose;
	
	our $_MARBLES = [];
	
	has 'value' =>	(is => 'ro', isa => 'Int');
	has 'cw' => 	(is => 'rw', isa =>'Int');
	has 'ccw' => 	(is => 'rw', isa =>'Int');
	has 'all_marbles' =>	(is => 'rw', isa => 'ArrayRef',
							 default => sub { return $_MARBLES }	);
	
	sub register {
		my $self = shift;
		$self->all_marbles->[$self->value] = $self;
	}
	
	sub insert_between {
		my ($self, $ccw, $cw) = @_;
		$self->ccw($ccw->value);
		$self->cw($cw->value);
		$ccw->cw($self->value);
		$cw->ccw($self->value);
	}
	
	sub remove {
		my $self = shift;
		my $ccw = $self->get_CCW();
		my $cw = $self->get_CW();
		$ccw->cw($self->cw);
		$cw->ccw($self->ccw);
	}
	
	sub get_CW {
		my $self = shift;
		return $_MARBLES->[$self->cw];
	}
	
	sub get_CCW {
		my $self = shift;
		return $_MARBLES->[$self->ccw];
	}

	no Moose;
__PACKAGE__->meta->make_immutable;

package main;

my @input = parse_input('challenge'); # 'test' for test input

say "Advent of Code 2018, Day 09: Marble Mania";

my $score;
for my $config (@input) {
	$score = solve_part(@{$config});
	say "Part One:";
	say "The winning elf's score is $score"; 
}

my @amplified = ($input[0]->[0], ($input[0]->[1])*100);
$score = solve_part(@amplified);
say "Part Two:";
say "The winning elf's score is $score"; 

exit( 0 );

sub parse_input {
	my $input_type = shift;
		
	my @content;
	
	if ($input_type eq 'test') {
		push(@content, [9, 25]);
		push(@content, [10, 1618]);
		push(@content, [13, 7999]);
		push(@content, [17, 1104]);
		push(@content, [21, 6111]);
		push(@content, [30, 5807]);
	}
	else {
		push(@content, [435, 71184]);
	}
	
	return @content;
}

sub solve_part {
	my ($player_count, $max_points) = @_;
	my $current_marble_value = 0;
	my $current_player = -1;
	my @scores = (0) x $player_count;
	
	my $marble = Marble->new('value' => $current_marble_value);
	$marble->register();
	$marble->insert_between($marble, $marble);
	my $current_marble = $marble;
	
	do {
		$current_player++;
		$current_player = 0 if ($current_player >= $player_count);
		$current_marble_value++;
		if ($current_marble_value % 23 != 0) {
			# Insert a new marble clockwise, skipping one.
			$current_marble = $current_marble->get_CW(); #skip
			my $ccw = $current_marble;
			my $cw = $current_marble->get_CW();
			$current_marble = Marble->new('value' => $current_marble_value);
			$current_marble->register();
			$current_marble->insert_between($ccw, $cw);
			#say "Inserted " . $current_marble->value . " between " . $ccw->value . " and " . $cw->value;
			#print_marbles($current_marble);
		}
		else {
			# Keep the marble, take the one 7 spots ccw
			$scores[$current_player] += $current_marble_value;
			for (my $i = 0; $i < 7; $i++) { $current_marble = $current_marble->get_CCW(); }
			my $next_current_marble = $current_marble->get_CW();
			$current_marble->remove();
			$scores[$current_player] += $current_marble->value;
			$current_marble = $next_current_marble;
		}
	} until $current_marble_value == $max_points;
	
	return max(@scores);
}

sub print_marbles {
	my $current_marble = shift;
	my $i = $current_marble;
	
	do {
		print $i->{'value'} . " ";
		$i = $i->get_CW();
	} until $i->value == $current_marble->value;
	print "\n";
}