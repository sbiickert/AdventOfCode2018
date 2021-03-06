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
use AOC::Grid qw(Grid2D);


package Battle;
	use Moose;

	has 'grid' => 	('is' => 'rw', 'isa' => 'Grid2D');
	has 'turns' =>	('is' => 'rw', 'isa' => 'Int', 'default' => 0);
	has 'end_grid' =>	('is' => 'ro', 'isa' => 'Grid2D');
	has 'end_turns' =>	('is' => 'ro', 'isa' => 'Int');
	has 'end_hp' => 	('is' => 'ro', 'isa' => 'Int');
	has 'end_score' => 	('is' => 'ro', 'isa' => 'Int');

	sub get_hp {
		my $self = shift;
		my $hp = 0;

		for my $r (0..$self->grid->height-1) {
			for my $c (0..$self->grid->width-1) {
				my $value = $self->grid->get($r, $c);
				if ($value =~ m/([EG])(\d+)/) {
					$hp += $2;
				}
			}
		}
		return $hp;
	}

	sub get_score {
		my $self = shift;
		my $hp = $self->get_hp();
		return $hp * $self->turns;
	}

	sub has_known_end_state {
		my $self = shift;
		return $self->end_turns > 0;
	}

	sub is_finished {
		my $self = shift;
		my $elf_count = 0;
		my $goblin_count = 0;
		for my $c ($self->grid->coords) {
			my $value = substr($self->grid->get(@$c), 0, 1);
			$elf_count++ if $value eq 'E';
			$goblin_count++ if $value eq 'G';
		}
		return ($elf_count xor $goblin_count);
	}

	sub init_hp {
		my ($self, $hp) = @_;
		for (my $r = 0; $r < $self->grid->height; $r++) {
			for (my $c = 0; $c < $self->grid->width; $c++) {
				my $chr = $self->grid->get($r, $c);
				$self->grid->set($chr . $hp, $r, $c) if ($chr eq 'E' or $chr eq 'G');
			}
		}
	}

	sub get_coords {
		my ($self, $class) = @_;
		my @coords;

		for my $r (0..$self->grid->height-1) {
			for my $c (0..$self->grid->width-1) {
				my $value = $self->grid->get($r, $c);
				if ($value =~ m/[$class]\d+/) {
					push(@coords, $r . ',' . $c);
				}
			}
		}
		return @coords;
	}

	sub get_class_at {
		my ($self, $coord) = @_;
		my ($r, $c) = split(',', $coord);
		my $grid_value = $self->grid->get($r, $c);
		return substr($grid_value, 0, 1);
	}

	sub get_attack_spaces {
		my ($self, $attacked_class) = @_;
		my @attack_coords;
		my @coords = $self->get_coords($attacked_class);
		for my $coord (@coords) {
			my @neighbors = $self->grid->neighbor_coords(split(',', $coord));
			for my $n (@neighbors) {
				my $value = $self->grid->get($n->[0], $n->[1]);
				push(@attack_coords, $n->[0] . ',' .  $n->[1]) if $value eq ".";
			}
		}
		return sort {
			my ($ar, $ac) = split(',', $a);
			my ($br, $bc) = split(',', $b);
			if ($ar == $br) { return $ac <=> $bc; }
			return $ar <=> $br;
		} @attack_coords;
	}

	sub cost_to_reach {
		my ($self, $from, $to) = @_;
		return 0;
	}

	sub do_turn {
		my ($self, $coord) = @_;
		# The fighter whose turn it is is at $coord
		my $fighter_class = $self->get_class_at($coord);
		die("Found $fighter_class at $coord when expecting E or G") 
			unless $fighter_class =~ m/[EG]/;
		my $enemy_class = ($fighter_class eq "G") ? "E" : "G";
		my @attack_spaces = $self->get_attack_spaces($enemy_class);

		my %costs;
		for my $a_coord (@attack_spaces) {
			say "Fighter at $coord can attack at $a_coord";
			my $cost = $self->cost_to_reach($coord, $a_coord);
			if ($cost >= 0) {
				say "And it's reachable with cost $cost.";
				if (!exists($costs{$cost}))
				{
					$costs{$cost} = $a_coord;
				}
			}
		}
		# If we're not next to a target
			# Need to find all targets
			# Find all reachable points next to targets
			# Find nearest point
			# Break ties with reading order
			# Move towards it
		# Next to target?
			# Attack` 
	}	

	sub draw {
		my $self = shift;
		say "Turns: " . $self->turns . " HP: " . $self->get_hp() . " Result: " . $self->get_score();
		for my $r (0..$self->grid->height-1) {
			my @fighters = ();
			for my $c (0..$self->grid->width-1) {
				my $value = $self->grid->get($r, $c);
				if ($value =~ m/([EG])(\d+)/) {
					push(@fighters, $1 . '(' . $2 . ')');
				}
				print substr($value, 0, 1);
			}
			print "   " . join(', ', @fighters);
			print "\n";
		}
		print "\n";
	}
	no Moose;
__PACKAGE__->meta->make_immutable;


package main;

my $INPUT_PATH = '../input';
my $INPUT_FILE = '15.test.txt';
#my $INPUT_FILE = '15.challenge.txt';
my @battles = parse_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2018, Day 15: Beverage Bandits";

for my $battle (@battles) {
	solve_part_one($battle);
}
#solve_part_two(@battles);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @battles;
	my @lines;

	while (my $line = <$input>) {
		chomp $line;
		push(@lines, $line) if (length($line) > 0);
		if (length($line) == 0) {
			push(@battles, parse_battle(@lines));
			@lines = ();
		}
	}
	if (scalar(@lines > 0)) {
		push(@battles, parse_battle(@lines));
	}
	
	close $input;
	
	return @battles;
}

sub parse_battle {
	my @input = @_;
	my $size = scalar(@input);
	my $start = Grid2D->new('width' => $size, 'height' => $size, 'default' => '#');
	my $end = Grid2D->new('width' => $size, 'height' => $size, 'default' => '#');
	my $hp = 0;
	my $turns = 0;

	for (my $r = 0; $r < $size; $r++) {
		my $line = $input[$r];
		my @data = split(/ +/, $line);
		# @data[0] is the line of the input battle map
		# @data[1] (if present) is the line of the output battle map
		# @data[2...] (if present) is the output hp and score
		my @chrs = split(//, $data[0]);
		for (my $c = 0; $c < $size; $c++) {
			$start->set($chrs[$c], $r, $c);
		}
		if (scalar(@data) > 1) {
			# The predicted end state (test only)
			@chrs = split(//, $data[1]);
			for (my $c = 0; $c < $size; $c++) {
				$end->set($chrs[$c], $r, $c);
			}
			# The predicted hit points and/or outcome
			if ($r == $size - 1) {
				# Outcome
				$turns = $data[3];
				$hp = $data[5];
			}
		}
	}

	my $battle = Battle->new('grid' => $start, 'end_grid' => $end, 
							'end_turns' => $turns, 'end_hp' => $hp, 
							'end_score' => $hp * $turns);
	$battle->init_hp(200);
	#$battle->draw();
	return $battle;
}

sub solve_part_one {
	my $battle = shift;

	while (!$battle->is_finished()) {
		$battle->draw();
		my @fighters = $battle->get_coords("EG");
		for my $fighter (@fighters) {
			say "Fighter at $fighter is beginning its turn.";
			$battle->do_turn($fighter);
		}
		die;
	}
}

sub solve_part_two {
	my @input = @_;
}
