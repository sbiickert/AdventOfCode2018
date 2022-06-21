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
use AOC::Geometry qw(Point2D);

package MineCart;
	use Moose;
	
	has 'pos' => 		(is => 'rw', isa => 'Point2D');
	has 'dir' => 		(is => 'rw', isa => 'Str'); 				# <>v^
	has 'last_turn' => 	(is => 'rw', isa => 'Str', default => 'R'); # LFR
	
	sub move {
		my ($self, $track) = @_;
		if ($track eq '\\') {
			if ($self->dir eq '>' || $self->dir eq '<') 	{ $self->turn_right(); }
			else 											{ $self->turn_left(); }
		}
		elsif ($track eq '/') {
			if ($self->dir eq '^' || $self->dir eq 'v') 	{ $self->turn_right(); }
			else 											{ $self->turn_left(); }
		}
		elsif	($track eq '+') {
			$self->choose_dir();
		}
		$self->move_forward();
	}
	
	sub move_forward {
		my $self = shift;
		if 		($self->dir eq '^')	{	$self->pos->py($self->pos->py - 1);	}
		elsif 	($self->dir eq '<')	{	$self->pos->px($self->pos->px - 1);	}
		elsif 	($self->dir eq 'v')	{	$self->pos->py($self->pos->py + 1);	}
		elsif 	($self->dir eq '>')	{	$self->pos->px($self->pos->px + 1);	}
	}
	
	sub choose_dir {
		my $self = shift;
		if ($self->last_turn eq 'L') {
			$self->last_turn('F');
		}
		elsif ($self->last_turn eq 'F') {
			$self->turn_right();
			$self->last_turn('R');
		}
		elsif ($self->last_turn eq 'R') {
			$self->turn_left();
			$self->last_turn('L');	
		}
	}
	
	sub turn_left {
		my $self = shift;
		if 		($self->dir eq '^')	{	$self->dir('<');	}
		elsif 	($self->dir eq '<')	{	$self->dir('v');	}
		elsif 	($self->dir eq 'v')	{	$self->dir('>');	}
		elsif 	($self->dir eq '>')	{	$self->dir('^');	}
	}
	
	sub turn_right {
		my $self = shift;
		if 		($self->dir eq '^')	{	$self->dir('>');	}
		elsif 	($self->dir eq '<')	{	$self->dir('^');	}
		elsif 	($self->dir eq 'v')	{	$self->dir('<');	}
		elsif 	($self->dir eq '>')	{	$self->dir('v');	}
	}
	
	no Moose;
__PACKAGE__->meta->make_immutable;


package main;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = '13.test1.txt';
#my $INPUT_FILE = '13.test2.txt';
my $INPUT_FILE = '13.challenge.txt';
my @MAP = parse_input("$INPUT_PATH/$INPUT_FILE");
my @carts = @{shift(@MAP)};

say "Advent of Code 2018, Day 13: Mine Cart Madness";

#print_map(\@carts);

solve_part_one(@carts);
#solve_part_two(@input);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @map;
	my @carts;
	
	my $row = 0;
	while (my $line = <$input>) {
		chomp $line;
		my @chars = split(//, $line);
		for (my $col = 0; $col <= $#chars; $col++) {
			if ($chars[$col] =~ m/[\\\/\-\|\+ ]/) {
				$map[$row][$col] = $chars[$col];
			}
			else {
				# this is a cart position
				my $cart = MineCart->new('pos' => Point2D->new('px' => $col, 'py' => $row), 'dir' => $chars[$col]);
				if 		($chars[$col] eq 'v' || $chars[$col] eq '^')	{
					$map[$row][$col] =  '|';
				}
				elsif 	($chars[$col] eq '<' || $chars[$col] eq '>')	{
					$map[$row][$col] =  '-';
				}
				push(@carts, $cart);
			}
		}
		$row++;
	}
	
	close $input;
	
	return (\@carts, @map);
}

sub solve_part_one {
	my @carts = @_;
	my $collision_pos;
	
	while (!$collision_pos) {
		# Sort carts by rows and cols
		my @cart_locations;
		for my $cart (@carts) { $cart_locations[$cart->pos->py][$cart->pos->px] = $cart; }
		
		# Move carts
		for (my $r = 0; $r <= $#MAP; $r++) {
			my @row = @{$MAP[$r]};
			for (my $c = 0; $c <= $#row; $c++) {
				if ($cart_locations[$r][$c]) {
					# Move cart
					my $cart = $cart_locations[$r][$c];
					$cart->move($MAP[$r][$c]);
	
					$collision_pos = find_collision(\@carts);
					if ($collision_pos) { last; }
				}
			}
		}
		#print_map(\@carts);
	}
	
	say "Part One:";
	say "There was a collision at " . $collision_pos->debugStr;
}

sub solve_part_two {
	my @input = @_;
}

sub find_collision {
	my $carts_ref = shift;
	my @carts = @{$carts_ref};
	my %cart_locations = ();
	for my $cart (@carts) {
		my $key = $cart->pos->px . ',' . $cart->pos->py;
		if (!exists($cart_locations{$key})) {
			$cart_locations{$key} = $cart->pos;
		}
		else {
			return $cart->pos;
		}
	}
	return 0;
}

sub print_map {
	my $carts_ref = shift;
	my @carts = @{$carts_ref};
	
	# Note the cart locations
	my %cart_locations = ();
	for my $cart (@carts) {
		$cart_locations{$cart->pos->px . ',' . $cart->pos->py} = $cart->dir;
	}
	
	for (my $r = 0; $r <= $#MAP; $r++) {
		my @row = @{$MAP[$r]};
		for (my $c = 0; $c <= $#row; $c++) {
			if (!exists($cart_locations{$c . ',' . $r})) {
				print $MAP[$r][$c];
			}
			else {
				print $cart_locations{$c . ',' . $r}
			}
		}
		print "\n";
	}
}