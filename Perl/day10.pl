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
use AOC::Geometry qw(Point2D Bounds2D);
use List::Util qw (min max);

package SkyLight;
	use Moose;
	
	has 'pos' => 	(is => 'rw', isa => 'Point2D');
	has 'vel' => 	(is => 'ro', isa => 'Point2D');
	
	sub moved_light {
		my $self = shift;
		# Don't clone objects. Slow. 
		#my $new_pos = $self->pos->meta->clone_object($self->pos);
		# Making the new and move a single line saved 5 seconds.
		my $new_pos = Point2D->new(
			'px' => $self->pos->px + $self->vel->px,
			'py' => $self->pos->py + $self->vel->py);
		#$new_pos->move($self->vel->px, $self->vel->py);
		return SkyLight->new('pos' => $new_pos, 'vel' => $self->vel);
	}
	
	no Moose;
__PACKAGE__->meta->make_immutable;


package main;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = '10.test.txt';
my $INPUT_FILE = '10.challenge.txt';
my @input = parse_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2018, Day 10: The Stars Align";

solve(@input);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @content;
	
	while (my $line = <$input>) {
		chomp $line;
		$line =~ m/< ?([-\d]+),  ?([-\d]+)>.+< ?([-\d]+),  ?([-\d]+)>/;
		my $pos = Point2D->new('px' => $1, 'py' => $2);
		my $vel = Point2D->new('px' => $3, 'py' => $4);
		
		push(@content, SkyLight->new('pos' => $pos, 'vel' => $vel));
	}
	
	close $input;
	return @content;
}

sub solve {
	my @lights = @_;
	#print_skylights(\@lights);
	my $bounds_grew = 0;
	my $i = 0;
	
	my $bounds = calc_bounds(\@lights);
	my $size = $bounds->size();
		
	while (1) {
		my @moved_lights = ();
		for my $light (@lights) {
			my $moved = $light->moved_light();
			push(@moved_lights, $moved);
		}
		
		my $moved_bounds = calc_bounds(\@moved_lights);
		my $moved_size = $moved_bounds->size();
		
		$bounds_grew = ($moved_size->px > $size->px) || ($moved_size->py > $size->py);
		#print_skylights(\@moved_lights);
		
		last if ($bounds_grew);
		
		@lights = @moved_lights;
		$bounds = $moved_bounds;
		$size = $moved_size;
		
		$i++;# say $i;
	}
	
	say "Part One:";
	say "The message is: ";
	print_skylights(\@lights);
	
	say "Part Two:";
	say "That took $i seconds.";
}

sub calc_bounds_growing {
	my $lights_ref = shift;
	my @lights = @{$lights_ref};
	my $bounds = Bounds2D->new();
	$bounds->shrink_to_fit($lights[0]->pos);
	
	for my $light (@lights) {
		$bounds->grow_to_fit($light->pos);
	}
	return $bounds;
}

# This is faster than the "growing" method
sub calc_bounds {
	my $lights_ref = shift;
	my @lights = @{$lights_ref};
	
	my @points = ();
	for my $light (@lights) {
		push(@points, $light->pos);
	}
	
	my $bounds = Bounds2D->new();
	$bounds->fit_to_points(@points);
	
	return $bounds;
}

sub print_skylights {
	my $lights_ref = shift;
	my @lights = @{$lights_ref};
	my %coords = ();
	my $bounds = Bounds2D->new();
	$bounds->shrink_to_fit($lights[0]->pos);
	
	for my $light (@lights) {
		$bounds->grow_to_fit($light->pos);
		$coords{$light->pos->px . ',' . $light->pos->py} = 1;
	}
	
	for (my $y = $bounds->ymin; $y <= $bounds->ymax; $y++) {
		for (my $x = $bounds->xmin; $x <= $bounds->xmax; $x++) {
			if (exists($coords{"$x,$y"})) 	{ print '#'; }
			else							{ print '.'; }
		}
		print "\n";
	}
}