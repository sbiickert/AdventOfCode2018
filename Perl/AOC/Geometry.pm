#!/usr/bin/env perl

package AOC::Geometry;
use Modern::Perl 2018;

package Point2D;
	use Moose;
	
	has 'px' => (is => 'rw', isa => 'Int');
	has 'py' => (is => 'rw', isa => 'Int');
	
	sub move {
		my ($self, $dx, $dy) = @_;
		$self->px( $self->px + $dx );
		$self->py( $self->py + $dy );
	}
	
	sub debugStr {
		my $self = shift;
		return "P2D x: " . $self->px . " y: " . $self->py;
	}
	
	no Moose;
__PACKAGE__->meta->make_immutable;

package Line2D;
	use Moose;
	
	has 'from' => 	(is => 'rw', isa => 'Point2D');
	has 'to' => 	(is => 'rw', isa => 'Point2D');
	
	sub dx {
		my $self = shift;
		return $self->to->px - $self->from->px;
	}
	
	sub dy {
		my $self = shift;
		return $self->to->py - $self->from->py;
	}
	
	sub manhattan_distance {
		my $self = shift;
		return abs($self->dx) + abs($self->dy);
	}

	no Moose;
__PACKAGE__->meta->make_immutable;
	
package Point3D;
	use Moose;
	
	extends 'Point2D';
	
	has 'pz' => (is => 'rw', isa => 'Int');
	
	sub dz {
		my $self = shift;
		return $self->to->pz - $self->from->pz;
	}
	
	no Moose;
__PACKAGE__->meta->make_immutable;

package Line3D;
	use Moose;
	
	has 'from' => 	(is => 'rw', isa => 'Point3D');
	has 'to' => 	(is => 'rw', isa => 'Point3D');
	
	sub dx {
		my $self = shift;
		return $self->to->px - $self->from->px;
	}
	
	sub dy {
		my $self = shift;
		return $self->to->py - $self->from->py;
	}
	
	sub dz {
		my $self = shift;
		return $self->to->pz - $self->from->pz;
	}

	no Moose;
__PACKAGE__->meta->make_immutable;
	
1;