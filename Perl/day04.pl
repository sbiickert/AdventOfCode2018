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

package ShiftRecord;
	use Moose;
	
	has 'ymd' => 			(is => 'ro', isa => 'Str');
	has 'guard' => 			(is => 'rw', isa => 'Str', default => 'Not set');
	has 'sleep_events' => 	(is => 'rw', isa => 'ArrayRef[Int]', default => sub { [] });
	has 'wake_events' => 	(is => 'rw', isa => 'ArrayRef[Int]', default => sub { [] });
	
	no Moose;
__PACKAGE__->meta->make_immutable;


package main;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = '04.test.txt';
my $INPUT_FILE = '04.challenge.txt';
my %input = parse_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2018, Day 04: Repose Record";

solve_part_one(%input);
solve_part_two(%input);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my %records;
	
	while (my $line = <$input>) {
		chomp $line;
		# line is [{yyyy-mm-dd} {hh}:{mm}] {event}
		$line =~ m/\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})] (.+)/;
		my $year = $1; my $month = $2; my $day = $3;
		my $hour = $4; my $minute = $5; my $event = $6;
		if ($hour eq '23') {
			# Guard came on shift before midnight. Previous day.
			#say $line;
			($month, $day) = increment_date($month, $day);
			#say "Incremented date is $month, $day";
		}
		my $ymd = join('-', ($year, $month, $day));
		if (!exists($records{$ymd})) {
			$records{$ymd} = ShiftRecord->new('ymd' => $ymd);
		}
		# Determine type of event
		if (index($event, 'Guard') == 0) {
			$event =~ m/#(\d+)/;
			$records{$ymd}->guard($1);
		}
		elsif (index($event, 'falls') == 0) {
			push(@{$records{$ymd}->sleep_events}, $minute+0);
		}
		elsif (index($event, 'wakes') == 0) {
			push(@{$records{$ymd}->wake_events}, $minute+0);
		}
	}
	
	close $input;
	
	# The wake and sleep events might be out of order. Sort them.
	for my $ymd (keys(%records)) {
		my $record = $records{$ymd};
		my @we = sort @{$record->wake_events};
		my @se = sort @{$record->sleep_events};
		#save the sorted arrays
		$record->wake_events(\@we);
		$record->sleep_events(\@se);
		$records{$ymd} = $record;
	}
	
	#print Dumper(\%records);
	return %records;
}

sub solve_part_one {
	my %input = @_;
	
	# Find guard that slept the most
	my %guard_tally = ();
	for my $ymd (keys(%input)) {
		my $record = $input{$ymd};
		my @we = @{$record->wake_events};
		my @se = @{$record->sleep_events};
		my $time_asleep = 0;
		for (my $i = 0; $i <= $#we; $i++) {
			$time_asleep += $we[$i] - $se[$i];
		}
		$guard_tally{$record->guard} += $time_asleep;
	}
	
	my $max_sleep = 0;
	my $sleepiest_guard = -1;
	for my $guard (keys(%guard_tally)) {
		if ($guard_tally{$guard} > $max_sleep) {
			$max_sleep = $guard_tally{$guard};
			$sleepiest_guard = $guard;
		}
	}
	say "Part One:";
	say "The sleepiest guard is $sleepiest_guard with $max_sleep minutes.";
	
	my @histogram = (0) x 60;
	for my $ymd (keys(%input)) {
		my $record = $input{$ymd};
		next if $record->guard ne $sleepiest_guard;
		my @we = @{$record->wake_events};
		my @se = @{$record->sleep_events};
		for (my $i = 0; $i <= $#we; $i++) {
			for (my $minute = $se[$i]; $minute < $we[$i]; $minute++) {
				$histogram[$minute]++;
			}
		}
	}
	
	# Find index with highest count
	my $sleepiest_minute = -1;
	my $max_count = 0;
	for (my $i = 0; $i < 60; $i++) {
		if ($histogram[$i] > $max_count) {
			$max_count = $histogram[$i];
			$sleepiest_minute = $i;
		}
	}
	
	say "The sleepiest time is minute $sleepiest_minute with $max_count times.";
	say "The guard ID x the minute is " . ($sleepiest_guard * $sleepiest_minute);
}

sub solve_part_two {
	my %input = @_;
	my %guard_records = ();
	
	for my $ymd (keys(%input)) {
		my $record = $input{$ymd};
		if (!exists($guard_records{$record->guard})) {
			$guard_records{$record->guard} = [];
		}
		push( @{$guard_records{$record->guard}}, $record );
	}
	
	my $sleepiest_guard = -1;
	my $max_count = 0;
	my $sleepiest_minute = -1;
	
	for my $guard (keys(%guard_records)) {
		my @histogram = (0) x 60;
		for my $record (@{$guard_records{$guard}}) {
			my @we = @{$record->wake_events};
			my @se = @{$record->sleep_events};
			for (my $i = 0; $i <= $#we; $i++) {
				for (my $minute = $se[$i]; $minute < $we[$i]; $minute++) {
					$histogram[$minute]++;
				}
			}
		}
	
		# Find index with highest count
		my $m = -1;
		my $max_count_this_guard = 0;
		for (my $i = 0; $i < 60; $i++) {
			if ($histogram[$i] > $max_count_this_guard) {
				$max_count_this_guard = $histogram[$i];
				$m = $i;
			}
		}
		
		if ($max_count_this_guard > $max_count) {
			$max_count = $max_count_this_guard;
			$sleepiest_minute = $m;
			$sleepiest_guard = $guard;
		}
	}

	say "Part Two:";
	say "The sleepiest guard is $sleepiest_guard.";
	say "The sleepiest time is minute $sleepiest_minute with $max_count times.";
	say "The guard ID x the minute is " . ($sleepiest_guard * $sleepiest_minute);
}

sub increment_date {
	my ($m, $d) = @_;
	my %max_dates = (1 => 31, 2 => 28, 3 => 31, 4 => 30, 5 => 31, 6 => 30,
					 7 => 31, 8 => 31, 9 => 30, 10 =>31, 11=> 30, 12=> 31);
	$d++;
	
	if ($d > $max_dates{$m+0}) {
		$m = sprintf('%02d', ++$m);
		$d = '01';
	}
	return ($m, $d);
}
