//
//  AOCDay04.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-13.
//

#import <Foundation/Foundation.h>
#import "AOCDay.h"
#import "AOCStrings.h"
#import "AOCArrays.h"

@interface ReposeMsg : NSObject

@property NSDate *timestamp;
@property NSString *message;
@property NSString *guard;

- (ReposeMsg *)init:(NSDate *)date message:(NSString *)msg;

@end

@implementation AOCDay04

- (AOCDay04 *)init {
	self = [super initWithDay:04 name:@"Repose Record"];
	return self;
}

- (struct AOCResult)solveInputIndex:(int)index inFile:(NSString *)filename {
	struct AOCResult result = [super solveInputIndex:index inFile:filename];
	
	NSArray<NSString *> *input = [AOCInput readGroupedInputFile:filename atIndex:index];
	
	NSArray<ReposeMsg *> *messages = [self sortInputByTime:input];
	NSDictionary<NSString *, NSMutableArray<NSDateInterval*> *> *guardSleeps = [self buildSleepIntervals:messages];
	
	result.part1 = [self solvePartOne: guardSleeps];
	result.part2 = [self solvePartTwo: guardSleeps];
	
	return result;
}

- (NSString *)solvePartOne:(NSDictionary<NSString *, NSMutableArray<NSDateInterval*> *> *)guardSleeps {
	NSInteger mostSleep = 0;
	NSString *sleepiestGuard = nil;
	for (NSString *guard in guardSleeps.allKeys) {
		NSInteger sum = 0;
		for (NSDateInterval *interval in guardSleeps[guard]) {
			sum += interval.duration;
		}
		sum /= 60;
		//NSLog(@"Guard %@ slept for %ld minutes", guard, sum);
		if (sum > mostSleep) {
			mostSleep = sum;
			sleepiestGuard = guard;
		}
	}
	
	NSArray<NSNumber *> *counts = [self buildSleepHistogram:guardSleeps[sleepiestGuard]];
	
	NSInteger maxMinute = 0;
	NSInteger maxSleepCount = 0;
	
	for (NSInteger i = 0; i < 60; i++) {
		NSInteger count = [counts[i] integerValue];
		if (count > maxSleepCount) {
			maxMinute = i;
			maxSleepCount = count;
		}
	}
	
	NSInteger result = [sleepiestGuard integerValue] * maxMinute;
	
	return [NSString stringWithFormat: @"The sleepiest guard was %@, asleep most at minute %ld for a result of %ld", sleepiestGuard, maxMinute, result];
}

- (NSString *)solvePartTwo:(NSDictionary<NSString *, NSMutableArray<NSDateInterval*> *> *)guardSleeps {
	NSInteger maxMinute = 0;
	NSInteger maxSleepCount = 0;
	NSString *chosenGuard = nil;

	for (NSString *guard in guardSleeps.allKeys) {
		NSArray<NSNumber *> *counts = [self buildSleepHistogram:guardSleeps[guard]];
		for (NSInteger i = 0; i < counts.count; i++) {
			NSInteger count = [counts[i] integerValue];
			if (count > maxSleepCount) {
				maxMinute = i;
				maxSleepCount = count;
				chosenGuard = guard;
			}
		}
	}
	
	NSInteger result = [chosenGuard integerValue] * maxMinute;
	
	return [NSString stringWithFormat: @"The chosen guard was %@, asleep %ld times at minute %ld for a result of %ld",
			chosenGuard, maxSleepCount, maxMinute, result];
}

- (NSArray<ReposeMsg *> *)sortInputByTime:(NSArray<NSString *> *)input {
	NSDateFormatter *df = [[NSDateFormatter alloc] init];
	[df setDateFormat:@"yy-MM-dd hh:mm"];
	
	NSMutableArray<ReposeMsg *> *messages = [NSMutableArray array];
	
	for (NSString *line in input) {
		NSArray<NSString *> *m = [line matchPattern:@"\\[15([^]]+)\\] (.+)"];
		NSDate *date = [df dateFromString:m[1]];
		//NSLog(@"%@ --> %@", m[1], date);
		ReposeMsg *rm = [[ReposeMsg alloc] init:date message:m[2]];
		[messages addObject:rm];
		//NSLog(@"%@", rm);
	}
	
	NSSortDescriptor *sd = [NSSortDescriptor sortDescriptorWithKey:@"self.timestamp" ascending:YES];
	[messages sortUsingDescriptors:[NSArray arrayWithObject:sd]];

	NSString *currentGuard = nil;
	for (ReposeMsg *msg in messages) {
		NSArray<NSString *> *m = [msg.message matchPattern:@"Guard #(\\d+)"];
		if (m) {
			currentGuard = m[1];
		}
		msg.guard = currentGuard;
		//NSLog(@"%@", msg);
	}
	
	return messages;
}

- (NSDictionary<NSString *, NSMutableArray<NSDateInterval*> *> *)buildSleepIntervals:(NSArray<ReposeMsg *> *)messages {
	NSMutableDictionary<NSString *, NSMutableArray<NSDateInterval*> *> *guardSleeps = [NSMutableDictionary dictionary];
	for (NSInteger i = 0; i < messages.count; i++) {
		if (guardSleeps[messages[i].guard] == nil) {
			guardSleeps[messages[i].guard] = [NSMutableArray array];
		}
		if ([messages[i].message containsString:@"wakes"]) {
			NSDateInterval *interval = [[NSDateInterval alloc] initWithStartDate:messages[i-1].timestamp
																		 endDate:messages[i].timestamp];
			[guardSleeps[messages[i].guard] addObject:interval];
		}
	}
	return guardSleeps;
}

- (NSArray<NSNumber*> *)buildSleepHistogram:(NSArray<NSDateInterval*> *)intervals {
	NSCalendar *cal = [NSCalendar currentCalendar];
	NSMutableArray<NSNumber *> *counts = [NSMutableArray array];
	for (int i = 0; i < 60; i++) { [counts addObject:@0]; }
	
	for (NSDateInterval *di in intervals) {
		NSDateComponents *dcStart = [cal components:(NSCalendarUnitMinute) fromDate:di.startDate];
		NSInteger startMin = dcStart.minute;
		NSDateComponents *dcEnd = [cal components:(NSCalendarUnitMinute) fromDate:di.endDate];
		NSInteger endMin = dcEnd.minute;
		for (NSInteger i = startMin; i < endMin; i++) {
			[AOCArrayUtil increment:counts at:i];
		}
	}
	return counts;
}

@end

@implementation ReposeMsg

- (ReposeMsg *)init:(NSDate *)date message:(NSString *)msg {
	self = [super init];
	
	_timestamp = date;
	_message = msg;
	_guard = nil;
	
	return self;
}

- (NSString *)description {
	return self.debugDescription;
}
- (NSString *)debugDescription {
	return [NSString stringWithFormat:@"[%@] %@ %@", self.timestamp, self.guard, self.message];
}

@end
