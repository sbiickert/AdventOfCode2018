//
//  AOCDay01.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-13.
//

#import <Foundation/Foundation.h>
#import "AOCDay.h"
#import "AOCStrings.h"

@implementation AOCDay01

- (AOCDay01 *)init {
	self = [super initWithDay:1 name:@"Chronal Calibration"];
	return self;
}

- (struct AOCResult)solveInputIndex:(int)index inFile:(NSString *)filename {
	struct AOCResult result = [super solveInputIndex:index inFile:filename];
	
	NSArray<NSString *> *input = [AOCInput readGroupedInputFile:filename atIndex:index];
	
	result.part1 = [self solvePartOne: input];
	result.part2 = [self solvePartTwo: input];
	
	return result;
}

- (NSString *)solvePartOne:(NSArray<NSString *> *)input {
	// Each line is +N or -N
	NSInteger value = 0;
	for (NSString *line in input) {
		NSInteger n = line.integerValue;
		value += n;
	}
	
	return [NSString stringWithFormat:@"The resulting frequency is %ld.", value];
}

- (NSString *)solvePartTwo:(NSArray<NSString *> *)input {
	NSMutableSet<NSNumber *> *values = [NSMutableSet set];
	[values addObject:@0];
	
	NSInteger i = 0;
	NSInteger value = 0;
	
	while (YES) {
		NSInteger n = input[i].integerValue;
		value += n;
		NSNumber *vObj = [NSNumber numberWithInteger:value];
		if ([values containsObject:vObj]) {
			break;
		}
		[values addObject:vObj];
		i = (i + 1) % input.count;
	}
	
	return [NSString stringWithFormat:@"The first repeating frequency is %ld.", value];
}

@end
