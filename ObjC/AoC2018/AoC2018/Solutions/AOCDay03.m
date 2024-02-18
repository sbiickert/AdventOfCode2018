//
//  AOCDay03.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-13.
//

#import <Foundation/Foundation.h>
#import "AOCDay.h"
#import "AOCStrings.h"
#import "AOCArrays.h"
#import "AOCSpatial.h"

@implementation AOCDay03

- (AOCDay03 *)init {
	self = [super initWithDay:03 name:@"No Matter How You Slice It"];
	return self;
}

- (struct AOCResult)solveInputIndex:(int)index inFile:(NSString *)filename {
	struct AOCResult result = [super solveInputIndex:index inFile:filename];
	
	NSArray<NSString *> *input = [AOCInput readGroupedInputFile:filename atIndex:index];
	
	NSArray<AOCExtent *> *extents = [self parseExtents:input];
	
	result.part1 = [self solvePartOne: extents];
	result.part2 = [self solvePartTwo: extents];
	
	return result;
}

- (NSString *)solvePartOne:(NSArray<AOCExtent *> *)extents {
	NSMutableArray *overlaps = [NSMutableArray array];
	
	for (NSInteger i = 0; i < extents.count-1; i++) {
		for (NSInteger j = i+1; j < extents.count; j++) {
			AOCExtent *intersection = [extents[i] intersectWith:extents[j]];
			if (intersection != nil) {
				[overlaps addObject:intersection];
			}
		}
	}
	
	NSMutableSet<AOCCoord *> *allOverlappingCoords = [NSMutableSet set];
	for (AOCExtent *ext in overlaps) {
		[allOverlappingCoords addObjectsFromArray:ext.allCoords];
	}

	return [NSString stringWithFormat: @"The sum of overlaps is %ld", allOverlappingCoords.count];
}

- (NSString *)solvePartTwo:(NSArray<AOCExtent *> *)extents {
	NSInteger idWithNoOverlaps = -1;
	
	for (NSInteger i = 0; i < extents.count; i++) {
		BOOL overlaps = NO;
		for (NSInteger j = 0; j < extents.count; j++) {
			if (i == j) { continue; }
			AOCExtent *intersection = [extents[i] intersectWith:extents[j]];
			if (intersection != nil) {
				overlaps = YES;
				break;
			}
		}
		if (overlaps == NO) {
			idWithNoOverlaps = i+1;
			break;
		}
	}

	return [NSString stringWithFormat: @"The ID of the claim with no overlaps is %ld", idWithNoOverlaps];
}

- (NSArray<AOCExtent *> *)parseExtents:(NSArray<NSString *> *)input {
	NSMutableArray<AOCExtent *> *extents = [NSMutableArray array];
	for (NSString *line in input) {
		NSMutableArray<NSString *> *m = [[line matchPattern:@"(\\d+),(\\d+): (\\d+)x(\\d+)" caseSensitive:YES] mutableCopy];
		[m removeObjectAtIndex:0];
		NSArray<NSNumber *> *numbers = [AOCArrayUtil stringArrayToNumbers:m];
		AOCExtent *ext = [AOCExtent xMin:[numbers[0] integerValue]
									yMin:[numbers[1] integerValue]
									xMax:[numbers[0] integerValue]+[numbers[2] integerValue]-1
									yMax:[numbers[1] integerValue]+[numbers[3] integerValue]-1];
		[extents addObject:ext];
	}
	return extents;
}

@end
