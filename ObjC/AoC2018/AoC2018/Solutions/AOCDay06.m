//
//  AOCDay06.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-19.
//

#import <Foundation/Foundation.h>
#import "AOCDay.h"
#import "AOCSpatial.h"
#import "AOCGrid.h"
#import "AOCArrays.h"

@implementation AOCDay06

- (AOCDay06 *)init {
	self = [super initWithDay:06 name:@"Chronal Coordinates"];
	return self;
}

- (struct AOCResult)solveInputIndex:(int)index inFile:(NSString *)filename {
	struct AOCResult result = [super solveInputIndex:index inFile:filename];
	
	NSArray<NSString *> *input = [AOCInput readGroupedInputFile:filename atIndex:index];
	NSArray<AOCCoord *> *coords = [self parseCoordinates:input];
	
	result.part1 = [self solvePartOne: coords];
	result.part2 = [self solvePartTwo: coords];
	
	return result;
}

- (NSString *)solvePartOne:(NSArray<AOCCoord *> *)coords {
	AOCGrid *grid = [AOCGrid grid];
	for (NSInteger i = 0; i < coords.count; i++) {
		AOCCoord *c = coords[i];
		[grid setObject:[NSNumber numberWithInteger:i] atCoord:c];
	}
	
	AOCExtent *ext = grid.extent;
	
	for (AOCCoord *loc in ext.allCoords) {
		NSInteger shortestDistance = [ext.max manhattanDistanceTo:ext.min];
		NSNumber *closest = nil;
		for (NSInteger i = 0; i < coords.count; i++) {
			AOCCoord *c = coords[i];
			NSInteger dist = [loc manhattanDistanceTo:c];
			if (dist < shortestDistance) {
				shortestDistance = dist;
				closest = [NSNumber numberWithInteger:i];
			}
			else if (dist == shortestDistance) {
				closest = nil;
			}
		}
		if (closest) {
			[grid setObject:closest atCoord:loc];
		}
	}
	
	NSMutableDictionary<NSString *, NSNumber *> *hist = grid.histogram.mutableCopy;
	
	for (AOCCoord *loc in ext.edgeCoords) {
		[hist removeObjectForKey:[grid stringAtCoord:loc]];
	}
	NSArray<NSNumber *> *countsAsc = [AOCArrayUtil sortedNumbers:hist.allValues ascending:YES];
	
	return [NSString stringWithFormat: @"The largest non-infinite area is %@", countsAsc.lastObject];
}

- (NSString *)solvePartTwo:(NSArray<AOCCoord *> *)coords {
	AOCGrid *grid = [AOCGrid grid];
	AOCExtent *ext = [[AOCExtent alloc] initFrom:coords];
	
	NSInteger limit = (coords.count > 10) ? 10000 : 32;
	
	for (AOCCoord *loc in ext.allCoords) {
		NSInteger sumDistance = 0;
		for (AOCCoord *c in coords) {
			sumDistance += [loc manhattanDistanceTo:c];
			if (sumDistance >= limit) {break;}
		}
		if (sumDistance < limit) {
			[grid setObject:[NSNumber numberWithInteger:sumDistance] atCoord:loc];
		}
	}
	
	NSInteger numberOfLocationsLessThanLimit = grid.coords.count;

	return [NSString stringWithFormat: @"The number of locations at less than %ld distance is %ld", limit, numberOfLocationsLessThanLimit];
}

- (NSArray<AOCCoord *> *)parseCoordinates:(NSArray<NSString *> *)input {
	NSMutableArray<AOCCoord *> *coords = [NSMutableArray array];
	for (NSString *line in input) {
		NSArray<NSString *> *split = [line componentsSeparatedByString:@", "];
		AOCCoord *c = [AOCCoord x:[split[0] integerValue] y:[split[1] integerValue]];
		[coords addObject:c];
	}
	return coords;
}

@end
