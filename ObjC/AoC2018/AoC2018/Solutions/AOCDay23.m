//
//  AOCDay23.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-03-05.
//

#import <Foundation/Foundation.h>
#import "AOCDay.h"
#import "AOCSpatial3D.h"
#import "AOCStrings.h"


@interface NanoBot : NSObject

- (NanoBot *)init:(NSString *)defn;

@property (readonly) AOCCoord3D *location;
@property (readonly) NSInteger radius;

@property (readonly) NSNumber *botID;
@property NSMutableSet<NSNumber *> *radiusOverlapsWith;
@property (readonly) AOCExtent3D *bbox;

@end

@interface SearchBox : NSObject

- (SearchBox *)init:(AOCExtent3D *)box botsInRange:(NSInteger)inRange;

@property (readonly) AOCExtent3D *box;
@property (readonly) NSInteger botsInRange;
@property (readonly) NSInteger distanceToOrigin;

@end


@implementation AOCDay23

- (AOCDay23 *)init {
	self = [super initWithDay:23 name:@"Experimental Emergency Teleportation"];
	return self;
}

- (struct AOCResult)solveInputIndex:(int)index inFile:(NSString *)filename {
	struct AOCResult result = [super solveInputIndex:index inFile:filename];
	
	NSArray<NSString *> *input = [AOCInput readGroupedInputFile:filename atIndex:index];
	
	NSMutableArray<NanoBot *> *bots = [NSMutableArray array];
	for (NSString *line in input) {
		[bots addObject:[[NanoBot alloc] init:line]];
	}
	
	result.part1 = [self solvePartOne: bots];
	result.part2 = [self solvePartTwo: bots];
	
	return result;
}


+ (NSInteger)countBotsInRangeOf:(AOCExtent3D *)box bots:(NSArray<NanoBot *> *)allBots {
	int count = 0;
	for (NanoBot *bot in allBots) {
		NSInteger dist = [box distanceTo:bot.location];
		if (dist <= bot.radius) {
			count++;
		}
	}
	return count;
}

+ (NSArray<AOCExtent3D *> *)divideBox:(AOCExtent3D *)box {
	NSMutableArray<AOCExtent3D *> *result = [NSMutableArray arrayWithCapacity:8];
	AOCCoord3D *center = box.center;
	for (AOCCoord3D *corner in box.corners) {
		NSInteger x = center.x;
		NSInteger y = center.y;
		NSInteger z = center.z;
		if (corner.x > x) { x++; }
		if (corner.y > y) { y++; }
		if (corner.z > z) { z++; }
		[result addObject:[AOCExtent3D min:corner max:[AOCCoord3D x:x y:y z:z]]];
	}
	return result;
}


- (NSString *)solvePartOne:(NSMutableArray<NanoBot *> *)bots {
	NSSortDescriptor *sd = [NSSortDescriptor sortDescriptorWithKey:@"self.radius" ascending:NO];
	[bots sortUsingDescriptors:[NSArray arrayWithObject:sd]];
	
	NanoBot *largestRadiusBot = bots.firstObject;
	NSInteger countInRadius = 0;
	
	for (NanoBot *bot in bots) {
		if ([bot.location manhattanDistanceTo:largestRadiusBot.location] <= largestRadiusBot.radius) {
			//NSLog(@"bot at %@ is in radius", bot.location);
			countInRadius++;
		}
	}
		
	return [NSString stringWithFormat: @"The number of bots within %ld of the strongest is %ld", largestRadiusBot.radius, countInRadius];
}


- (NSString *)solvePartTwo:(NSMutableArray<NanoBot *> *)bots {
	// Answer: 113799398, 975 bots in range, according to https://raw.githack.com/ypsu/experiments/master/aoc2018day23/vis.html
	NSMutableArray<SearchBox *> *work = [NSMutableArray array];
	NSArray<AOCCoord3D *> *coords = [bots valueForKey:@"location"];
	AOCExtent3D *box = [[AOCExtent3D alloc] initFrom:coords];
	SearchBox *sBox = [[SearchBox alloc] init:box botsInRange:[AOCDay23 countBotsInRangeOf:box bots:bots]];
	[work addObject:sBox];
	
	SearchBox *workBox;
	while (YES) {
		[work sortUsingComparator:^NSComparisonResult(SearchBox *b1, SearchBox *b2) {
			//	1) is in range for the most bots,
			//	2) in case of ties is closest to origin,
			//	3) in case of ties is the smallest.
			if (b1.botsInRange != b2.botsInRange) {
				// Fewest to most
				return [[NSNumber numberWithInteger:b1.botsInRange] compare:[NSNumber numberWithInteger:b2.botsInRange]];
			}
			
			if (b1.distanceToOrigin != b2.distanceToOrigin) {
				// Largest to smallest
				return [[NSNumber numberWithInteger:b2.distanceToOrigin] compare:[NSNumber numberWithInteger:b1.distanceToOrigin]];
			}
			
			// Largest to smallest
			return [[NSNumber numberWithInteger:b2.box.volume] compare:[NSNumber numberWithInteger:b1.box.volume]];
		}];
		
		workBox = work.lastObject;
//		NSLog(@"workBox: %@", workBox);
		
		[work removeLastObject];
		
		if (workBox.box.volume == 1) { break; }
		
		// Split into 8 boxes
		NSArray<AOCExtent3D *> *split = [AOCDay23 divideBox:workBox.box];
		for (AOCExtent3D *subBox in split) {
			[work addObject: [[SearchBox alloc] init:subBox botsInRange:[AOCDay23 countBotsInRangeOf: subBox bots:bots]]];
		}
	}
	
	return [NSString stringWithFormat: @"The closest point is at distance %ld.", [workBox.box distanceTo:[AOCCoord3D origin]]];
}

- (NSInteger)searchVolume:(AOCExtent3D *)volume bots:(NSArray<NanoBot *> *)bots {
	NSInteger mostInRadius = 0;
	AOCCoord3D *closestToOrigin = nil;
	NSInteger closestMD = NSIntegerMax;
	AOCCoord3D *origin = [AOCCoord3D origin];
	
	for (AOCCoord3D *coord in volume.allCoords) {
		NSInteger count = 0;
		for (NanoBot *bot in bots) {
			if ([coord manhattanDistanceTo:bot.location] <= bot.radius) { count++; }
		}
		if (count > mostInRadius) {
			mostInRadius = count;
			closestToOrigin = coord;
			closestMD = [coord manhattanDistanceTo:origin];
		}
		else if (count == mostInRadius) {
			NSInteger mdToOrigin = [coord manhattanDistanceTo:origin];
			if (mdToOrigin < closestMD) {
				closestToOrigin = coord;
			}
		}
	}
	NSLog(@"%@", closestToOrigin);
	return closestMD;
}

@end



@implementation NanoBot

static NSInteger _nextID = 0;

- (NanoBot *)init:(NSString *)defn {
	self = [super init];
	
	NSArray<NSString *> *match = [defn matchPattern:@"<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)" caseSensitive:NO];
	_location = [AOCCoord3D x:match[1].integerValue y:match[2].integerValue z:match[3].integerValue];
	_radius = match[4].integerValue;
	
	_botID = [NSNumber numberWithInteger:_nextID++];
	_radiusOverlapsWith = [NSMutableSet set];
	_bbox = [[AOCExtent3D alloc] initXMin:_location.x - _radius
									 yMin:_location.y - _radius
									 zMin:_location.z - _radius
									 xMax:_location.x + _radius
									 yMax:_location.y + _radius
									 zMax:_location.z + _radius];
	
	return self;
}

@end

@implementation SearchBox

- (SearchBox *)init:(AOCExtent3D *)box botsInRange:(NSInteger)inRange {
	self = [super init];
	_box = box;
	_botsInRange = inRange;
	_distanceToOrigin = [box distanceTo:[AOCCoord3D origin]];
	return self;
}

- (NSString *)description {
	return [NSString stringWithFormat:@"%@ bots:%ld dist:%ld volume:%ld", self.box, self.botsInRange, self.distanceToOrigin, self.box.volume];
}

- (NSString *)debugDescription {
	return self.description;
}

@end
