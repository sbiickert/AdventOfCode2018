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
	for (NSInteger i = 0; i < bots.count-1; i++) {
		for (NSInteger j = i+1; j < bots.count; j++) {
			NSInteger md = [bots[i].location manhattanDistanceTo:bots[j].location];
			NSInteger rDist = bots[i].radius + bots[j].radius;
			//NSLog(@"Bot #%@ to #%@ md is %ld. Radius sum is %ld", bots[i].botID, bots[j].botID, md, rDist);
			if (md <= rDist) {
				// spheres defined by their radius touch
				[bots[i].radiusOverlapsWith addObject:bots[j].botID];
				[bots[j].radiusOverlapsWith addObject:bots[i].botID];
			}
		}
	}
	
	// Create lookup, Find the total volume
	NSMutableDictionary<NSNumber *, NanoBot *> *botLookup = [NSMutableDictionary dictionary];
	NSMutableArray<AOCCoord3D *> *coords = [NSMutableArray array];

	for (NanoBot *bot in bots) {
		//NSLog(@"bot #%@ overlaps with %@", bot.botID, bot.radiusOverlapsWith);
		botLookup[bot.botID] = bot;
		[coords addObject:bot.location];
	}
	AOCExtent3D *totalVolume = [[AOCExtent3D alloc] initFrom:coords];
	
	// Search the space in this radius
	NSInteger mostInRadius = 0;
	AOCCoord3D *closestToOrigin = nil;
	NSInteger closestMD = NSIntegerMax;
	AOCCoord3D *origin = [AOCCoord3D origin];
	
	for (AOCCoord3D *coord in totalVolume.allCoords) {
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
//	for (NSInteger x = smallest.location.x - smallest.radius; x <= smallest.location.x + smallest.radius; x++) {
//		for (NSInteger y = smallest.location.y - smallest.radius; y <= smallest.location.y + smallest.radius; y++) {
//			for (NSInteger z = smallest.location.z - smallest.radius; z <= smallest.location.z + smallest.radius; z++) {
//				AOCCoord3D *test = [AOCCoord3D x:x y:y z:z];
//				if ([test manhattanDistanceTo:smallest.location] <= smallest.radius) {
//					NSInteger count = 0;
//					for (NSNumber *otherID in smallest.radiusOverlapsWith) {
//						NanoBot *other = botLookup[otherID];
//						NSInteger otherDistance = [test manhattanDistanceTo:other.location];
//						if (otherDistance <= other.radius) {
//							count++;
//						}
//					}
//					if (count == smallest.radiusOverlapsWith.count) {
//						// All others were within radius
//						NSInteger originDistance = [test manhattanDistanceTo:origin];
//						if (originDistance < closestMD) {
//							closestMD = originDistance;
//							closestToOrigin = test;
//						}
//					}
//				}
//			}
//		}
//	}

	return [NSString stringWithFormat: @"The closest point is %@ at distance %ld", closestToOrigin, closestMD];
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
