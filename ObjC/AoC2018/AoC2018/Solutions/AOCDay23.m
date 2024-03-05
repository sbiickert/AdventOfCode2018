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
	result.part2 = [self solvePartTwo: input];
	
	return result;
}

- (NSString *)solvePartOne:(NSMutableArray<NanoBot *> *)bots {
	NSSortDescriptor *sd = [NSSortDescriptor sortDescriptorWithKey:@"self.radius" ascending:NO];
	[bots sortUsingDescriptors:[NSArray arrayWithObject:sd]];
	
	NanoBot *largestRadiusBot = bots.firstObject;
	NSInteger countInRadius = 0;
	
	for (NanoBot *bot in bots) {
		if ([bot.location manhattanDistanceTo:largestRadiusBot.location] <= largestRadiusBot.radius) {
			NSLog(@"bot at %@ is in radius", bot.location);
			countInRadius++;
		}
	}
		
	return [NSString stringWithFormat: @"The number of bots within %ld of the strongest is %ld", largestRadiusBot.radius, countInRadius];
}

- (NSString *)solvePartTwo:(NSArray<NSString *> *)input {
	
	return [NSString stringWithFormat: @"World"];
}

@end



@implementation NanoBot

- (NanoBot *)init:(NSString *)defn {
	self = [super init];
	
	NSArray<NSString *> *match = [defn matchPattern:@"<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)" caseSensitive:NO];
	_location = [AOCCoord3D x:match[1].integerValue y:match[2].integerValue z:match[3].integerValue];
	_radius = match[4].integerValue;
	
	return self;
}

@end
