//
//  AOCDay25.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-07-24.
//

#import <Foundation/Foundation.h>
#import "AOCDay.h"


@interface AOCCoord4D : NSObject <NSCopying>

+ (AOCCoord4D *)origin;
+ (AOCCoord4D *)x:(NSInteger)x y:(NSInteger)y z:(NSInteger)z t:(NSInteger)t;
+ (AOCCoord4D *)copyOf:(AOCCoord4D *)other;

- (AOCCoord4D *)initX:(NSInteger)x y:(NSInteger)y z:(NSInteger)z t:(NSInteger)t;

@property (readonly) NSInteger x;
@property (readonly) NSInteger y;
@property (readonly) NSInteger z;
@property (readonly) NSInteger t;

- (BOOL)isEqualToCoord4D:(AOCCoord4D *)other;
- (AOCCoord4D *)deltaTo:(AOCCoord4D *)other;
- (NSInteger)manhattanDistanceTo:(AOCCoord4D *)other;

@end


@implementation AOCDay25

- (AOCDay25 *)init {
	self = [super initWithDay:25 name:@"Four-Dimensional Adventure"];
	return self;
}

- (NSArray<AOCCoord4D *> *)parseCoords:(NSArray<NSString *> *)input {
	NSMutableArray<AOCCoord4D *> *coords = [NSMutableArray arrayWithCapacity:input.count];
	for (NSString *line in input) {
		NSArray<NSString *> *numStrs = [line componentsSeparatedByString:@","];
		AOCCoord4D *c = [AOCCoord4D x:numStrs[0].integerValue
									y:numStrs[1].integerValue
									z:numStrs[2].integerValue
									t:numStrs[3].integerValue];
		[coords addObject:c];
	}
	return coords;
}

- (struct AOCResult)solveInputIndex:(int)index inFile:(NSString *)filename {
	struct AOCResult result = [super solveInputIndex:index inFile:filename];
	
	NSArray<NSString *> *input = [AOCInput readGroupedInputFile:filename atIndex:index];
	NSArray<AOCCoord4D *> *coords = [self parseCoords:input];
	
	result.part1 = [self solvePartOne: coords];
	result.part2 = [self solvePartTwo: input];
	
	return result;
}

- (BOOL)constellationsAreJoined:(NSArray<AOCCoord4D *> *)c1 with:(NSArray<AOCCoord4D *> *)c2 {
	for (NSInteger i = 0; i < c1.count; i++) {
		for (NSInteger j = 0; j < c2.count; j++) {
			AOCCoord4D *coord1 = c1[i];
			AOCCoord4D *coord2 = c2[j];
			NSInteger md = [coord1 manhattanDistanceTo:coord2];
			//assert(md == mdActual);
			if (md > 0 && md <= 3) {
				return YES;
			}
		}
	}
	return NO;
}

- (NSString *)solvePartOne:(NSArray<AOCCoord4D *> *)input {
	NSMutableArray<NSArray<AOCCoord4D *> *> *constellations = [NSMutableArray array];
	for (AOCCoord4D *c in input) {
		[constellations addObject:[NSArray arrayWithObject:c]];
	}
	
	while (YES) {
		BOOL altered = NO;
		for (NSInteger i = 0; i < constellations.count-1; i++) {
			for (NSInteger j = i+1; j < constellations.count; j++) {
				// See if constellations i and j should connect
				if ([self constellationsAreJoined:constellations[i] with:constellations[j]]) {
					NSArray<AOCCoord4D *> *joined = [constellations[i] arrayByAddingObjectsFromArray:constellations[j]];
					[constellations removeObjectAtIndex:j];
					[constellations replaceObjectAtIndex:i withObject:joined];
					altered = YES;
					break;
				}
			}
			if (altered) {break;}
		}
		if (altered == NO) {break;}
		//if (constellations.count % 25 == 0) {NSLog(@"Constellation count: %ld", constellations.count);}
	}
	
	return [NSString stringWithFormat: @"The number of constellations is %ld", constellations.count];
}

- (NSString *)solvePartTwo:(NSArray<NSString *> *)input {
	return [NSString stringWithFormat: @"Merry Christmas!"];
}

@end






@implementation AOCCoord4D

+ (AOCCoord4D *)origin {
	return [AOCCoord4D x:0 y:0 z:0 t:0];
}

+ (AOCCoord4D *)copyOf:(AOCCoord4D *)other {
	return [AOCCoord4D x:other.x y:other.y z:other.z t:other.t];
}

+ (AOCCoord4D *)x:(NSInteger)x y:(NSInteger)y z:(NSInteger)z t:(NSInteger)t {
	return [[AOCCoord4D alloc] initX:x y:y z:z t:t];
}

- (AOCCoord4D *)initX:(NSInteger)x y:(NSInteger)y z:(NSInteger)z t:(NSInteger)t {
	self = [super init];
	_x = x;
	_y = y;
	_z = z;
	_t = t;
	return self;
}

- (NSInteger)manhattanDistanceTo:(AOCCoord4D *)other {
	AOCCoord4D *delta = [self deltaTo:other];
	return labs(delta.x) + labs(delta.y) + labs(delta.z) + labs(delta.t);
}

- (AOCCoord4D *)deltaTo:(AOCCoord4D *)other {
	if (other == nil) {
		return [AOCCoord4D copyOf:self];
	}
	return [AOCCoord4D x:other.x - self.x
					   y:other.y - self.y
					   z:other.z - self.z
					   t:other.t - self.t];
}

- (BOOL)isEqualToCoord4D:(AOCCoord4D *)other
{
	if (other == nil) {
		return NO;
	}
	return self.x == other.x && self.y == other.y && self.z == other.z && self.t == other.t;
}

- (BOOL)isEqual:(nullable id)object {
	if (object == nil) {
		return NO;
	}

	if (self == object) {
		return YES;
	}

	if (![object isKindOfClass:[AOCCoord4D class]]) {
		return NO;
	}

	return [self isEqualToCoord4D:(AOCCoord4D *)object];
}

- (NSUInteger)hash {
	return [@(self.x) hash] ^ [@(self.y) hash] ^ [@(self.z) hash] ^ [@(self.t) hash];
}

// NSCopying (to let this be a key in NSDictionary)
- (id)copyWithZone:(NSZone *)zone
{
	AOCCoord4D *copy = [[AOCCoord4D allocWithZone:zone] initX:_x y:_y z:_z t:_t];
	return copy;
}

- (NSString *)description {
	return [NSString stringWithFormat:@"[%ld,%ld,%ld,%ld]", self.x, self.y, self.z, self.t];
}

- (NSString *)debugDescription {
	return self.description;
}
@end
