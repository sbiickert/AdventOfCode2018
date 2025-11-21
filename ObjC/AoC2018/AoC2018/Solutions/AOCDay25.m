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
+ (NSInteger)manhattanDistanceBetweenX1:(NSInteger)x1 x2:(NSInteger)x2 y1:(NSInteger)y1 y2:(NSInteger)y2 z1:(NSInteger)z1 z2:(NSInteger)z2 t1:(NSInteger)t1 t2:(NSInteger)t2;

- (AOCCoord4D *)initX:(NSInteger)x y:(NSInteger)y z:(NSInteger)z t:(NSInteger)t;

@property (readonly) NSInteger x;
@property (readonly) NSInteger y;
@property (readonly) NSInteger z;
@property (readonly) NSInteger t;

- (BOOL)isEqualToCoord4D:(AOCCoord4D *)other;
- (AOCCoord4D *)deltaTo:(AOCCoord4D *)other;
- (NSInteger)manhattanDistanceTo:(AOCCoord4D *)other;

@end


@interface Constellation : NSObject

- (Constellation *)init:(AOCCoord4D *) star;

@property (readwrite) NSArray<AOCCoord4D *> *stars;
@property (readwrite) AOCCoord4D *center;
@property (readwrite) NSInteger maxOffset;

- (BOOL)isJoinedWith:(Constellation *)other;
- (void)join:(Constellation *)other;

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

- (NSString *)solvePartOne:(NSArray<AOCCoord4D *> *)input {
	NSMutableArray<Constellation *> *constellations = [NSMutableArray array];
	for (AOCCoord4D *c in input) {
		[constellations addObject:[[Constellation alloc] init:c]];
	}
	
	while (YES) {
		BOOL altered = NO;
		for (NSInteger i = 0; i < constellations.count-1; i++) {
			for (NSInteger j = i+1; j < constellations.count; j++) {
				// See if constellations i and j should connect
				Constellation *c1 = constellations[i];
				Constellation *c2 = constellations[j];
				if ([c1 isJoinedWith:c2]) {
					[c1 join:c2];
					[constellations removeObjectAtIndex:j];
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


@implementation Constellation

- (Constellation *)init:(AOCCoord4D *)star {
	self = [super init];
	self.stars = @[star];
	self.center = star;
	self.maxOffset = 0;
	return self;
}

- (BOOL)isJoinedWith:(Constellation *)other {
	NSInteger centerDistance = [self.center manhattanDistanceTo:other.center];
	if (centerDistance > (self.maxOffset + other.maxOffset + 3)) { return NO; }
		
	for (NSInteger i = 0; i < self.stars.count; i++) {
		for (NSInteger j = 0; j < other.stars.count; j++) {
			AOCCoord4D *coord1 = self.stars[i];
			AOCCoord4D *coord2 = other.stars[j];
			NSInteger md = [AOCCoord4D manhattanDistanceBetweenX1:coord1.x x2:coord2.x
															   y1:coord1.y y2:coord2.y
															   z1:coord1.z z2:coord2.z
															   t1:coord1.t t2:coord2.t];
			//assert(md == mdActual);
			if (md > 0 && md <= 3) {
				return YES;
			}
		}
	}
	return NO;
}

- (void)updateCenterAndOffset {
	NSInteger xmin = self.stars.firstObject.x;
	NSInteger ymin = self.stars.firstObject.y;
	NSInteger zmin = self.stars.firstObject.z;
	NSInteger tmin = self.stars.firstObject.t;
	NSInteger xmax = self.stars.firstObject.x;
	NSInteger ymax = self.stars.firstObject.y;
	NSInteger zmax = self.stars.firstObject.z;
	NSInteger tmax = self.stars.firstObject.t;
	
	for (AOCCoord4D *star in self.stars) {
		xmin = MIN(xmin, star.x);
		ymin = MIN(ymin, star.y);
		zmin = MIN(zmin, star.z);
		tmin = MIN(tmin, star.t);
		xmax = MAX(xmax, star.x);
		ymax = MAX(ymax, star.y);
		zmax = MAX(zmax, star.z);
		tmax = MAX(tmax, star.t);
	}
	
	self.center = [[AOCCoord4D alloc] initX:(xmin + xmax) / 2 y:(ymin + ymax) / 2 z:(zmin + zmax) / 2 t:(tmin + tmax) / 2];
	self.maxOffset = 0;
	
	for (NSNumber *x in @[[NSNumber numberWithLong:xmin], [NSNumber numberWithLong:xmax]]) {
		for (NSNumber *y in @[[NSNumber numberWithLong:ymin], [NSNumber numberWithLong:ymax]]) {
			for (NSNumber *z in @[[NSNumber numberWithLong:zmin], [NSNumber numberWithLong:zmax]]) {
				for (NSNumber *t in @[[NSNumber numberWithLong:tmin], [NSNumber numberWithLong:tmax]]) {
					NSInteger md = [AOCCoord4D manhattanDistanceBetweenX1:x.longValue x2:self.center.x
																	   y1:y.longValue y2:self.center.y
																	   z1:z.longValue z2:self.center.z
																	   t1:t.longValue t2:self.center.t];
					self.maxOffset = MAX(self.maxOffset, md);
				}
			}
		}
	}
}


- (void)join:(Constellation *)other {
	NSArray<AOCCoord4D *> *joined = [self.stars arrayByAddingObjectsFromArray:other.stars];
	self.stars = joined;
	[self updateCenterAndOffset];
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
+ (NSInteger)manhattanDistanceBetweenX1:(NSInteger)x1 x2:(NSInteger)x2 y1:(NSInteger)y1 y2:(NSInteger)y2 z1:(NSInteger)z1 z2:(NSInteger)z2 t1:(NSInteger)t1 t2:(NSInteger)t2 {
	return labs(x1 - x2) + labs(y1 - y2) + labs(z1 - z2) + labs(t1 - t2);
}

@end
