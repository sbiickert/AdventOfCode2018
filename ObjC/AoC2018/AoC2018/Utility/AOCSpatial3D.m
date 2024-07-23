//
//  AOCSpatial3D.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-08.
//

#import <Foundation/Foundation.h>
#import "AOCSpatial3D.h"

@implementation AOCCoord3D

+ (AOCCoord3D *)origin
{
	return [[AOCCoord3D alloc] initX:0 y:0 z:0];
}

+ (AOCCoord3D *)x:(NSInteger)x y:(NSInteger)y z:(NSInteger)z
{
	return [[AOCCoord3D alloc] initX:x y:y z:z];
}

+ (AOCCoord3D *)copyOf:(AOCCoord3D *)other
{
	return [[AOCCoord3D alloc] initX:other.x y:other.y z:other.z];
}

//+ (AOCCoord *)offset:(NSString *)direction;

- (AOCCoord3D *)initX:(NSInteger)x y:(NSInteger)y z:(NSInteger)z
{
	self = [super init];
	_x = x;
	_y = y;
	_z = z;
	return self;
}

- (NSString *)description {
	return [NSString stringWithFormat:@"[%ld,%ld,%ld]", self.x, self.y, self.z];
}

- (AOCCoord3D *)add:(AOCCoord3D *)other
{
	if (other == nil) {
		return [AOCCoord3D copyOf:self];
	}
	return [AOCCoord3D x:self.x + other.x y:self.y + other.y z:self.z + other.z];
}

- (AOCCoord3D *)deltaTo:(AOCCoord3D *)other
{
	if (other == nil) {
		return [AOCCoord3D copyOf:self];
	}
	return [AOCCoord3D x:other.x - self.x
					   y:other.y - self.y
					   z:other.z - self.z];
}
//- (AOCCoord3D *)offset:(NSString *)direction;

- (double)distanceTo:(AOCCoord3D *)other
{
	NSLog(@"AOCCoord3D::distance is not implemented yet.");
	return 0.0;
}

- (NSInteger)manhattanDistanceTo:(AOCCoord3D *)other
{
	AOCCoord3D *delta = [self deltaTo:other];
	return labs(delta.x) + labs(delta.y) + labs(delta.z);
}

- (BOOL)isEqualToCoord3D:(AOCCoord3D *)other
{
	if (other == nil) {
		return NO;
	}
	return self.x == other.x && self.y == other.y && self.z == other.z;
}

- (BOOL)isEqual:(nullable id)object {
	if (object == nil) {
		return NO;
	}

	if (self == object) {
		return YES;
	}

	if (![object isKindOfClass:[AOCCoord3D class]]) {
		return NO;
	}

	return [self isEqualToCoord3D:(AOCCoord3D *)object];
}

- (NSUInteger)hash {
	return [@(self.x) hash] ^ [@(self.y) hash] ^ [@(self.z) hash];
}

// NSCopying (to let this be a key in NSDictionary)
- (id)copyWithZone:(NSZone *)zone
{
	AOCCoord3D *copy = [[AOCCoord3D allocWithZone:zone] initX:_x y:_y z:_z];
	return copy;
}

@end



@implementation AOCExtent3D


+ (AOCExtent3D *)min:(AOCCoord3D *)min max:(AOCCoord3D *)max {
	return [[AOCExtent3D alloc] initMin:min max:max];
}

+ (AOCExtent3D *)xMin:(NSInteger)xmin yMin:(NSInteger)ymin zMin:(NSInteger)zmin xMax:(NSInteger)xmax yMax:(NSInteger)ymax zMax:(NSInteger)zmax {
	return [[AOCExtent3D alloc] initXMin:xmin yMin:ymin zMin:zmin xMax:xmax yMax:ymax zMax:zmax];
}

+ (AOCExtent3D *)copyOf:(AOCExtent3D *)other {
	return [[AOCExtent3D alloc] initXMin:other.min.x yMin:other.min.y zMin:other.min.z xMax:other.max.x yMax:other.max.y zMax:other.max.z];
}

- (AOCExtent3D *)initXMin:(NSInteger)xmin yMin:(NSInteger)ymin  zMin:(NSInteger)zmin xMax:(NSInteger)xmax yMax:(NSInteger)ymax zMax:(NSInteger)zmax {
	AOCCoord3D *c1 = [AOCCoord3D x:xmin y:ymin z:zmin];
	AOCCoord3D *c2 = [AOCCoord3D x:xmax y:ymax z:zmax];
	NSArray<AOCCoord3D *> *coords = @[c1, c2];
	return [self initFrom:coords];
}

- (AOCExtent3D *)initMin:(AOCCoord3D *)min max:(AOCCoord3D *)max {
	NSMutableArray<AOCCoord3D *> *coords = [NSMutableArray array];
	if (min != nil) {
		[coords addObject:min];
	}
	if (max != nil) {
		[coords addObject:max];
	}
	return [self initFrom:coords];
}

- (AOCExtent3D *)initFrom:(NSArray<AOCCoord3D *> *)array {
	self = [super init];
	if (array == nil || array.count == 0) {
		_min = [AOCCoord3D origin];
		_max = [AOCCoord3D origin];
		return self;
	}
	NSInteger xmin = [array firstObject].x;
	NSInteger xmax = [array firstObject].x;
	NSInteger ymin = [array firstObject].y;
	NSInteger ymax = [array firstObject].y;
	NSInteger zmin = [array firstObject].z;
	NSInteger zmax = [array firstObject].z;
	for (int i = 1; i < array.count; i++) {
		xmin = MIN(xmin, [array objectAtIndex:i].x);
		xmax = MAX(xmax, [array objectAtIndex:i].x);
		ymin = MIN(ymin, [array objectAtIndex:i].y);
		ymax = MAX(ymax, [array objectAtIndex:i].y);
		zmin = MIN(zmin, [array objectAtIndex:i].z);
		zmax = MAX(zmax, [array objectAtIndex:i].z);
	}
	_min = [AOCCoord3D x:xmin y:ymin z:zmin];
	_max = [AOCCoord3D x:xmax y:ymax z:zmax];
	return self;
}

- (NSInteger)width {
	return self.max.x - self.min.x + 1;
}
- (NSInteger)height {
	return self.max.y - self.min.y + 1;
}
- (NSInteger)depth {
	return self.max.z - self.min.z + 1;
}
- (NSInteger)volume {
	NSInteger v = self.width * self.height * self.depth;
	if (v < 0) { return NSIntegerMax; }
	return v;
}

- (void)expandToFit:(AOCCoord3D *)coord {
	if (coord.x < self.min.x) {
		_min = [AOCCoord3D x:coord.x y:self.min.y z:self.min.z];
	}
	else if (coord.x > self.max.x) {
		_max = [AOCCoord3D x:coord.x y:self.max.y z:self.min.z];
	}
	if (coord.y < self.min.y) {
		_min = [AOCCoord3D x:self.min.x y:coord.y z:self.min.z];
	}
	else if (coord.y > self.max.y) {
		_max = [AOCCoord3D x:self.max.x y:coord.y z:self.min.z];
	}
	if (coord.z < self.min.z) {
		_min = [AOCCoord3D x:self.min.x y:self.min.y z:coord.z];
	}
	else if (coord.z > self.max.z) {
		_max = [AOCCoord3D x:self.max.x y:self.max.z z:coord.z];
	}
}

- (AOCExtent3D *)expandedToFit:(AOCCoord3D *)coord {
	NSInteger xmin = MIN(coord.x, self.min.x);
	NSInteger xmax = MAX(coord.x, self.max.x);
	NSInteger ymin = MIN(coord.y, self.min.y);
	NSInteger ymax = MAX(coord.y, self.max.y);
	NSInteger zmin = MIN(coord.z, self.min.z);
	NSInteger zmax = MAX(coord.z, self.max.z);
	return [AOCExtent3D xMin:xmin yMin:ymin zMin:zmin
					  xMax:xmax yMax:ymax zMax:zmax];
}

- (NSArray<AOCCoord3D *> *)allCoords {
	NSMutableArray<AOCCoord3D *> *coords = [NSMutableArray array];
	for (NSInteger x = self.min.x; x <= self.max.x; x++) {
		for (NSInteger y = self.min.y; y <= self.max.y; y++) {
			for (NSInteger z = self.min.z; z <= self.max.z; z++) {
				[coords addObject:[AOCCoord3D x:x y:y z:z]];
			}
		}
	}
	return coords;

}

- (AOCExtent3D *)inset:(NSInteger)amount {
	NSInteger xmin = self.min.x + amount;
	NSInteger xmax = self.max.x - amount;
	NSInteger ymin = self.min.y + amount;
	NSInteger ymax = self.max.y - amount;
	NSInteger zmin = self.min.z + amount;
	NSInteger zmax = self.max.z - amount;

	if (xmin > xmax || ymin > ymax || zmin > zmax) { return nil; }
	
	return [AOCExtent3D xMin:xmin yMin:ymin zMin:zmin
						xMax:xmax yMax:ymax zMax:zmax];

}

- (AOCCoord3D *)center {
	NSInteger xMid = (self.max.x + self.min.x) / 2;
	NSInteger yMid = (self.max.y + self.min.y) / 2;
	NSInteger zMid = (self.max.z + self.min.z) / 2;
	return [AOCCoord3D x:xMid y:yMid z:zMid];
}

- (NSArray<AOCCoord3D *> *)corners {
	NSArray<AOCCoord3D *> *result = @[
		[AOCCoord3D x:self.min.x y:self.min.y z:self.min.z],
		[AOCCoord3D x:self.min.x y:self.min.y z:self.max.z],
		[AOCCoord3D x:self.min.x y:self.max.y z:self.min.z],
		[AOCCoord3D x:self.min.x y:self.max.y z:self.max.z],
		[AOCCoord3D x:self.max.x y:self.min.y z:self.min.z],
		[AOCCoord3D x:self.max.x y:self.min.y z:self.max.z],
		[AOCCoord3D x:self.max.x y:self.max.y z:self.min.z],
		[AOCCoord3D x:self.max.x y:self.max.y z:self.max.z]
	];
	return result;
}


- (BOOL)contains:(AOCCoord3D *)coord {
	return 	coord.x >= self.min.x &&
			coord.x <= self.max.x &&
			coord.y >= self.min.y &&
			coord.y <= self.max.y &&
			coord.z >= self.min.z &&
			coord.z <= self.max.z;
}


- (AOCExtent3D *)intersectWith:(AOCExtent3D *)other {
	NSInteger commonMinX = MAX(self.min.x, other.min.x);
	NSInteger commonMaxX = MIN(self.max.x, other.max.x);
	if (commonMaxX < commonMinX) { return nil; }
	NSInteger commonMinY = MAX(self.min.y, other.min.y);
	NSInteger commonMaxY = MIN(self.max.y, other.max.y);
	if (commonMaxY < commonMinY) { return nil; }
	NSInteger commonMinZ = MAX(self.min.z, other.min.z);
	NSInteger commonMaxZ = MIN(self.max.z, other.max.z);
	if (commonMaxZ < commonMinZ) { return nil; }

	return [[AOCExtent3D alloc] initMin:[AOCCoord3D x:commonMinX y:commonMinY z:commonMinZ]
									max:[AOCCoord3D x:commonMaxX y:commonMaxY z:commonMaxZ]];
}

- (NSInteger)distanceTo:(AOCCoord3D *)coord {
	if ([self contains:coord]) { return 0; }
	AOCExtent3D *expanded = [self expandedToFit:coord];
	NSInteger distance = [self.min manhattanDistanceTo:expanded.min] + [self.max manhattanDistanceTo:expanded.max];
//	NSInteger minDistance = NSIntegerMax;
//	for (AOCCoord3D *c in self.corners) {
//		NSInteger md = [c manhattanDistanceTo:coord];
//		if (md < minDistance) { minDistance = md; }
//	}
//	return minDistance;
	return distance;
}


- (BOOL)isEqualToExtent:(AOCExtent3D *)other {
	if (other == nil) { return NO; }
	return [self.min isEqualToCoord3D:other.min] &&
	[self.max isEqualToCoord3D:other.max];
}

- (BOOL)isEqual:(nullable id)object {
	if (object == nil) {
		return NO;
	}

	if (self == object) {
		return YES;
	}

	if (![object isKindOfClass:[AOCExtent3D class]]) {
		return NO;
	}

	return [self isEqualToExtent:(AOCExtent3D *)object];
}

- (NSUInteger)hash {
	return [self.min hash] ^ [self.max hash];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
	AOCExtent3D *copy = [[AOCExtent3D allocWithZone:zone] initMin:self.min max:self.max];
	return copy;
}

- (NSString *)description {
	return [NSString stringWithFormat:@"Box %@, %@", self.min.description, self.max.description];
}

- (NSString *)debugDescription {
	return self.description;
}

@end
