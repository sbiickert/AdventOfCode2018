//
//  AOCSpatial.m
//  AoC2018
//
//  Created by Simon Biickert .
//

#import <Foundation/Foundation.h>
#import "AOCSpatial.h"
#include "math.h"

NSString * const NORTH = @"north";
NSString * const SOUTH = @"south";
NSString * const WEST = @"west";
NSString * const EAST = @"east";
NSString * const NW = @"nw";
NSString * const NE = @"ne";
NSString * const SW = @"sw";
NSString * const SE = @"se";
NSString * const UP = @"up";
NSString * const DOWN = @"down";
NSString * const LEFT = @"left";
NSString * const RIGHT = @"right";

NSString * const ROOK = @"rook";
NSString * const BISHOP = @"bishop";
NSString * const QUEEN = @"queen";

NSString * const CW = @"cw";
NSString * const CCW = @"ccw";



/* *************************************************
 
 AOCCoord
 
 ************************************************* */

static NSDictionary<NSString *, AOCCoord *> *_offsets = nil;
static NSDictionary<NSString *, NSArray<AOCCoord *> *> *_offsetsForRule = nil;

@implementation AOCCoord

+ (void)initialize
{
	if (!_offsets) {
		NSMutableDictionary<NSString *, AOCCoord *> *dict = [NSMutableDictionary dictionary];
		[dict setObject:[AOCCoord x: 0 y:-1] forKey:NORTH];
		[dict setObject:[AOCCoord x: 0 y: 1] forKey:SOUTH];
		[dict setObject:[AOCCoord x:-1 y: 0] forKey:WEST];
		[dict setObject:[AOCCoord x: 1 y: 0] forKey:EAST];
		
		[dict setObject:[AOCCoord x:-1 y:-1] forKey:NW];
		[dict setObject:[AOCCoord x: 1 y:-1] forKey:NE];
		[dict setObject:[AOCCoord x:-1 y: 1] forKey:SW];
		[dict setObject:[AOCCoord x: 1 y: 1] forKey:SE];
				
		_offsets = dict;
	}
	if (!_offsetsForRule) {
		NSMutableDictionary<NSString *, NSArray<AOCCoord *> *> *dict = [NSMutableDictionary dictionary];
		NSArray<AOCCoord *> *coords;
		coords = @[_offsets[NORTH], _offsets[EAST], _offsets[SOUTH], _offsets[WEST]];
		dict[ROOK] = coords;
		
		coords = @[_offsets[NW], _offsets[NE], _offsets[SE], _offsets[SW]];
		dict[BISHOP] = coords;
		
		coords = @[_offsets[NORTH], _offsets[EAST], _offsets[SOUTH], _offsets[WEST],
				   _offsets[NW], _offsets[NE], _offsets[SE], _offsets[SW]];
		dict[QUEEN] = coords;
		
		_offsetsForRule = dict;
	}
}

+ (AOCCoord *)origin {
	return [[AOCCoord alloc] initX:0 y:0];
}

+ (AOCCoord *)x:(NSInteger)x y:(NSInteger)y {
	return [[AOCCoord alloc] initX:x y:y];
}

+ (AOCCoord *)copyOf:(AOCCoord *)other {
	return [[AOCCoord alloc] initX:other.x y:other.y];
}

+ (AOCCoord *)offset:(NSString *)direction {
	NSString *checked = direction;
	if ([AOCDir isDirection:direction] == NO) {
		checked = [AOCDir resolveAlias:direction];
		if ([AOCDir isDirection:checked] == NO) {
			return nil;
		}
	}
	return [_offsets objectForKey:checked];
}

+ (NSArray<AOCCoord *> *)adjacentOffsetsWithRule:(NSString *)rule {
	return _offsetsForRule[rule];
}


- (AOCCoord *)initX:(NSInteger)x y:(NSInteger)y {
	self = [super init];
	_x = x;
	_y = y;
	return self;
}

- (NSInteger)row {
	return self.y;
}

- (NSInteger)col {
	return self.x;
}

- (NSString *)description {
	return [NSString stringWithFormat:@"[%ld,%ld]", self.x, self.y];
}

- (AOCCoord *)add:(AOCCoord *)other {
	if (other == nil) {
		return [AOCCoord copyOf:self];
	}
	return [AOCCoord x:self.x + other.x y:self.y + other.y];
}

- (AOCCoord *)deltaTo:(AOCCoord *)other {
	if (other == nil) {
		return [AOCCoord copyOf:self];
	}
	return [AOCCoord x:other.x - self.x
					   y:other.y - self.y];
}

- (AOCCoord *)offset:(NSString *)direction
{
	return [self add: [AOCCoord offset:direction]];
}


- (double)distanceTo:(AOCCoord *)other {
	AOCCoord *delta = [self deltaTo:other];
	return sqrt(pow(delta.x, 2) + pow(delta.y, 2));
}

- (NSInteger)manhattanDistanceTo:(AOCCoord *)other {
	AOCCoord *delta = [self deltaTo:other];
	return labs(delta.x) + labs(delta.y);
}

- (BOOL)isAdjacentTo:(AOCCoord *)other rule:(NSString *)rule {
	if ([rule isEqualToString:ROOK]) {
		return [self manhattanDistanceTo:other] == 1;
	}
	if ([rule isEqualToString:BISHOP]) {
		return labs(self.x - other.x) == 1 && labs(self.y - other.y) == 1;
	}
	if ([rule isEqualToString:QUEEN]) {
		return ([self manhattanDistanceTo:other] == 1) || (labs(self.x - other.x) == 1 && labs(self.y - other.y) == 1);
	}
	return NO;
}

- (NSArray<AOCCoord *> *)adjacentCoordsWithRule:(NSString *)rule {
	NSMutableArray<AOCCoord *> *result = [NSMutableArray array];
	
	for (AOCCoord *c in _offsetsForRule[rule]) {
		[result addObject:[self add:c]];
	}
	
	return result;
}

- (NSString *)directionTo:(AOCCoord *)other {
	if ([self isEqualToCoord:other]) { return nil; }
	AOCCoord *diff = [other deltaTo:self];
	if (diff.y < 0) {
		if (diff.x < 0)   { return NW; }
		if (diff.x == 0)  { return NORTH; }
		else			  { return NE; }
	}
	if (diff.y == 0) {
		if (diff.x < 0)   { return WEST; }
		if (diff.x > 0)   { return EAST; }
	}
	if (diff.y > 0) {
		if (diff.x < 0)   { return SW; }
		if (diff.x == 0)  { return SOUTH; }
		else			  { return SE; }
	}
	return nil; // Should never get here.
}

- (BOOL)isEqualToCoord:(AOCCoord *)other {
	if (other == nil) {
		return NO;
	}
	return self.x == other.x && self.y == other.y;
}

- (BOOL)isEqual:(nullable id)object {
	if (object == nil) {
		return NO;
	}

	if (self == object) {
		return YES;
	}

	if (![object isKindOfClass:[AOCCoord class]]) {
		return NO;
	}

	return [self isEqualToCoord:(AOCCoord *)object];
}

- (NSUInteger)hash {
	return [@(self.x) hash] ^ [@(self.y) hash];
}

// NSCopying (to let this be a key in NSDictionary)
- (id)copyWithZone:(NSZone *)zone
{
	AOCCoord *copy = [[AOCCoord allocWithZone:zone] initX:_x y:_y];
	return copy;
}

@end


/* *************************************************
 
 AOCDir
 
 ************************************************* */

static NSArray<NSString *> *_dirs = nil;
static NSSet<NSString *> *_dirSet = nil;
static NSDictionary<NSString *, NSString*> *_aliases = nil;

@implementation AOCDir

+ (void)initialize
{
	if (!_dirs) {
		_dirs = @[NORTH, NE, EAST, SE, SOUTH, SW, WEST, NW];
		_dirSet = [NSSet setWithArray:_dirs];
	}
	
	if (!_aliases) {
		_aliases = @{@"^": NORTH, UP: NORTH, @"u": NORTH,
					 @"<": WEST, LEFT: WEST, @"l": WEST,
					 @">": EAST, RIGHT: EAST, @"r": EAST,
					 @"v": SOUTH, DOWN: SOUTH, @"d": SOUTH};
	}
}

+ (BOOL)isDirection:(NSString *)dir {
	return [_dirs containsObject:dir];
}

+ (NSString *)resolveAlias:(NSString *)alias {
	NSString *lc = [alias lowercaseString];
	return _aliases[lc];
}

+ (AOCCoord *)offset:(NSString *)dir {
	return _offsets[dir];
}

+ (NSString *)turn:(NSString *)dir inDirection:(NSString *)turnDir {
	NSUInteger i = [_dirs indexOfObject:dir];
	if (i == NSNotFound) { return dir; }
	
	if ([turnDir isEqualToString:CW]) {
		i += 2;
		i = i % 8;
		return _dirs[i];
	}
	if ([turnDir isEqualToString:CCW]) {
		i += 6;
		i = i % 8;
		return _dirs[i];
	}
	return dir;
}

@end


/* *************************************************
 
 AOCPos
 
 ************************************************* */

@implementation AOCPos

+ (AOCPos *)position:(AOCCoord *)location direction:(NSString *)dir {
	return [[AOCPos alloc] init:location direction:dir];
}

- (AOCPos *)init:(AOCCoord *)location direction:(NSString *)dir {
	self = [super init];
	_location = location;
	_direction = dir;
	return self;
}

- (void)moveForward:(NSInteger)distance {
	AOCCoord *offset = [AOCDir offset:self.direction];
	AOCCoord *movement = [AOCCoord x:offset.x * distance
								   y:offset.y * distance];
	_location = [self.location add:movement];
}

- (AOCPos *)movedForward:(NSInteger)distance {
	AOCCoord *offset = [AOCDir offset:self.direction];
	AOCCoord *movement = [AOCCoord x:offset.x * distance
								   y:offset.y * distance];
	return [[AOCPos alloc] init:[self.location add:movement]
					  direction:self.direction];
}

- (void)turn:(NSString *)turnDir {
	NSString *newDir = [AOCDir turn:self.direction inDirection:turnDir];
	_direction = newDir;
}

- (AOCPos *)turned:(NSString *)turnDir {
	NSString *newDir = [AOCDir turn:self.direction inDirection:turnDir];
	return [[AOCPos alloc] init:self.location direction:newDir];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone { 
	AOCPos *copy = [[AOCPos allocWithZone:zone] init:self.location direction:self.direction];
	return copy;

}

- (BOOL)isEqualToPos:(AOCPos *)other {
	if (other == nil) { return NO; }
	return [self.location isEqualToCoord:other.location] &&
			[self.direction isEqualToString:other.direction];
}

@end



/* *************************************************
 
 AOCExtent
 
 ************************************************* */

@implementation AOCExtent

+ (AOCExtent *)xMin:(NSInteger)xmin yMin:(NSInteger)ymin xMax:(NSInteger)xmax yMax:(NSInteger)ymax {
	return [[AOCExtent alloc] initXMin:xmin yMin:ymin xMax:xmax yMax:ymax];
}

+ (AOCExtent *)min:(AOCCoord *)min max:(AOCCoord *)max {
	return [[AOCExtent alloc] initMin:min max:max];
}

+ (AOCExtent *)copyOf:(AOCExtent *)other
{
	return [[AOCExtent alloc] initXMin:other.min.x yMin:other.min.y xMax:other.max.x yMax:other.max.y];
}


- (AOCExtent *)initXMin:(NSInteger)xmin yMin:(NSInteger)ymin xMax:(NSInteger)xmax yMax:(NSInteger)ymax
{
	AOCCoord *c1 = [AOCCoord x:xmin y:ymin];
	AOCCoord *c2 = [AOCCoord x:xmax y:ymax];
	NSArray<AOCCoord *> *coords = @[c1, c2];
	return [self initFrom:coords];
}

- (AOCExtent *)initMin:(AOCCoord *)min max:(AOCCoord *)max
{
	NSMutableArray<AOCCoord *> *coords = [NSMutableArray array];
	if (min != nil) {
		[coords addObject:min];
	}
	if (max != nil) {
		[coords addObject:max];
	}
	return [self initFrom:coords];
}

- (AOCExtent *)initFrom:(NSArray<AOCCoord *> *)array
{
	self = [super init];
	if (array == nil || array.count == 0) {
		_min = [AOCCoord origin];
		_max = [AOCCoord origin];
		return self;
	}
	NSInteger xmin = [array firstObject].x;
	NSInteger xmax = [array firstObject].x;
	NSInteger ymin = [array firstObject].y;
	NSInteger ymax = [array firstObject].y;
	for (int i = 1; i < array.count; i++) {
		xmin = MIN(xmin, [array objectAtIndex:i].x);
		xmax = MAX(xmax, [array objectAtIndex:i].x);
		ymin = MIN(ymin, [array objectAtIndex:i].y);
		ymax = MAX(ymax, [array objectAtIndex:i].y);
	}
	_min = [AOCCoord x:xmin y:ymin];
	_max = [AOCCoord x:xmax y:ymax];
	return self;
}

- (NSInteger)width
{
	return self.max.x - self.min.x + 1;
}

- (NSInteger)height
{
	return self.max.y - self.min.y + 1;
}

- (NSInteger)area
{
	return self.width * self.height;
}

- (AOCCoord *)nw {
	return self.min;
}

- (AOCCoord *)ne {
	return [AOCCoord x:self.max.x y:self.min.y];
}

- (AOCCoord *)se {
	return self.max;
}

- (AOCCoord *)sw {
	return [AOCCoord x:self.min.x y:self.max.y];
}

- (void)expandToFit:(AOCCoord *)coord
{
	if (coord.x < self.min.x) {
		_min = [AOCCoord x:coord.x y:self.min.y];
	}
	else if (coord.x > self.max.x) {
		_max = [AOCCoord x:coord.x y:self.max.y];
	}
	if (coord.y < self.min.y) {
		_min = [AOCCoord x:self.min.x y:coord.y];
	}
	else if (coord.y > self.max.y) {
		_max = [AOCCoord x:self.max.x y:coord.y];
	}
}

- (AOCExtent *)expandedToFit:(AOCCoord *)coord {
	NSInteger xmin = MIN(coord.x, self.min.x);
	NSInteger xmax = MAX(coord.x, self.max.x);
	NSInteger ymin = MIN(coord.y, self.min.y);
	NSInteger ymax = MAX(coord.y, self.max.y);
	return [AOCExtent xMin:xmin yMin:ymin xMax:xmax yMax:ymax];
}

- (NSArray<AOCCoord *> *)allCoords
{
	NSMutableArray<AOCCoord *> *coords = [NSMutableArray array];
	for (NSInteger x = self.min.x; x <= self.max.x; x++) {
		for (NSInteger y = self.min.y; y <= self.max.y; y++) {
			[coords addObject:[AOCCoord x:x y:y]];
		}
	}
	return coords;
}

- (NSArray<AOCCoord *> *)coordsInColumn:(NSInteger)column {
	NSMutableArray<AOCCoord *> *coords = [NSMutableArray array];
	for (NSInteger y = self.min.y; y <= self.max.y; y++) {
		[coords addObject:[AOCCoord x:column y:y]];
	}
	return coords;
}

- (NSArray<AOCCoord *> *)coordsInRow:(NSInteger)row {
	NSMutableArray<AOCCoord *> *coords = [NSMutableArray array];
	for (NSInteger x = self.min.x; x <= self.max.x; x++) {
		[coords addObject:[AOCCoord x:x y:row]];
	}
	return coords;
}

- (BOOL)contains:(AOCCoord *)coord
{
	return 	coord.x >= self.min.x &&
			coord.x <= self.max.x &&
			coord.y >= self.min.y &&
			coord.y <= self.max.y;
}

- (AOCExtent *)inset:(NSInteger)amount
{
	NSInteger xmin = self.min.x + amount;
	NSInteger xmax = self.max.x - amount;
	NSInteger ymin = self.min.y + amount;
	NSInteger ymax = self.max.y - amount;
	
	if (xmin > xmax || ymin > ymax) { return nil; }
	
	return [AOCExtent xMin:xmin yMin:ymin xMax:xmax yMax:ymax];
}

- (NSArray<AOCExtent *> *)unionWith:(AOCExtent *)other {
	NSMutableArray<AOCExtent *> *results = [NSMutableArray array];
	
	if ([self isEqualToExtent:other]) {
		// Equal extents
		[results addObject:self];
		return results;
	}
	
	AOCExtent *intersection = [self intersectWith:other];
	if (!intersection) {
		// Disjoint extents
		[results addObject:self];
		[results addObject:other];
		return results;
	}
	
	[results addObject:intersection];

	for (AOCExtent *ext in @[self, other]) {
		if (ext.min.x < intersection.min.x) {
			if (ext.min.y < intersection.min.y) {
				AOCExtent *result = [AOCExtent min:ext.nw max:[intersection.nw offset:NW]];
				[results addObject: result];
			}
			if (ext.max.y > intersection.max.y) {
				AOCExtent *result = [AOCExtent min:[intersection.sw offset:SW] max:ext.sw];
				[results addObject: result];
			}
			// West
			AOCExtent *result = [AOCExtent xMin:ext.min.x yMin:intersection.min.y
										   xMax:intersection.min.x-1 yMax:intersection.max.y];
			[results addObject: result];
		}
		if (intersection.max.x < ext.max.x) {
			if (ext.min.y < intersection.min.y) {
				AOCExtent *result = [AOCExtent min:ext.ne max:[intersection.ne offset:NE]];
				[results addObject: result];
			}
			if (ext.max.y > intersection.max.y) {
				AOCExtent *result = [AOCExtent min:[intersection.se offset:SE] max:ext.se];
				[results addObject: result];
			}
			// East
			AOCExtent *result = [AOCExtent xMin:intersection.max.x+1 yMin:intersection.min.y
										   xMax:ext.max.x yMax:intersection.max.y];
			[results addObject: result];
		}
		if (ext.min.y < intersection.min.y) {
			// North
			AOCExtent *result = [AOCExtent xMin:intersection.min.x yMin:intersection.min.y-1
										   xMax:intersection.max.x yMax:ext.min.y];
			[results addObject: result];
		}
		if (intersection.max.y < ext.max.y) {
			// South
			AOCExtent *result = [AOCExtent xMin:intersection.min.x yMin:intersection.max.y+1
										   xMax:intersection.max.x yMax:ext.max.y];
			[results addObject: result];
		}
	}
	
	return results;
}

- (AOCExtent *)intersectWith:(AOCExtent *)other {
	NSInteger commonMinX = MAX(self.min.x, other.min.x);
	NSInteger commonMaxX = MIN(self.max.x, other.max.x);
	if (commonMaxX < commonMinX) { return nil; }
	NSInteger commonMinY = MAX(self.min.y, other.min.y);
	NSInteger commonMaxY = MIN(self.max.y, other.max.y);
	if (commonMaxY < commonMinY) { return nil; }
	
	return [[AOCExtent alloc] initMin:[AOCCoord x:commonMinX y:commonMinY]
								  max:[AOCCoord x:commonMaxX y:commonMaxY]];
}

- (NSString *)description {
	return [NSString stringWithFormat:@"{min: %@ max: %@}", self.min.description, self.max.description];
}

- (BOOL)isEqualToExtent:(AOCExtent *)other {
	if (other == nil) {
		return NO;
	}
	return [self.min isEqualToCoord:other.min] && [self.max isEqualToCoord:other.max];
}

- (BOOL)isEqual:(nullable id)object {
	if (object == nil) {
		return NO;
	}

	if (self == object) {
		return YES;
	}

	if (![object isKindOfClass:[AOCExtent class]]) {
		return NO;
	}

	return [self isEqualToExtent:(AOCExtent *)object];
}

- (NSUInteger)hash {
	return [self.min hash] ^ [self.max hash];
}
- (nonnull id)copyWithZone:(nullable NSZone *)zone { 
	AOCExtent *copy = [[AOCExtent allocWithZone:zone] initMin:self.min max:self.max];
	return copy;
}

@end
