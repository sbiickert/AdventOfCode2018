//
//  AOCGrid2D.m
//  AoC2015
//
//  Created by Simon Biickert on 2023-01-29.
//

#import <Foundation/Foundation.h>
#import "AOCGrid.h"
#import "AOCStrings.h"

@implementation AOCGrid {
	NSMutableDictionary<AOCCoord *, NSObject *> *_data;
	AOCExtent *_extent;
}

+ (AOCGrid *)grid
{
	return [[AOCGrid alloc] init];
}

- (AOCGrid *)init
{
	self = [self initWithDefault:@"." adjacency:ROOK];
	return self;
}
- (AOCGrid *)initWithDefault:(NSObject *)value adjacency:(NSString *)rule
{
	self = [super init];
	
	_data = [NSMutableDictionary dictionary];
	_extent = nil;
	_defaultValue = value;
	_rule = rule;
	
	return self;
}

- (void)load:(NSArray<NSString *> *)data {
	[self clearAll];
	for (NSInteger row = 0; row < data.count; row++) {
		NSArray<NSString *> *letters = data[row].allCharacters;
		for (NSInteger col = 0; col < letters.count; col++) {
			[self setObject:letters[col] atCoord:[AOCCoord x:col y:row]];
		}
	}
}

- (NSObject *)objectAtCoord:(AOCCoord *)coord
{
	if (coord == nil || [_data objectForKey:coord] == nil) {
		return self.defaultValue;
	}
	return [_data objectForKey:coord];
}

- (NSInteger)integerAtCoord:(AOCCoord *)coord
{
	NSNumber *num = (NSNumber *)[self objectAtCoord:coord];
	return [num integerValue];
}

- (NSString *)stringAtCoord:(AOCCoord *)coord
{
	NSString *str;
	id obj = [self objectAtCoord:coord];
	if ([obj isKindOfClass:[NSString class]]) {
		str = (NSString *)[self objectAtCoord:coord];
	}
	else if ([obj respondsToSelector:@selector(glyph)]) {
		str = [obj glyph];
	}
	else {
		str = [NSString stringWithFormat:@"%@", obj];
	}
	return str;
}


- (void)setObject:(NSObject *)value atCoord:(AOCCoord *)coord
{
	[_data setObject:value forKey:coord];
	if (_extent == nil) {
		_extent = [[AOCExtent alloc] initFrom:@[coord]];
	}
	else {
		[_extent expandToFit:coord];
	}
}

- (void)clearAtCoord:(AOCCoord *)coord {
	[_data removeObjectForKey:coord];
}

- (void)clearAtCoord:(AOCCoord *)coord resetExtent:(BOOL)reset {
	[self clearAtCoord: coord];
	if (reset) {
		_extent = [[AOCExtent alloc] initFrom:self.coords];
	}
}

- (void)clearAll
{
	[_data removeAllObjects];
	_extent = nil;
}

- (AOCExtent *)extent
{
	return [AOCExtent copyOf:_extent];
}

- (NSArray<AOCCoord *> *)coords
{
	return [_data allKeys];
}

- (NSArray<AOCCoord *> *)coordsWithValue:(NSObject *)value
{
	NSMutableArray<AOCCoord *> *coords = [NSMutableArray array];
	for (AOCCoord *key in [_data allKeys]) {
		if ([[_data objectForKey:key] isEqualTo:value]) {
			[coords addObject:key];
		}
	}
	return coords;
}

- (NSDictionary<NSString *, NSNumber *> *)histogram {
	NSMutableDictionary<NSString *, NSNumber *> *result = [NSMutableDictionary dictionary];
	
	AOCExtent *ext = self.extent;
	if (ext) {
		for (NSInteger row = ext.min.y; row <= ext.max.y; row++) {
			for (NSInteger col = ext.min.x; col <= ext.max.x; col++) {
				NSString *value = [self stringAtCoord:[AOCCoord x:col y:row]];
				if (![result objectForKey:value]) {
					result[value] = @0;
				}
				result[value] = [NSNumber numberWithInteger:[result[value] integerValue] + 1];
			}
		}
	}
	
	return result;
}


- (NSArray<AOCCoord *> *)adjacentTo:(AOCCoord *)coord;
{
	NSMutableArray<AOCCoord *> *a = [NSMutableArray array];
	
	NSArray<AOCCoord *> *o = [AOCCoord adjacentOffsetsWithRule:self.rule];
	for (AOCCoord *offset in o) {
		[a addObject:[coord add:offset]];
	}
	
	return a;
}

- (NSArray<AOCCoord *> *)adjacentTo:(AOCCoord *)coord withValue:(NSObject *)value
{
	NSArray<AOCCoord *> *allAdjacent = [self adjacentTo:coord];
	NSMutableArray<AOCCoord *> *result = [NSMutableArray array];
	
	for (AOCCoord *coord in allAdjacent) {
		if ([[_data objectForKey:coord] isEqualTo:value]) {
			[result addObject:coord];
		}
	}
	
	return result;
}

- (void)print
{
	[self printWithOverlay:nil drawExtent:nil];
}

- (void)printWithOverlay:(NSDictionary<AOCCoord *, NSString *> *)overlay {
	[self printWithOverlay:overlay drawExtent:nil];
}

- (void)printWithOverlay:(NSDictionary<AOCCoord *, NSString *> *)overlay 
			  drawExtent:(AOCExtent *)drawExtent {
	NSString *str = [self toStringWithOverlay:overlay
								   drawExtent:drawExtent];
	[str println];
}

- (NSString *)toString {
	return [self toStringWithOverlay:nil drawExtent:nil];
}

- (NSString *)toStringWithOverlay:(NSDictionary<AOCCoord *, NSString *> *)overlay {
	return [self toStringWithOverlay:overlay drawExtent:nil];
}

- (NSString *)toStringWithOverlay:(NSDictionary<AOCCoord *, NSString *> *)overlay 
					   drawExtent:(AOCExtent *)drawExtent {
	
	NSMutableString *str = [NSMutableString string];
	AOCExtent *ext = self.extent;
	
	if (self.isTiledInfinitely && drawExtent != nil) {
		ext = drawExtent;
	}
		
	for (NSInteger row = ext.min.y; row <= ext.max.y; row++) {
		NSMutableString *line = [NSMutableString string];
		for (NSInteger col = _extent.min.x; col <= _extent.max.x; col++) {
			AOCCoord *c = [AOCCoord x:col y:row];
			NSString *s = [self stringAtCoord:c];
			
			if (overlay != nil && [overlay objectForKey:c] != nil) {
				s = [overlay objectForKey:c];
			}
			
			[line appendString:s];
		}
		[str appendFormat:@"%@\\n", line];
	}

	return str;
}



- (NSDictionary<AOCCoord *, NSObject *> *)data
{
	return [_data copy];
}

- (BOOL)isEqualToGrid:(AOCGrid *)other
{
	return (self.defaultValue == other.defaultValue &&
			[self.rule isEqualToString:other.rule] &&
			[_data isEqualTo:other.data]);
}


@end
