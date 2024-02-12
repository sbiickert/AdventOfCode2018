//
//  AOCSpatial3D.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-08.
//

#import <Foundation/Foundation.h>

//@implementation AOCCoord3D
//
//+ (AOCCoord3D *)origin
//{
//	return [[AOCCoord3D alloc] initX:0 y:0 z:0];
//}
//
//+ (AOCCoord3D *)x:(NSInteger)x y:(NSInteger)y z:(NSInteger)z
//{
//	return [[AOCCoord3D alloc] initX:x y:y z:z];
//}
//
//+ (AOCCoord3D *)copyOf:(AOCCoord3D *)other
//{
//	return [[AOCCoord3D alloc] initX:other.x y:other.y z:other.z];
//}
//
////+ (AOCCoord *)offset:(NSString *)direction;
//
//- (AOCCoord3D *)initX:(NSInteger)x y:(NSInteger)y z:(NSInteger)z
//{
//	self = [super init];
//	_x = x;
//	_y = y;
//	_z = z;
//	return self;
//}
//
//- (NSString *)description {
//	return [NSString stringWithFormat:@"[%ld,%ld,%ld]", self.x, self.y, self.z];
//}
//
//- (AOCCoord3D *)add:(AOCCoord3D *)other
//{
//	if (other == nil) {
//		return [AOCCoord3D copyOf:self];
//	}
//	return [AOCCoord3D x:self.x + other.x y:self.y + other.y z:self.z + other.z];
//}
//
//- (AOCCoord3D *)delta:(AOCCoord3D *)other
//{
//	if (other == nil) {
//		return [AOCCoord3D copyOf:self];
//	}
//	return [AOCCoord3D x:other.x - self.x
//					   y:other.y - self.y
//					   z:other.z - self.z];
//}
////- (AOCCoord3D *)offset:(NSString *)direction;
//
//- (double)distanceTo:(AOCCoord3D *)other
//{
//	NSLog(@"AOCCoord3D::distance is not implemented yet.");
//	return 0.0;
//}
//
//- (NSInteger)manhattanDistanceTo:(AOCCoord3D *)other
//{
//	AOCCoord3D *delta = [self delta:other];
//	return labs(delta.x) + labs(delta.y) + labs(delta.z);
//}
//
//- (BOOL)isEqualToCoord3D:(AOCCoord3D *)other
//{
//	if (other == nil) {
//		return NO;
//	}
//	return self.x == other.x && self.y == other.y && self.z == other.z;
//}
//
//- (BOOL)isEqual:(nullable id)object {
//	if (object == nil) {
//		return NO;
//	}
//
//	if (self == object) {
//		return YES;
//	}
//
//	if (![object isKindOfClass:[AOCCoord3D class]]) {
//		return NO;
//	}
//
//	return [self isEqualToCoord3D:(AOCCoord3D *)object];
//}
//
//- (NSUInteger)hash {
//	return [@(self.x) hash] ^ [@(self.y) hash] ^ [@(self.z) hash];
//}
//
//// NSCopying (to let this be a key in NSDictionary)
//- (id)copyWithZone:(NSZone *)zone
//{
//	AOCCoord3D *copy = [[AOCCoord3D allocWithZone:zone] initX:_x y:_y z:_z];
//	return copy;
//}
//
//@end
