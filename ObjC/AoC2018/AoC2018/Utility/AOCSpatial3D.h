//
//  AOCSpatial3D.h
//  AoC2018
//
//  Created by Simon Biickert .
//


@interface AOCCoord3D : NSObject <NSCopying>

+ (AOCCoord3D *)origin;
+ (AOCCoord3D *)x:(NSInteger)x y:(NSInteger)y z:(NSInteger)z;
+ (AOCCoord3D *)copyOf:(AOCCoord3D *)other;
//+ (AOCCoord *)offset:(NSString *)direction;

- (AOCCoord3D *)initX:(NSInteger)x y:(NSInteger)y z:(NSInteger)z;

@property (readonly) NSInteger x;
@property (readonly) NSInteger y;
@property (readonly) NSInteger z;

- (BOOL)isEqualToCoord3D:(AOCCoord3D *)other;

- (AOCCoord3D *)add:(AOCCoord3D *)other;
- (AOCCoord3D *)deltaTo:(AOCCoord3D *)other;
//- (AOCCoord3D *)offset:(NSString *)direction;

- (double)distanceTo:(AOCCoord3D *)other;
- (NSInteger)manhattanDistanceTo:(AOCCoord3D *)other;

@end
