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

@interface AOCExtent3D : NSObject <NSCopying>

+ (AOCExtent3D *)xMin:(NSInteger)xmin yMin:(NSInteger)ymin zMin:(NSInteger)zmin xMax:(NSInteger)xmax yMax:(NSInteger)ymax zMax:(NSInteger)zmax;
+ (AOCExtent3D *)min:(AOCCoord3D *)min max:(AOCCoord3D *)max;
+ (AOCExtent3D *)copyOf:(AOCExtent3D *)other;

- (AOCExtent3D *)initXMin:(NSInteger)xmin yMin:(NSInteger)ymin  zMin:(NSInteger)zmin xMax:(NSInteger)xmax yMax:(NSInteger)ymax zMax:(NSInteger)zmax;
- (AOCExtent3D *)initMin:(AOCCoord3D *)min max:(AOCCoord3D *)max;
- (AOCExtent3D *)initFrom:(NSArray<AOCCoord3D *> *)array;

@property (readonly) AOCCoord3D *min;
@property (readonly) AOCCoord3D *max;

- (NSInteger)width;
- (NSInteger)height;
- (NSInteger)depth;
- (NSInteger)volume;

- (void)expandToFit:(AOCCoord3D *)coord;
- (AOCExtent3D *)expandedToFit:(AOCCoord3D *)coord;
- (NSArray<AOCCoord3D *> *)allCoords;
- (AOCExtent3D *)inset:(NSInteger)amount;

- (BOOL)isEqualToExtent:(AOCExtent3D *)other;
- (BOOL)contains:(AOCCoord3D *)coord;
- (AOCExtent3D *)intersectWith:(AOCExtent3D *)other;

@end
