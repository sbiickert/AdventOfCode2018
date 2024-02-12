//
//  AOCSpatial.h
//  AoC2018
//
//  Created by Simon Biickert .
//

extern NSString * const NORTH;
extern NSString * const SOUTH;
extern NSString * const WEST;
extern NSString * const EAST;
extern NSString * const NW;
extern NSString * const NE;
extern NSString * const SW;
extern NSString * const SE;
extern NSString * const UP;
extern NSString * const DOWN;
extern NSString * const LEFT;	// Also used for AOCDir:
extern NSString * const RIGHT;	// Also used for AOCDir:

extern NSString * const ROOK;
extern NSString * const BISHOP;
extern NSString * const QUEEN;

extern NSString * const CW;
extern NSString * const CCW;


/* *************************************************
 
 AOCCoord
 
 ************************************************* */

@interface AOCCoord : NSObject <NSCopying>

+ (AOCCoord *)origin;
+ (AOCCoord *)x:(NSInteger)x y:(NSInteger)y;
+ (AOCCoord *)copyOf:(AOCCoord *)other;
+ (AOCCoord *)offset:(NSString *)direction;
+ (NSArray<AOCCoord *> *)adjacentOffsetsWithRule:(NSString *)rule;

- (AOCCoord *)initX:(NSInteger)x y:(NSInteger)y;

@property (readonly) NSInteger x;
@property (readonly) NSInteger y;

- (NSInteger)row;
- (NSInteger)col;

- (BOOL)isEqualToCoord:(AOCCoord *)other;

- (AOCCoord *)add:(AOCCoord *)other;
- (AOCCoord *)deltaTo:(AOCCoord *)other;
- (AOCCoord *)offset:(NSString *)direction;

- (double)distanceTo:(AOCCoord *)other;
- (NSInteger)manhattanDistanceTo:(AOCCoord *)other;

- (BOOL)isAdjacentTo:(AOCCoord *)other rule:(NSString *)rule;
- (NSArray<AOCCoord *> *)adjacentCoordsWithRule:(NSString *)rule;
- (NSString *)directionTo:(AOCCoord *)other;

@end


/* *************************************************
 
 AOCDir - Container class for utility functions
 
 Directions are NSStrings with known values or aliases.
 No direction is a nil, unrecognized or empty string.
 
 ************************************************* */

@interface AOCDir : NSObject

+ (BOOL)isDirection:(NSString *)dir;
+ (NSString *)resolveAlias:(NSString *)alias;
+ (AOCCoord *)offset:(NSString *)dir;
+ (NSString *)turn:(NSString *)dir inDirection:(NSString *)turnDir;

@end


/* *************************************************
 
 AOCPos - Combination of a location and a direction.
 
 Good for puzzles where you navigate a path through a grid.
 
 ************************************************* */


@interface AOCPos : NSObject <NSCopying>

@property (readonly) AOCCoord *location;
@property (readonly) NSString *direction;

+ (AOCPos *)position:(AOCCoord *)location direction:(NSString *)dir;

- (AOCPos *)init:(AOCCoord *)location direction:(NSString *)dir;

- (BOOL)isEqualToPos:(AOCPos *)other;

- (void)turn:(NSString *)turnDir;
- (AOCPos *)turned:(NSString *)turnDir;

- (void)moveForward:(NSInteger)distance;
- (AOCPos *)movedForward:(NSInteger)distance;

@end


/* *************************************************
 
 AOCExtent - 2D box with a min (upper left) and max (lower right)
 
 ************************************************* */


@interface AOCExtent : NSObject <NSCopying>

+ (AOCExtent *)xMin:(NSInteger)xmin yMin:(NSInteger)ymin xMax:(NSInteger)xmax yMax:(NSInteger)ymax;
+ (AOCExtent *)min:(AOCCoord *)min max:(AOCCoord *)max;
+ (AOCExtent *)copyOf:(AOCExtent *)other;

- (AOCExtent *)initXMin:(NSInteger)xmin yMin:(NSInteger)ymin xMax:(NSInteger)xmax yMax:(NSInteger)ymax;
- (AOCExtent *)initMin:(AOCCoord *)min max:(AOCCoord *)max;
- (AOCExtent *)initFrom:(NSArray<AOCCoord *> *)array;

@property (readonly) AOCCoord *min;
@property (readonly) AOCCoord *max;

- (NSInteger)width;
- (NSInteger)height;
- (NSInteger)area;

- (AOCCoord *)nw;
- (AOCCoord *)ne;
- (AOCCoord *)sw;
- (AOCCoord *)se;

- (void)expandToFit:(AOCCoord *)coord;
- (AOCExtent *)expandedToFit:(AOCCoord *)coord;
- (NSArray<AOCCoord *> *)allCoords;
- (NSArray<AOCCoord *> *)coordsInColumn:(NSInteger)column;
- (NSArray<AOCCoord *> *)coordsInRow:(NSInteger)row;
- (AOCExtent *)inset:(NSInteger)amount;

- (BOOL)isEqualToExtent:(AOCExtent *)other;

- (BOOL)contains:(AOCCoord *)coord;
- (AOCExtent *)intersectWith:(AOCExtent *)other;
- (NSArray<AOCExtent *> *)unionWith:(AOCExtent *)other;

@end

