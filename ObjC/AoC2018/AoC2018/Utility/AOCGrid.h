//
//  AOCGrid2D.h
//  AoC2015
//
//  Created by Simon Biickert on 2023-01-29.
//
#import "AOCSpatial.h"

@interface AOCGrid : NSObject

+ (AOCGrid *)grid;

- (AOCGrid *)init;
- (AOCGrid *)initWithDefault:(NSObject *)value adjacency:(NSString *)rule;

- (void)load:(NSArray<NSString *> *)data;

@property (readonly) NSString *rule;
@property (readonly) NSObject *defaultValue;
@property BOOL isTiledInfinitely;

- (NSObject *)objectAtCoord:(AOCCoord *)coord;
- (NSInteger)integerAtCoord:(AOCCoord *)coord;
- (NSString *)stringAtCoord:(AOCCoord *)coord;
- (void)setObject:(NSObject *)value atCoord:(AOCCoord *)coord;

- (void)clearAtCoord:(AOCCoord *)coord;
- (void)clearAtCoord:(AOCCoord *)coord resetExtent:(BOOL)reset;
- (void)clearAll;

- (AOCExtent *)extent;

- (NSArray<AOCCoord *> *)coords;
- (NSArray<AOCCoord *> *)coordsWithValue:(NSObject *)value;
- (NSDictionary<NSString *, NSNumber *> *)histogram;
- (NSDictionary<NSString *, NSNumber *> *)histogramIncludingDefaults;

- (NSArray<AOCCoord *> *)adjacentTo:(AOCCoord *)coord;
- (NSArray<AOCCoord *> *)adjacentTo:(AOCCoord *)coord withValue:(NSObject *)value;

- (NSString *)toString;
- (NSString *)toStringWithOverlay:(NSDictionary<AOCCoord *, NSString *> *)overlay;
- (NSString *)toStringWithOverlay:(NSDictionary<AOCCoord *, NSString *> *)overlay drawExtent:(AOCExtent *)drawExtent;
- (void)print;
- (void)printWithOverlay:(NSDictionary<AOCCoord *, NSString *> *)overlay;
- (void)printWithOverlay:(NSDictionary<AOCCoord *, NSString *> *)overlay drawExtent:(AOCExtent *)drawExtent;

- (NSDictionary<AOCCoord *, NSObject *> *)data;
- (BOOL)isEqualToGrid:(AOCGrid *)other;

@end



@protocol AOCGridRepresentable <NSObject>

- (NSString *)glyph;

@end
