//
//  AOCMath.h
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-05.
//

@interface AOCMath : NSObject

+ (NSInteger)gcdForX:(NSInteger)x Y:(NSInteger)y;
+ (NSInteger)lcmForX:(NSInteger)x Y:(NSInteger)y;
+ (NSInteger)lcmIn:(NSArray<NSNumber *>*)values;

@end
