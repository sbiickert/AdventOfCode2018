//
//  AOCArrays.h
//  AoC2015
//
//  Created by Simon Biickert on 2023-09-03.
//

@interface AOCArrayUtil : NSObject

+ (NSArray<NSNumber *> *)stringArrayToNumbers:(NSArray<NSString *> *)input;
+ (void)sortNumbers:(NSMutableArray<NSNumber *> *)array ascending:(BOOL)asc;
+ (NSArray<NSNumber *> *)sortedNumbers:(NSArray<NSNumber *> *)array ascending:(BOOL)asc;

@end

@interface NSArray (AOCArray)

@end

@interface NSMutableArray (AOCMutableArray)

@end
