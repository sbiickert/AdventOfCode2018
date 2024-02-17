//
//  AOCArrays.m
//  AoC2015
//
//  Created by Simon Biickert on 2023-09-03.
//

#import <Foundation/Foundation.h>
#import "AOCArrays.h"

@implementation AOCArrayUtil

+ (void)sortNumbers:(NSMutableArray<NSNumber *> *)array ascending:(BOOL)asc {
	NSSortDescriptor *sd = [NSSortDescriptor sortDescriptorWithKey:@"self" ascending:asc];
	[array sortUsingDescriptors:[NSArray arrayWithObject:sd]];
}

+ (NSArray<NSNumber *>*)sortedNumbers:(NSArray<NSNumber *> *)array ascending:(BOOL)asc {
	NSSortDescriptor *sd = [NSSortDescriptor sortDescriptorWithKey:@"self" ascending:asc];
	return [array sortedArrayUsingDescriptors:[NSArray arrayWithObject:sd]];
}

+ (NSArray<NSNumber *> *)stringArrayToNumbers:(NSArray<NSString *> *)input {
	NSMutableArray<NSNumber *> *result = [NSMutableArray array];
	
	[input enumerateObjectsUsingBlock:^(NSString *obj, NSUInteger idx, BOOL *stop) {
		NSInteger i = [obj integerValue];

		// [NSString integerValue will return 0 if it fails to find integer content.
		if (i == 0 && ![[obj stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceCharacterSet] isEqualToString:@"0"]) {
			// It's zero, and we've established that the original string wasn't "0".
			NSLog(@"AOCArrayUtil::stringArrayToNumbers: Non-integer value at index %ld: '%@'", idx, obj);
		}
		
		NSNumber *n = n = [NSNumber numberWithInteger:i];
		[result addObject:n];
	}];

	return result;
}

+ (void)increment:(NSMutableArray<NSNumber *> *)array at:(NSInteger)index {
	if (array == nil || index < 0 || index >= array.count) { return; }
	
	array[index] = [NSNumber numberWithInteger:([array[index] integerValue] + 1)];
}

+ (void)decrement:(NSMutableArray<NSNumber *> *)array at:(NSInteger)index {
	if (array == nil || index < 0 || index >= array.count) { return; }
	
	array[index] = [NSNumber numberWithInteger:([array[index] integerValue] - 1)];
}


@end
