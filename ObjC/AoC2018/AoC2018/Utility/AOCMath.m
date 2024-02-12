//
//  AOCMath.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-05.
//

#import <Foundation/Foundation.h>
#import "AOCMath.h"

@implementation AOCMath

+ (NSInteger)gcdForX:(NSInteger)x Y:(NSInteger)y {
	NSInteger a = 0;
	NSInteger b = MAX(x,y);
	NSInteger r = MIN(x,y);
	while (r != 0) {
		a = b;
		b = r;
		r = a % b;
	}
	return b;
}

+ (NSInteger)lcmForX:(NSInteger)x Y:(NSInteger)y {
	return x / [AOCMath gcdForX:x Y:y] * y;
}

+ (NSInteger)lcmIn:(NSArray<NSNumber *>*)values{
	if (values.count == 0) { return 0; }
	NSInteger running = values.firstObject.integerValue;
	for (NSInteger i = 1; i < values.count; i++) {
		running = [AOCMath lcmForX:running Y:values[i].integerValue];
	}
	return running;
}

@end
