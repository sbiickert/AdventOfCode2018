//
//  AOCDay00.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-05.
//

#import <Foundation/Foundation.h>
#import "AOCDay.h"

@implementation AOCDay00

- (AOCDay00 *)init {
	self = [super initWithDay:0 name:@"Test"];
	return self;
}

- (struct AOCResult)solveInputIndex:(int)index inFile:(NSString *)filename {
	struct AOCResult result = [super solveInputIndex:index inFile:filename];
	result.part1 = @"Hello";
	result.part2 = @"World";
	return result;
}

@end
