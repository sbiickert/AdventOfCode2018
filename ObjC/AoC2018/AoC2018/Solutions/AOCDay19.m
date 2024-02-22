//
//  AOCDay19.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-21.
//

#import <Foundation/Foundation.h>
#import "AOCDay.h"

@implementation AOCDay19

- (AOCDay19 *)init {
	self = [super initWithDay:19 name:@"Go With The Flow"];
	return self;
}

- (struct AOCResult)solveInputIndex:(int)index inFile:(NSString *)filename {
	struct AOCResult result = [super solveInputIndex:index inFile:filename];
	
	NSArray<NSString *> *input = [AOCInput readGroupedInputFile:filename atIndex:index];
	
	result.part1 = [self solvePartOne: input];
	result.part2 = [self solvePartTwo: input];
	
	return result;
}

- (NSString *)solvePartOne:(NSArray<NSString *> *)input {
	
	return [NSString stringWithFormat: @"Hello"];
}

- (NSString *)solvePartTwo:(NSArray<NSString *> *)input {
	
	return [NSString stringWithFormat: @"World"];
}

@end
