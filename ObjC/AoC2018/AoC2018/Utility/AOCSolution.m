//
//  AOCSolution.m
//  AoC2015
//
//  Created by Simon Biickert on 2023-01-27.
//

#import <Foundation/Foundation.h>
#import "AOCSolution.h"
#import "AOCDay.h"

@implementation AOCSolution

+ (NSArray<AOCSolution *> *)allSolutions {
	return @[ [[AOCDay00 alloc] init] ];
}

- (AOCSolution *)initWithDay:(int)day name:(NSString *)name {
	self = [super init];
	_day = day;
	_name = name;
	self.emptyLinesIndicateMultipleInputs = YES;
	return self;
}

- (struct AOCResult)solveInputIndex:(int)index inFile:(NSString *)filename {
	NSLog(@"Day %02d: \"%@\" with input %@ [%d]", self.day, self.name, filename, index);
	struct AOCResult result;
	result.part1 = @"";
	result.part2 = @"";
	return result;
}

- (struct AOCResult)solveInput:(AOCInput *)input {
	return [self solveInputIndex:input.index inFile:input.filename];
}

@end
