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
	NSArray<AOCSolution *> *solutions = @[ [[AOCDay00 alloc] init],
										   [[AOCDay01 alloc] init],
										   [[AOCDay02 alloc] init],
										   [[AOCDay03 alloc] init],
//										   [[AOCDay04 alloc] init],
//										   [[AOCDay05 alloc] init],
//										   [[AOCDay06 alloc] init],
//										   [[AOCDay07 alloc] init],
//										   [[AOCDay08 alloc] init],
//										   [[AOCDay09 alloc] init],
//										   [[AOCDay10 alloc] init],
//										   [[AOCDay11 alloc] init],
//										   [[AOCDay12 alloc] init],
//										   [[AOCDay13 alloc] init],
//										   [[AOCDay14 alloc] init],
//										   [[AOCDay15 alloc] init],
//										   [[AOCDay16 alloc] init],
//										   [[AOCDay17 alloc] init],
//										   [[AOCDay18 alloc] init],
//										   [[AOCDay19 alloc] init],
//										   [[AOCDay20 alloc] init],
//										   [[AOCDay21 alloc] init],
//										   [[AOCDay22 alloc] init],
//										   [[AOCDay23 alloc] init],
//										   [[AOCDay24 alloc] init],
//										   [[AOCDay25 alloc] init]
	];
	return [[solutions reverseObjectEnumerator] allObjects];
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
