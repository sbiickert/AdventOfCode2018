//
//  AOCDay02.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-13.
//

#import <Foundation/Foundation.h>
#import "AOCDay.h"
#import "AOCStrings.h"

@implementation AOCDay02

- (AOCDay02 *)init {
	self = [super initWithDay:02 name:@"Inventory Management System"];
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
	NSInteger doubles = 0;
	NSInteger triples = 0;
	
	for (NSString *line in input) {
		NSDictionary<NSString *, NSNumber*> *hist = line.histogram;
		if ([hist.allValues containsObject:@2]) { doubles++; }
		if ([hist.allValues containsObject:@3]) { triples++; }
	}
	
	NSInteger checksum = doubles * triples;
	
	return [NSString stringWithFormat: @"The simple checksum is %ld", checksum];
}

- (NSString *)solvePartTwo:(NSArray<NSString *> *)input {
	NSMutableArray<NSArray<NSString *> *> *data = [NSMutableArray array];
	for (NSString *line in input) {
		[data addObject:line.allCharacters];
	}
	
	for (NSInteger i = 0; i < data.count-1; i++) {
		for (NSInteger j = i+1; j < data.count; j++) {
			NSInteger count = [self diffCount:data[i] with:data[j]];
			if (count == 1) {
				NSMutableString *common = [NSMutableString string];
				for (NSInteger idx = 0; idx < data[i].count; idx++) {
					if ([data[i][idx] isEqualToString:data[j][idx]]) {
						[common appendString:data[i][idx]];
					}
				}
				return [NSString stringWithFormat: @"The common letters are %@", common];
			}
		}
	}
	return nil;
}

- (NSInteger)diffCount:(NSArray<NSString *> *)arr1 with:(NSArray<NSString *> *)arr2 {
	NSInteger count = 0;
	for (NSInteger idx = 0; idx < arr1.count; idx++) {
		if ([arr1[idx] isEqualToString:arr2[idx]] == NO) {
			count++;
		}
	}
	return count;
}

@end
