//
//  AOCDay05.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-18.
//

#import <Foundation/Foundation.h>
#import "AOCDay.h"
#import "AOCStrings.h"

@implementation AOCDay05

- (AOCDay05 *)init {
	self = [super initWithDay:05 name:@"Alchemical Reduction"];
	return self;
}

- (struct AOCResult)solveInputIndex:(int)index inFile:(NSString *)filename {
	struct AOCResult result = [super solveInputIndex:index inFile:filename];
	
	NSArray<NSString *> *input = [AOCInput readGroupedInputFile:filename atIndex:index];
	
	NSString *polymer = input[0];
	
	result.part1 = [self solvePartOne: polymer];
	[@"end part one." println];
//	result.part2 = [self solvePartTwo: polymer];
	result.part2 = [self solvePartTwoGCD: polymer];

	return result;
}

- (NSString *)solvePartOne:(NSString *)input {
	NSString *result = [self react:input];
	NSInteger unitCount = result.length;
	return [NSString stringWithFormat: @"The resulting polymer has %ld units", unitCount];
}

- (NSString *)solvePartTwo:(NSString *)input {
	NSInteger shortest = input.length;
	NSString *removedCharacter = nil;
	
	for (NSString *l in ALPHABET.allCharacters) {
		[l println];
		NSString *polymer = [input replaceMatching:l with:@"" caseSensitive:NO];
		polymer = [self react:polymer];
		if (polymer.length < shortest) {
			shortest = polymer.length;
			removedCharacter = l;
		}
		[[l stringByAppendingFormat:@": %ld", polymer.length] println];
	}
	
	return [NSString stringWithFormat: @"Removing %@ resulted in a polymer with %ld units", removedCharacter, shortest];
}

- (NSString *)solvePartTwoGCD:(NSString *)input {
	NSMutableArray<NSNumber *> *lengths = [NSMutableArray array];

	dispatch_queue_t queue = dispatch_get_global_queue(QOS_CLASS_DEFAULT, 0);
	__weak typeof(self) weakSelf = self;
	dispatch_group_t workItems = dispatch_group_create();
	
	for (NSString *l in ALPHABET.allCharacters) {
		dispatch_block_t block = dispatch_block_create(0, ^{
			[l println];
			NSString *polymer = [input replaceMatching:l with:@"" caseSensitive:NO];
			polymer = [weakSelf react:polymer];
			[lengths addObject:[NSNumber numberWithInteger:polymer.length]];
			[[l stringByAppendingFormat:@": %ld", polymer.length] println];
			
		});
		dispatch_group_async(workItems, queue, block);
	}
	
	dispatch_group_wait(workItems, DISPATCH_TIME_FOREVER);
	
	NSInteger shortest = input.length;
	for (NSNumber *n in lengths) {
		if (n.integerValue < shortest) {
			shortest = n.integerValue;
		}
	}
	
	return [NSString stringWithFormat: @"The shortest polymer had %ld units", shortest];
}

- (NSString *)react:(NSString *)polymer {
	NSInteger len = 100000;
	
	//[polymer println];
	while (len != polymer.length) {
		len = polymer.length;
		for (NSString *l in ALPHABET.allCharacters) {
			NSString *u = l.uppercaseString;
			polymer = [polymer replaceMatching:[l stringByAppendingString:u] with:@"" caseSensitive:YES];
			polymer = [polymer replaceMatching:[u stringByAppendingString:l] with:@"" caseSensitive:YES];
		}
		//[polymer println];
	}
	return polymer;
}

@end
