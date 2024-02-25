//
//  AOCDay21.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-21.
//

#import <Foundation/Foundation.h>
#import "AOCDay.h"
#import "AOCStrings.h"
#import "AOCArrays.h"



@interface ChronalInstruction21 : NSObject

@property (readonly) NSString *opcode;
@property (readonly) NSInteger a;
@property (readonly) NSInteger b;
@property (readonly) NSInteger c;

- (ChronalInstruction21 *)init:(NSString *)defn;
- (ChronalInstruction21 *)init:(NSString *)opcode inputA:(NSInteger)a inputB:(NSInteger)b outputC:(NSInteger)c;

@end




@implementation AOCDay21

- (AOCDay21 *)init {
	self = [super initWithDay:21 name:@"Chronal Conversion"];
	return self;
}

- (struct AOCResult)solveInputIndex:(int)index inFile:(NSString *)filename {
	struct AOCResult result = [super solveInputIndex:index inFile:filename];
	
	NSArray<NSString *> *input = [AOCInput readGroupedInputFile:filename atIndex:index];
	
	NSInteger instructionPointerRegister = [input[0] matchPattern:@"(\\d)" caseSensitive:NO][1].integerValue;
	NSMutableArray<ChronalInstruction21 *> *instructions = [NSMutableArray array];
	for (NSInteger i = 1; i < input.count; i++) {
		[instructions addObject:[[ChronalInstruction21 alloc] init:input[i]]];
	}
	
	NSInteger f = 101 / 4;
	
	NSInteger magicNumber = instructions[7].a;
	
	NSInteger p1 = [self runActivationSystem:magicNumber isPartOne:YES];
	NSInteger p2 = [self runActivationSystem:magicNumber isPartOne:NO];

	result.part1 = [NSString stringWithFormat:@"The part one answer is %ld", p1]; //12502875 too high
	result.part2 = [NSString stringWithFormat:@"The part two answer is %ld", p2];

	return result;
}

// https://www.reddit.com/r/adventofcode/comments/a86jgt/comment/ec8lyck/
- (NSInteger)runActivationSystem:(NSInteger)magicNumber isPartOne:(BOOL)isPartOne {
	NSMutableSet<NSNumber *> *seen = [NSMutableSet set];
	NSInteger c = 0;
	NSInteger lastUniqueC = -1;
	
	while(YES) {
		NSInteger a = c | 65536;
		c = magicNumber;
		
		while(YES) {
			c = (((c + (a & 255)) & 16777215) * 65899) & 16777215;

			if (256 > a) {
				if (isPartOne) {
					return c;
				}
				else {
					NSNumber *cObj = [NSNumber numberWithInteger:c];
					if (![seen containsObject:cObj]) {
						[seen addObject:cObj];
						lastUniqueC = c;
						break;
					}
					else {
						return lastUniqueC;
					}
				}
			}
			else {
				a /= 256;
			}
		}
	}
}

@end


@implementation ChronalInstruction21

- (ChronalInstruction21 *)init:(NSString *)defn {
	self = [super init];
	NSArray<NSString *> *components = [defn componentsSeparatedByString:@" "];
	return [self init:components[0]
			   inputA:[components[1] integerValue]
			   inputB:[components[2] integerValue]
			  outputC:[components[3] integerValue]];
}

- (ChronalInstruction21 *)init:(NSString *)opcode inputA:(NSInteger)a inputB:(NSInteger)b outputC:(NSInteger)c {
	self = [super init];
	_opcode = opcode;
	_a = a;
	_b = b;
	_c = c;
	return self;
}

@end

