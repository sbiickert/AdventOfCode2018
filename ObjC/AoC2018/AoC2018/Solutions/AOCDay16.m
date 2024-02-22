//
//  AOCDay16.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-21.
//

#import <Foundation/Foundation.h>
#import "AOCDay.h"
#import "AOCStrings.h"
#import "AOCArrays.h"


@interface ChronalCase : NSObject

@property (readonly) NSArray<NSNumber *> *before;
@property (readonly) NSArray<NSNumber *> *after;
@property (readonly) NSArray<NSNumber *> *instruction;

- (ChronalCase *)initWithRaw:(NSArray<NSString *> *)data;

@end


@interface ChronalInstruction : NSObject

@property (readonly) NSString *opcode;
@property (readonly) NSInteger a;
@property (readonly) NSInteger b;
@property (readonly) NSInteger c;

- (ChronalInstruction *)init:(NSString *)opcode inputA:(NSInteger)a inputB:(NSInteger)b outputC:(NSInteger)c;

@end


@interface ChronalComputer : NSObject

@property (readonly) NSArray<NSNumber *> *registers;

- (ChronalComputer *)init;

- (void)setRegisterValues:(NSInteger)r0 r1:(NSInteger)r1 r2:(NSInteger)r2 r3:(NSInteger)r3;
- (NSArray<NSNumber *> *)apply:(ChronalInstruction *)instr commit:(BOOL)commit;

@end



@implementation AOCDay16

- (AOCDay16 *)init {
	self = [super initWithDay:16 name:@"Chronal Classification"];
	return self;
}

- (struct AOCResult)solveInputIndex:(int)index inFile:(NSString *)filename {
	struct AOCResult result = [super solveInputIndex:index inFile:filename];
	
	NSArray<NSArray<NSString *> *> *input = [AOCInput readGroupedInputFile:filename];
	
	NSMutableArray<ChronalCase *> *cases = [NSMutableArray array];
	NSArray<NSString *> *program;
	
	for (NSArray<NSString *> *group in input) {
		if (group.count == 0) {continue;}
		if (group.count > 3) {
			program = group;
			break;
		}
		[cases addObject:[[ChronalCase alloc] initWithRaw:group]];
	}

	NSDictionary<NSNumber *, NSString *> *opcodeLookup = nil;
	
	result.part1 = [self solvePartOne: cases outLookup:&opcodeLookup];
	
	NSArray<ChronalInstruction *> *instructions = [self convert:program lookup:opcodeLookup];
	result.part2 = [self solvePartTwo: instructions];
	
	return result;
}

- (NSString *)solvePartOne:(NSArray<ChronalCase *> *)input outLookup:(NSDictionary<NSNumber *, NSString *> **)dict {
	NSInteger threeCount = 0;
	NSArray<NSString *> *opcodes = @[@"addr", @"addi", @"mulr", @"muli",
									 @"banr", @"bani", @"borr", @"bori",
									 @"setr", @"seti", @"gtir", @"gtri", @"gtrr",
									 @"eqir", @"eqri", @"eqrr"];
	
	NSMutableDictionary<NSNumber *, NSMutableSet<NSString *> *> *lookup = [NSMutableDictionary dictionary];
	for (NSInteger i = 0; i < 16; i++) {
		[lookup setObject:[NSMutableSet setWithArray:opcodes] forKey:[NSNumber numberWithInteger:i]];
	}
	
	ChronalComputer *computer = [[ChronalComputer alloc] init];
	for (ChronalCase *cc in input) {
		NSInteger count = 0;
		for (NSString *opcode in opcodes) {
			ChronalInstruction *ci = [[ChronalInstruction alloc] init:opcode
															   inputA:cc.instruction[1].integerValue
															   inputB:cc.instruction[2].integerValue
															  outputC:cc.instruction[3].integerValue];
			[computer setRegisterValues:cc.before[0].integerValue
									 r1:cc.before[1].integerValue
									 r2:cc.before[2].integerValue
									 r3:cc.before[3].integerValue];
			NSArray<NSNumber *> *result = [computer apply:ci commit:NO];
			if ([result isEqualToArray:cc.after]) {
//				[[NSString stringWithFormat:@"[%@] --> %@ --> [%@] is valid", 
//				  [cc.before componentsJoinedByString:@","],
//				  opcode, 
//				  [cc.after componentsJoinedByString:@","]] println];
				count++;
			}
			else {
				[lookup[cc.instruction[0]] removeObject:opcode];
			}
		}
		if (count >= 3) { threeCount++; }
	}
	
	NSMutableDictionary<NSNumber *, NSString *> *outLookup = [NSMutableDictionary dictionary];
	while (lookup.count > 0) {
		for (NSNumber *code in lookup.allKeys) {
			NSMutableSet *values = lookup[code];
			if (values.count == 1) {
				NSString *opcodeValue = values.allObjects.firstObject;
				outLookup[code] = opcodeValue;
				[lookup removeObjectForKey:code];
				for (NSNumber *other in lookup.allKeys) {
					[lookup[other] removeObject:opcodeValue];
				}
				continue;
			}
		}
	}

	*dict = outLookup;
	
	return [NSString stringWithFormat: @"%ld samples behave like 3 or more opcodes", threeCount];
}

- (NSString *)solvePartTwo:(NSArray<ChronalInstruction *> *)program {
	ChronalComputer *computer = [[ChronalComputer alloc] init];

	for (ChronalInstruction *ci in program) {
		[computer apply:ci commit:YES];
	}
	
	return [NSString stringWithFormat: @"The value in register 0 is %@", computer.registers[0]];
}

- (NSArray<ChronalInstruction *> *)convert:(NSArray<NSString *> *)program lookup:(NSDictionary<NSNumber *, NSString *> *)dict {
	NSMutableArray<ChronalInstruction *> *result = [NSMutableArray array];
	
	for (NSString * line in program) {
		NSArray<NSString *> *components = [line componentsSeparatedByString:@" "];
		NSArray<NSNumber *> *n = [AOCArrayUtil stringArrayToNumbers:components];
		ChronalInstruction *ci = [[ChronalInstruction alloc] init:dict[n[0]]
														   inputA:n[1].integerValue
														   inputB:n[2].integerValue
														  outputC:n[3].integerValue];
		[result addObject:ci];
	}
	
	return result;
}

@end


@implementation ChronalCase

- (ChronalCase *)initWithRaw:(NSArray<NSString *> *)data {
	self = [super init];
	NSString *pat = @"(\\d+)\\D+(\\d+)\\D+(\\d+)\\D+(\\d+)";
	
	NSMutableArray<NSNumber *> *arr;
	NSArray<NSString *> *beforeMatch = [data[0] matchPattern:pat caseSensitive:NO];
	arr = [NSMutableArray array];
	for (int i = 1; i <= 4; i++) { [arr addObject:[NSNumber numberWithInteger:beforeMatch[i].integerValue]]; }
	_before = arr;
	
	NSArray<NSString *> *instrMatch = [data[1] matchPattern:pat caseSensitive:NO];
	arr = [NSMutableArray array];
	for (int i = 1; i <= 4; i++) { [arr addObject:[NSNumber numberWithInteger:instrMatch[i].integerValue]]; }
	_instruction = arr;

	NSArray<NSString *> *afterMatch = [data[2] matchPattern:pat caseSensitive:NO];
	arr = [NSMutableArray array];
	for (int i = 1; i <= 4; i++) { [arr addObject:[NSNumber numberWithInteger:afterMatch[i].integerValue]]; }
	_after = arr;

	return self;
}

@end



@implementation ChronalInstruction

- (ChronalInstruction *)init:(NSString *)opcode inputA:(NSInteger)a inputB:(NSInteger)b outputC:(NSInteger)c {
	self = [super init];
	_opcode = opcode;
	_a = a;
	_b = b;
	_c = c;
	return self;
}

@end


@implementation ChronalComputer

- (ChronalComputer *)init {
	self = [super init];
	_registers = @[@0, @0, @0, @0];
	return self;
}

- (void)setRegisterValues:(NSInteger)r0 r1:(NSInteger)r1 r2:(NSInteger)r2 r3:(NSInteger)r3 {
	_registers = @[[NSNumber numberWithInteger:r0], [NSNumber numberWithInteger:r1],
				   [NSNumber numberWithInteger:r2], [NSNumber numberWithInteger:r3]];
}

- (NSArray<NSNumber *> *)apply:(ChronalInstruction *)instr commit:(BOOL)commit {
	NSMutableArray<NSNumber *> *newReg = [NSMutableArray arrayWithArray:self.registers];
	
	NSInteger c = -1;
	if ([instr.opcode isEqualToString:@"addr"]) {
		c = newReg[instr.a].integerValue + newReg[instr.b].integerValue;
	}
	else if ([instr.opcode isEqualToString:@"addi"]) {
		c = newReg[instr.a].integerValue + instr.b;
	}
	else if ([instr.opcode isEqualToString:@"mulr"]) {
		c = newReg[instr.a].integerValue * newReg[instr.b].integerValue;
	}
	else if ([instr.opcode isEqualToString:@"muli"]) {
		c = newReg[instr.a].integerValue * instr.b;
	}
	else if ([instr.opcode isEqualToString:@"banr"]) {
		c = newReg[instr.a].integerValue & newReg[instr.b].integerValue;
	}
	else if ([instr.opcode isEqualToString:@"bani"]) {
		c = newReg[instr.a].integerValue & instr.b;
	}
	else if ([instr.opcode isEqualToString:@"borr"]) {
		c = newReg[instr.a].integerValue | newReg[instr.b].integerValue;
	}
	else if ([instr.opcode isEqualToString:@"bori"]) {
		c = newReg[instr.a].integerValue | instr.b;
	}
	else if ([instr.opcode isEqualToString:@"setr"]) {
		c = newReg[instr.a].integerValue;
	}
	else if ([instr.opcode isEqualToString:@"seti"]) {
		c = instr.a;
	}
	else if ([instr.opcode isEqualToString:@"gtir"]) {
		c = (instr.a > newReg[instr.b].integerValue) ? 1 : 0;
	}
	else if ([instr.opcode isEqualToString:@"gtri"]) {
		c = (newReg[instr.a].integerValue > instr.b) ? 1 : 0;
	}
	else if ([instr.opcode isEqualToString:@"gtrr"]) {
		c = (newReg[instr.a].integerValue > newReg[instr.b].integerValue) ? 1 : 0;
	}
	else if ([instr.opcode isEqualToString:@"eqir"]) {
		c = (instr.a == newReg[instr.b].integerValue) ? 1 : 0;
	}
	else if ([instr.opcode isEqualToString:@"eqri"]) {
		c = (newReg[instr.a].integerValue == instr.b) ? 1 : 0;
	}
	else if ([instr.opcode isEqualToString:@"eqrr"]) {
		c = (newReg[instr.a].integerValue == newReg[instr.b].integerValue) ? 1 : 0;
	}

	[newReg replaceObjectAtIndex:instr.c withObject:[NSNumber numberWithInteger:c]];

	if (commit) {
		_registers = newReg;
	}
	
	return newReg;
}

@end
