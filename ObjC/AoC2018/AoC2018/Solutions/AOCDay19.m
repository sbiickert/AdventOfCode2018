//
//  AOCDay19.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-21.
//

#import <Foundation/Foundation.h>
#import "AOCDay.h"
#import "AOCStrings.h"
#import "AOCArrays.h"


@interface ChronalInstruction19 : NSObject

@property (readonly) NSString *opcode;
@property (readonly) NSInteger a;
@property (readonly) NSInteger b;
@property (readonly) NSInteger c;

- (ChronalInstruction19 *)init:(NSString *)defn;
- (ChronalInstruction19 *)init:(NSString *)opcode inputA:(NSInteger)a inputB:(NSInteger)b outputC:(NSInteger)c;

@end


@interface ChronalComputer19 : NSObject

@property (readonly) NSMutableArray<NSNumber *> *registers;
@property NSInteger instructionPointer;
@property NSInteger instructionPointerRegister;
@property (readonly) NSArray<ChronalInstruction19 *> *program;

- (ChronalComputer19 *)init;

- (NSInteger)getRegisterValue:(NSInteger)r;
- (void)setValue:(NSInteger)value inRegister:(NSInteger)r;
- (void)load:(NSArray<ChronalInstruction19 *> *)program;
- (NSInteger)run;

@end



@implementation AOCDay19

- (AOCDay19 *)init {
	self = [super initWithDay:19 name:@"Go With The Flow"];
	return self;
}

- (struct AOCResult)solveInputIndex:(int)index inFile:(NSString *)filename {
	struct AOCResult result = [super solveInputIndex:index inFile:filename];
	
	NSArray<NSString *> *input = [AOCInput readGroupedInputFile:filename atIndex:index];
	
	NSInteger instructionPointerRegister = [input[0] matchPattern:@"(\\d)" caseSensitive:NO][1].integerValue;
	NSMutableArray<ChronalInstruction19 *> *instructions = [NSMutableArray array];
	for (NSInteger i = 1; i < input.count; i++) {
		[instructions addObject:[[ChronalInstruction19 alloc] init:input[i]]];
	}
	
	result.part1 = [self solvePartOne:instructions ipReg:instructionPointerRegister];
	result.part2 = [self solvePartTwo:instructions ipReg:instructionPointerRegister];
	
	return result;
}

- (NSString *)solvePartOne:(NSArray<ChronalInstruction19 *> *)program ipReg:(NSInteger)ipRegister {
	ChronalComputer19 *computer = [[ChronalComputer19 alloc] init];
	computer.instructionPointerRegister = ipRegister;
	[computer load:program];
	NSInteger code = [computer run];
	
	return [NSString stringWithFormat: @"The value in register 0 is %ld", code];
}

- (NSString *)solvePartTwo:(NSArray<ChronalInstruction19 *> *)program ipReg:(NSInteger)ipRegister {
	ChronalComputer19 *computer = [[ChronalComputer19 alloc] init];
	computer.instructionPointerRegister = ipRegister;
	[computer load:program];
	[computer setValue:1 inRegister:0];
	NSInteger code = [computer run];
	
	return [NSString stringWithFormat: @"The value in register 0 is %ld", code];
}

@end


@implementation ChronalInstruction19

- (ChronalInstruction19 *)init:(NSString *)defn {
	self = [super init];
	NSArray<NSString *> *components = [defn componentsSeparatedByString:@" "];
	return [self init:components[0] 
			   inputA:[components[1] integerValue]
			   inputB:[components[2] integerValue]
			  outputC:[components[3] integerValue]];
}

- (ChronalInstruction19 *)init:(NSString *)opcode inputA:(NSInteger)a inputB:(NSInteger)b outputC:(NSInteger)c {
	self = [super init];
	_opcode = opcode;
	_a = a;
	_b = b;
	_c = c;
	return self;
}

@end


@implementation ChronalComputer19

- (ChronalComputer19 *)init {
	self = [super init];
	_registers = @[@0, @0, @0, @0, @0, @0].mutableCopy;
	return self;
}

- (NSInteger)getRegisterValue:(NSInteger)r {
	assert(r >= 0 && r < _registers.count );
	return _registers[r].integerValue;
}

- (void)setValue:(NSInteger)value inRegister:(NSInteger)r {
	assert(r >= 0 && r < _registers.count );
	[_registers replaceObjectAtIndex:r withObject:[NSNumber numberWithInteger:value]];
}

- (void)apply:(ChronalInstruction19 *)instr {
	NSInteger c = -1;
	if ([instr.opcode isEqualToString:@"addr"]) {
		c = _registers[instr.a].integerValue + _registers[instr.b].integerValue;
	}
	else if ([instr.opcode isEqualToString:@"addi"]) {
		c = _registers[instr.a].integerValue + instr.b;
	}
	else if ([instr.opcode isEqualToString:@"mulr"]) {
		c = _registers[instr.a].integerValue * _registers[instr.b].integerValue;
	}
	else if ([instr.opcode isEqualToString:@"muli"]) {
		c = _registers[instr.a].integerValue * instr.b;
	}
	else if ([instr.opcode isEqualToString:@"banr"]) {
		c = _registers[instr.a].integerValue & _registers[instr.b].integerValue;
	}
	else if ([instr.opcode isEqualToString:@"bani"]) {
		c = _registers[instr.a].integerValue & instr.b;
	}
	else if ([instr.opcode isEqualToString:@"borr"]) {
		c = _registers[instr.a].integerValue | _registers[instr.b].integerValue;
	}
	else if ([instr.opcode isEqualToString:@"bori"]) {
		c = _registers[instr.a].integerValue | instr.b;
	}
	else if ([instr.opcode isEqualToString:@"setr"]) {
		c = _registers[instr.a].integerValue;
	}
	else if ([instr.opcode isEqualToString:@"seti"]) {
		c = instr.a;
	}
	else if ([instr.opcode isEqualToString:@"gtir"]) {
		c = (instr.a > _registers[instr.b].integerValue) ? 1 : 0;
	}
	else if ([instr.opcode isEqualToString:@"gtri"]) {
		c = (_registers[instr.a].integerValue > instr.b) ? 1 : 0;
	}
	else if ([instr.opcode isEqualToString:@"gtrr"]) {
		c = (_registers[instr.a].integerValue > _registers[instr.b].integerValue) ? 1 : 0;
	}
	else if ([instr.opcode isEqualToString:@"eqir"]) {
		c = (instr.a == _registers[instr.b].integerValue) ? 1 : 0;
	}
	else if ([instr.opcode isEqualToString:@"eqri"]) {
		c = (_registers[instr.a].integerValue == instr.b) ? 1 : 0;
	}
	else if ([instr.opcode isEqualToString:@"eqrr"]) {
		c = (_registers[instr.a].integerValue == _registers[instr.b].integerValue) ? 1 : 0;
	}

	[self setValue:c inRegister:instr.c];
}

- (void)load:(NSArray<ChronalInstruction19 *> *)program {
	_program = program;
	for (NSInteger i = 0; i < 6; i++) {	[self setValue:0 inRegister:i]; }
	self.instructionPointer = 0;
}

- (NSInteger)run {
	while (0 <= self.instructionPointer && self.instructionPointer < _program.count) {
		ChronalInstruction19 *instruction = self.program[self.instructionPointer];
		
		// When the instruction pointer is bound to a register, its value is written
		// to that register just before each instruction is executed
		[self setValue:self.instructionPointer inRegister:self.instructionPointerRegister];

		if (self.instructionPointer == 11) {
			[self shortCircuitTightInnerLoop];
		}
		
		[self apply:instruction];

		// and the value of that register is written back to the instruction pointer
		// immediately after each instruction finishes execution
		self.instructionPointer = self.registers[self.instructionPointerRegister].integerValue;
		
		// Afterward, move to the next instruction by adding one to the instruction pointer,
		// even if the value in the instruction pointer was just updated by an instruction
		self.instructionPointer++;
	}
	
	return self.registers[0].integerValue;
}

- (void)shortCircuitTightInnerLoop {
	// Short circuit of the tight inner loop
	// https://www.reddit.com/r/adventofcode/comments/a7j9zc/2018_day_19_solutions/
	/*
	 11: seti 2 6 1  			Set IP to 2
	 [0,2,0,2,10551358,1]
	 3: mulr 5 3 2				Set R2 to R5 * R3
	 [0,3,2,2,10551358,1]
	 4: eqrr 2 4 2				Set R2 to (1:0) R2 eq R4
	 [0,4,0,2,10551358,1]
	 5: addr 2 1 1				Set IP to R2 + IP. i.e. If R3*R5=R4 then instruction 7 increments R0 by R5
	 [0,5,0,2,10551358,1]
	 6: addi 1 1 1				Set IP to IP + 1
	 [0,7,0,2,10551358,1]
	 8: addi 3 1 3				Set R3 to R3 + 1
	 [0,8,0,3,10551358,1]
	 9: gtrr 3 4 2				Set R2 to (1:0) R3 eq R4 // R3 has reached the number we're factoring
	 [0,9,0,3,10551358,1]
	 10: addr 1 2 1				Set IP to IP + R2  // If R2 is not zero, then will not loop to 11
	 [0,10,0,3,10551358,1]
	 */
	// Need to get the sum of factors of the number in register[4]
	while ([self getRegisterValue:5] <= [self getRegisterValue:4]) {
		NSInteger product = [self getRegisterValue:3] * [self getRegisterValue:5];
		if (product == [self getRegisterValue:4]) {
			[self setValue:[self getRegisterValue:0]+[self getRegisterValue:5] inRegister:0];
		}
		if (product >= [self getRegisterValue:4]) {
			[self setValue:0 inRegister:3];
			[AOCArrayUtil increment:_registers at:5];
		}
		[AOCArrayUtil increment:_registers at:3];
	}
}

@end
