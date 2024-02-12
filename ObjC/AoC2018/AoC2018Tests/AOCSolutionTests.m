//
//  AOCSolutionTests.m
//  AoC2018Tests
//
//  Created by Simon Biickert on 2024-02-07.
//

#import <XCTest/XCTest.h>
#import "AOCSolution.h"
#import "AOCInput.h"

@interface AOCSolutionTests : XCTestCase

@end

@implementation AOCSolutionTests

- (void) testListSolutions {
	NSArray<AOCSolution *> *solutions = [AOCSolution allSolutions];
	XCTAssert(solutions.count > 0);
	XCTAssert(solutions.lastObject.day == 0);
}

- (void) testSolve {
	AOCSolution *last = [AOCSolution allSolutions].lastObject;
	NSArray<AOCInput *> *inputs = [AOCInput getInputsFor:last];
	struct AOCResult r1 = [last solveInput:inputs[0]];
	NSLog(@"Part 1: %@, Part 2: %@", r1.part1, r1.part2);
	struct AOCResult r2 = [last solveInput:inputs[2]];
	NSLog(@"Part 1: %@, Part 2: %@", r2.part1, r2.part2);
}

@end
