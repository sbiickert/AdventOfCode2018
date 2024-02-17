//
//  AOCArraysTests.m
//  AoC2018Tests
//
//  Created by Simon Biickert on 2024-02-07.
//

#import <XCTest/XCTest.h>
#import "AOCArrays.h"

@interface AOCArraysTests : XCTestCase

@end

@implementation AOCArraysTests

- (void) testStringArrayToNumbers {
	NSArray<NSString *> *goodStrings = @[@"0", @"1", @"2", @"3", @"1000", @"-5"];
	NSArray<NSString *> *badStrings = @[@"0", @"1", @"number", @"2", @"3", @"1000", @"-5"];
	
	NSArray<NSNumber *> *numbers = [AOCArrayUtil stringArrayToNumbers:goodStrings];
	NSLog(@"%@", numbers);
	XCTAssert([numbers[0] isEqualToNumber:@0]);
	XCTAssert([numbers[1] isEqualToNumber:@1]);
	XCTAssert([numbers[5] isEqualToNumber:@-5]);

	NSArray<NSNumber *> *numbersWithErr = [AOCArrayUtil stringArrayToNumbers:badStrings];
	NSLog(@"%@", numbersWithErr);
	XCTAssert([numbersWithErr[0] isEqualToNumber:@0]);
	XCTAssert([numbersWithErr[2] isEqualToNumber:@0]);
	XCTAssert([numbersWithErr[5] isEqualToNumber:@1000]);
}

- (void) testSortNumbers {
	NSArray<NSNumber *> *numbers = @[@33, @51, @19, @21, @25, @2, @18, @51, @46, @31];
	NSArray<NSNumber *> *ascCheck = @[@2, @18, @19, @21, @25, @31, @33, @46, @51, @51];
	NSArray<NSNumber *> *descCheck = @[@51, @51, @46, @33, @31, @25, @21, @19, @18, @2];

	// Create new array
	NSArray<NSNumber *> *sorted = [AOCArrayUtil sortedNumbers:numbers ascending:YES];
	XCTAssert([sorted isEqualToArray:ascCheck]);
	sorted = [AOCArrayUtil sortedNumbers:numbers ascending:NO];
	XCTAssert([sorted isEqualToArray:descCheck]);
	
	// In place
	NSMutableArray<NSNumber *> *mNumbers = [numbers mutableCopy];
	[AOCArrayUtil sortNumbers:mNumbers ascending:YES];
	XCTAssert([mNumbers isEqualToArray:ascCheck]);
	mNumbers = [numbers mutableCopy];
	[AOCArrayUtil sortNumbers:mNumbers ascending:NO];
	XCTAssert([mNumbers isEqualToArray:descCheck]);
}

- (void)testIncrementDecremen {
	NSMutableArray<NSNumber *>*numbers = [@[@1, @2, @3, @3.5] mutableCopy];
	
	[AOCArrayUtil increment:numbers at:0];
	XCTAssert([numbers[0] isEqualToNumber:@2]);
	[AOCArrayUtil increment:numbers at:3];
	XCTAssert([numbers[3] isEqualToNumber:@4]);
	
	[AOCArrayUtil decrement:numbers at:1];
	XCTAssert([numbers[1] isEqualToNumber:@1]);
	
	NSArray<NSNumber *> *copy = [[NSArray alloc] initWithArray:numbers copyItems:YES];
	[AOCArrayUtil increment:numbers at:10];
	XCTAssert([numbers isEqualToArray:copy]);
}

@end
