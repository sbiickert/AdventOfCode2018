//
//  AOCStringsTests.m
//  AoC2018Tests
//
//  Created by Simon Biickert on 2024-02-07.
//

#import <XCTest/XCTest.h>
#import "AOCStrings.h"

@interface AOCStringsTests : XCTestCase

@end

@implementation AOCStringsTests

- (void)testBinaryStringFromInteger {
	NSString *num8 = [NSString binaryStringFromInteger:69 width:8];
	XCTAssert([num8 isEqualToString:@"01000101"]);
	NSString *num16 = [NSString binaryStringFromInteger:69 width:16];
	XCTAssert([num16 isEqualToString:@"0000000001000101"]);
}

- (void)testAllCharacters {
	NSArray<NSString *> *all = @"Hello".allCharacters;
	NSArray<NSString *> *check = @[@"H", @"e", @"l", @"l", @"o"];
	XCTAssert([all isEqualToArray:check]);
}

- (void)testIsAllDigits {
	NSString *allDigits = @"976188734";
	XCTAssert(allDigits.isAllDigits);
	allDigits = @"-114";
	XCTAssert(allDigits.isAllDigits);
	
	NSString *notAllDigits = @"0x32";
	XCTAssertFalse(notAllDigits.isAllDigits);
	notAllDigits = @"hello world";
	XCTAssertFalse(notAllDigits.isAllDigits);
}

- (void)testSplitOnSpaces {
	NSString *test = @"I am a hot dog.";
	NSArray<NSString *> *split = test.splitOnSpaces;
	XCTAssert(split.count == 5);
	XCTAssert([split[3] isEqualToString:@"hot"]);
	
	test = @"I have  a double space.";
	split = test.splitOnSpaces;
	XCTAssert(split.count == 5);
	XCTAssert([split[3] isEqualToString:@"double"]);
}

- (void)testIntsFromCSV {
	NSString *csv = @"1,2,3,4,99,100,-100";
	NSArray<NSNumber *> *numbers = csv.integersFromCSV;
	XCTAssert(numbers.count == 7);
	XCTAssert([numbers[6] isEqualToNumber:@-100]);
	
	csv = @"1,2,3,doggy,99,100,-100";
	numbers = csv.integersFromCSV;
	XCTAssert(numbers.count == 7);
	XCTAssert([numbers[3] isEqualToNumber:@0]);
}

- (void)testMatching {
	NSString *test = @"The red car is moving west at 5 km per hour.";
	NSString *pattern = @"The (\\w+) car [\\w\\s]+ (\\d+) km";
	NSArray<NSString *> *matches = [test matchPattern:pattern caseSensitive:YES];
	XCTAssert(matches.count == 3);
	XCTAssert([matches[1] isEqualToString:@"red"]);
	XCTAssert([matches[2] isEqualToString:@"5"]);
}

- (void)testMatchAndReplace {
	NSString *test = @"The red rated car is moving west at 5 km per hour.";
	NSString *pattern1 = @"\\br\\w+d\\b";
	NSString *result = [test replaceMatching:pattern1 with:@"awesome" caseSensitive:YES];
	XCTAssert([result isEqualToString:@"The awesome awesome car is moving west at 5 km per hour."]);
}

- (void)testHistogram {
	NSString *test = @"Always allow fun!";
	NSDictionary<NSString *,NSNumber *> *hist = test.histogram;
	XCTAssert([hist[@"A"] isEqualToNumber:@1]);
	XCTAssert([hist[@"l"] isEqualToNumber:@3]);
	XCTAssert([hist[@"a"] isEqualToNumber:@2]);
}

- (void)testMD5 {
	NSString *input = @"1234";
	NSString *md5 = input.md5Hex;
	XCTAssert([md5 isEqualToString:@"81dc9bdb52d04dc20036dbd8313ed055"]);
	input = @"I think, therefore I am.";
	md5 = input.md5Hex;
	XCTAssert([md5 isEqualToString:@"d86e695e0400e7c5a53fce453565d76a"]);
}

- (void)testReverse {
	NSString *fwd = @"MacBook Pro";
	NSString *rev = fwd.reverse;
	XCTAssert([rev isEqualToString:@"orP kooBcaM"]);
}

@end
