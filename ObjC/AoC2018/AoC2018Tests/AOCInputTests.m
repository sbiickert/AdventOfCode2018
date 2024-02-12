//
//  AOCInputTests.m
//  AOCInputTests
//
//  Created by Simon Biickert on 2024-02-05.
//

#import <XCTest/XCTest.h>
#import "AOCSolution.h"
#import "AOCInput.h"

@interface AOCInputTests : XCTestCase

@end

@implementation AOCInputTests

- (void)setUp {
    // Put setup code here. This method is called before the invocation of each test method in the class.
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
}

- (void)testInput {
	AOCSolution *s = [AOCSolution allSolutions].lastObject;
	NSArray<AOCInput *> *i = [AOCInput getInputsFor:s];
	XCTAssert(i.count == 4);
	AOCInput *challenge = i.firstObject;
	NSLog(@"%@", challenge.filename);
	XCTAssert([challenge.filename containsString:@"challenge"]);
	NSLog(@"%@", challenge.inputPath);
	NSArray<NSString *> *challengeInput = challenge.textLines;
	XCTAssert(challengeInput.count == 3);
	XCTAssert([challengeInput[1] isEqualToString:@"G0, L1"]);
	AOCInput *test1 = i[2]; // i[0] is challenge
	NSLog(@"%@", test1.filename);
	XCTAssert([test1.filename containsString:@"test"]);
	NSLog(@"%@", test1.inputPath);
	NSArray<NSString *> *testInput = test1.textLines;
	XCTAssert(testInput.count == 2);
	XCTAssert([testInput.firstObject isEqualToString:@"G1, L0"] == YES);
}

@end
