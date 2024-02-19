//
//  AOCSpatialTests.m
//  AoC2018Tests
//
//  Created by Simon Biickert on 2024-02-08.
//

#import <XCTest/XCTest.h>
#import "AOCSpatial.h"
#import "AOCStrings.h"

@interface AOCSpatialTests : XCTestCase

@end

@implementation AOCSpatialTests

- (void)testDir {
	NSString *dir = [AOCDir resolveAlias:@"^"];
	XCTAssert([dir isEqualToString:NORTH]);
	dir = [AOCDir resolveAlias:@"up"];
	XCTAssert([dir isEqualToString:NORTH]);
	dir = [AOCDir resolveAlias:@"L"];
	XCTAssert([dir isEqualToString:WEST]);
}

- (void)testTurn {
	NSString *left = [AOCDir turn:NORTH inDirection:CCW];
	XCTAssert([left isEqualToString:WEST]);
	
	NSString *dir = NORTH;
	for (int i = 0; i < 6; i++) {
		dir = [AOCDir turn:dir inDirection:CW];
	}
	XCTAssert([dir isEqualToString:SOUTH]);

	dir = [AOCDir turn:NORTH inDirection:@"none"];
	XCTAssert([dir isEqualToString:NORTH]);

	dir = [AOCDir resolveAlias:@"monkey"];
	XCTAssertFalse([AOCDir isDirection:dir]);
	dir = [AOCDir turn:dir inDirection:CW];
	XCTAssertFalse([AOCDir isDirection:dir]);
}

- (void)testC2DCreate {
	AOCCoord *c = [AOCCoord x:1 y:3];
	XCTAssertEqual(c.x, 1);
	XCTAssertEqual(c.y, 3);
	XCTAssertEqual(c.col, 1);
	XCTAssertEqual(c.row, 3);
}

- (void)testC2DMath {
	AOCCoord *c1 = [AOCCoord x:1 y:3];
	AOCCoord *c2 = [AOCCoord x:10 y:30];
	
	AOCCoord *sum = [c1 add:c2];
	XCTAssertEqual(sum.x, 11);
	XCTAssertEqual(sum.y, 33);

	XCTAssert([[c2 deltaTo:sum] isEqualToCoord:c1]);
	
	AOCCoord *delta = [c1 deltaTo:c2];
	XCTAssertEqual(delta.x, 9);
	XCTAssertEqual(delta.y, 27);
}

- (void)testDistance {
	AOCCoord *c1 = [AOCCoord x:1 y:3];
	AOCCoord *c2 = [AOCCoord x:10 y:30];
	
	double dist = [c1 distanceTo:c2];
	XCTAssert(dist > 28 && dist < 29);

	NSInteger md = [c1 manhattanDistanceTo:c2];
	XCTAssertEqual(md, 36);
}

- (void)testAdjacent {
	AOCCoord *c1 = [AOCCoord x: 1 y: 3];
	AOCCoord *c2 = [AOCCoord x: 10 y: 30];
	AOCCoord *horiz = [AOCCoord x: 2 y: 3];
	AOCCoord *vert = [AOCCoord x: 1 y: 2];
	AOCCoord *diag = [AOCCoord x: 2 y: 2];

	XCTAssertFalse([c1 isAdjacentTo:c2 rule:ROOK]);
	XCTAssertFalse([c1 isAdjacentTo:c2 rule:BISHOP]);
	XCTAssertFalse([c1 isAdjacentTo:c2 rule:QUEEN]);
	
	XCTAssert([c1 isAdjacentTo:horiz rule:ROOK]);
	XCTAssertFalse([c1 isAdjacentTo:horiz rule:BISHOP]);
	XCTAssert([c1 isAdjacentTo:horiz rule:QUEEN]);
	
	XCTAssert([c1 isAdjacentTo:vert rule:ROOK]);
	XCTAssertFalse([c1 isAdjacentTo:vert rule:BISHOP]);
	XCTAssert([c1 isAdjacentTo:vert rule:QUEEN]);
	
	XCTAssertFalse([c1 isAdjacentTo:diag rule:ROOK]);
	XCTAssert([c1 isAdjacentTo:diag rule:BISHOP]);
	XCTAssert([c1 isAdjacentTo:diag rule:QUEEN]);
	
	NSArray<AOCCoord *> *offsets = [AOCCoord adjacentOffsetsWithRule:ROOK];
	XCTAssert(offsets.count == 4);
	XCTAssert([offsets[0] isEqualToCoord:[AOCCoord x: 0 y: -1]]);
	offsets = [AOCCoord adjacentOffsetsWithRule:BISHOP];
	XCTAssert(offsets.count == 4);
	offsets = [AOCCoord adjacentOffsetsWithRule:QUEEN];
	XCTAssert(offsets.count == 8);
	
	NSArray<AOCCoord *> *adj = [c1 adjacentCoordsWithRule:ROOK];
	XCTAssert(adj.count == 4);
	XCTAssert([adj[0] isEqualToCoord:[AOCCoord x: 1 y: 2]]);
	adj = [c1 adjacentCoordsWithRule:BISHOP];
	XCTAssert(adj.count == 4);
	XCTAssert([adj[0] isEqualToCoord:[AOCCoord x:0 y: 2]]);
	adj = [c1 adjacentCoordsWithRule:QUEEN];
	XCTAssert(adj.count == 8);
	XCTAssert([adj[6] isEqualToCoord:[AOCCoord x: 2 y: 4]]);
}

- (void)testDirection {
	AOCCoord *c1 = [AOCCoord origin]; // 0,0
	
	AOCCoord *n = [c1 add:[AOCCoord offset:NORTH]];
	XCTAssert(n.x == 0 && n.y == -1);
	n = [c1 offset:NORTH]; // Equivalent operation
	XCTAssert(n.x == 0 && n.y == -1);
	AOCCoord *e = [c1 add:[AOCCoord offset:EAST]];
	XCTAssert(e.x == 1 && e.y == 0);
	AOCCoord *s = [c1 add:[AOCCoord offset:@"down"]];
	XCTAssert(s.x == 0 && s.y == 1);
	AOCCoord *w = [c1 add:[AOCCoord offset:WEST]];
	XCTAssert(w.x == -1 && w.y == 0);
}

- (void)testPosition {
	AOCPos *p1 = [AOCPos position:[AOCCoord origin] direction:NORTH];
	AOCPos *moved = [p1 movedForward:1];
	XCTAssert([moved.location isEqualToCoord: [p1.location offset:NORTH]]);
	moved = [p1 movedForward:1000];
	XCTAssert([moved.location isEqualToCoord:[AOCCoord x:0 y: -1000]]);
	
	AOCPos *turned = [p1 turned:CW];
	moved = [turned movedForward:1];
	XCTAssert([moved.location isEqualToCoord:[AOCCoord x:1 y:0]]);

	// In place
	[p1 moveForward:10];
	XCTAssert([p1.location isEqualToCoord:[AOCCoord x:0 y:-10]]);
	[p1 turn:CCW];
	[p1 moveForward:5];
	XCTAssert([p1.location isEqualToCoord:[AOCCoord x:-5 y:-10]]);
}

- (void)testExtentCreate {
	AOCExtent *e1 = [AOCExtent min:[AOCCoord origin] max:[AOCCoord x:10 y:5]];
	XCTAssert(e1.min.x == 0 && e1.min.y == 0 && e1.max.x == 10 && e1.max.y == 5);
	XCTAssert(e1.width == 11 && e1.height == 6);
	XCTAssert(e1.area == 66);

	AOCExtent *e2 = [AOCExtent min:[AOCCoord origin] max:[AOCCoord x:-10 y:-5]];
	XCTAssert(e2.max.x == 0 && e2.max.y == 0 && e2.min.x == -10 && e2.min.y == -5);
	XCTAssert(e2.width == 11 && e2.height == 6);
	XCTAssert(e2.area == 66);

	AOCCoord *c1 = [AOCCoord x:1 y:1];
	AOCCoord *c2 = [AOCCoord x:10 y:10];
	AOCCoord *c3 = [AOCCoord x:2 y:-5];
	AOCExtent *e3 = [[AOCExtent alloc] initFrom:@[c1, c2, c3]];
	XCTAssertNotNil(e3);
	XCTAssert(e3.min.x == 1 && e3.min.y == -5 && e3.max.x == 10 && e3.max.y == 10);
	XCTAssert(e3.width == 10 && e3.height == 16);
	XCTAssert(e3.area == 160);

	AOCExtent *nilext = [[AOCExtent alloc] initFrom:@[]];
	XCTAssert([nilext isEqualToExtent:[AOCExtent xMin:0 yMin:0 xMax:0 yMax:0]]);
	
	AOCExtent *e4 = [AOCExtent xMin:1 yMin:2 xMax:3 yMax:4];
	XCTAssert(e4.min.x == 1 && e4.min.y == 2 && e4.max.x == 3 && e4.max.y == 4);
}

- (void)testExtentExpand {
	AOCExtent *e1 = [AOCExtent min:[AOCCoord origin] max:[AOCCoord x:10 y:5]];
	AOCExtent *expanded = [e1 expandedToFit:[AOCCoord x:-5 y:6]];
	XCTAssert(expanded.min.x == -5 && expanded.min.y == 0 &&
			  expanded.max.x == 10 && expanded.max.y == 6);
	
	// In place
	[e1 expandToFit:[AOCCoord x:-5 y:6]];
	XCTAssert(e1.min.x == -5 && e1.min.y == 0 &&
			  e1.max.x == 10 && e1.max.y == 6);
}

- (void)testExtentInset {
	AOCExtent *e1 = [AOCExtent min:[AOCCoord origin] max:[AOCCoord x:10 y:5]];
	AOCExtent *inset = [e1 inset:2];
	XCTAssertNotNil(inset);
	XCTAssert(inset.min.x == 2 && inset.min.y == 2 &&
			  inset.max.x == 8 && inset.max.y == 3);
	XCTAssertNil([e1 inset: 3]);
	AOCExtent *expanded = [e1 inset:-1];
	XCTAssert(expanded.min.x == -1 && expanded.max.x == 11 && expanded.max.y == 6);
}

- (void)testExtentCoords {
	AOCExtent *e1 = [AOCExtent min:[AOCCoord origin] max:[AOCCoord x:5 y:6]];
	NSArray<AOCCoord *> *coords = e1.allCoords;
	XCTAssertEqual(coords.count, e1.area);
	XCTAssert([coords[0] isEqualToCoord:[AOCCoord origin]]);
	XCTAssert([coords.lastObject isEqualToCoord:[AOCCoord x:5 y:6]]);
	
	coords = e1.edgeCoords;
	XCTAssertEqual(coords.count, 22);
}

- (void)testExtentRelations {
	AOCExtent *e1 = [AOCExtent min:[AOCCoord origin] max:[AOCCoord x:10 y:5]];

	XCTAssert([e1 contains:[AOCCoord x:1 y:1]]);
	XCTAssert([e1 contains:[AOCCoord x:10 y:1]]);
	XCTAssertFalse([e1 contains:[AOCCoord x:11 y:1]]);
	XCTAssertFalse([e1 contains:[AOCCoord x:-1 y:-1]]);
	
	AOCExtent *e = [AOCExtent xMin:1 yMin:1 xMax:10 yMax:10];
	AOCExtent *i = [e intersectWith:[AOCExtent xMin:5 yMin:5 xMax:12 yMax:12]];
	XCTAssert([i isEqualToExtent:[AOCExtent xMin:5 yMin:5 xMax:10 yMax:10]]);
	i = [e intersectWith:[AOCExtent xMin:5 yMin:5 xMax:7 yMax:7]];
	XCTAssert([i isEqualToExtent:[AOCExtent xMin:5 yMin:5 xMax:7 yMax:7]]);
	i = [e intersectWith:[AOCExtent xMin:1 yMin:1 xMax:12 yMax:2]];
	XCTAssert([i isEqualToExtent:[AOCExtent xMin:1 yMin:1 xMax:10 yMax:2]]);
	i = [e intersectWith:[AOCExtent xMin:11 yMin:11 xMax:12 yMax:12]];
	XCTAssertNil(i);
	i = [e intersectWith:[AOCExtent xMin:1 yMin:10 xMax:10 yMax:20]];
	XCTAssert([i isEqualToExtent:[AOCExtent xMin:1 yMin:10 xMax:10 yMax:10]]);
	
	NSArray<AOCExtent *> *products = [e unionWith:[AOCExtent xMin:5 yMin:5 xMax:12 yMax:12]];
	NSArray<AOCExtent *> *expected = @[[AOCExtent xMin:5 yMin:5 xMax:10 yMax:10],
										[AOCExtent xMin:1 yMin:1 xMax:4 yMax:4],
										[AOCExtent xMin:1 yMin:5 xMax:4 yMax:10],
										[AOCExtent xMin:5 yMin:1 xMax:10 yMax:4],
										[AOCExtent xMin:11 yMin:11 xMax:12 yMax:12],
										[AOCExtent xMin:11 yMin:5 xMax:12 yMax:10],
										[AOCExtent xMin:5 yMin:11 xMax:10 yMax:12]];
	XCTAssertTrue([self verifyUnionResults: products expected:expected]);
	products = [e unionWith:[AOCExtent xMin:5 yMin:5 xMax:7 yMax:7]];
	expected = @[[AOCExtent xMin:5 yMin:5 xMax:7 yMax:7],
				[AOCExtent xMin:1 yMin:1 xMax:4 yMax:4],
				[AOCExtent xMin:1 yMin:8 xMax:4 yMax:10],
				[AOCExtent xMin:1 yMin:5 xMax:4 yMax:7],
				[AOCExtent xMin:8 yMin:1 xMax:10 yMax:4],
				[AOCExtent xMin:8 yMin:8 xMax:10 yMax:10],
				[AOCExtent xMin:8 yMin:5 xMax:10 yMax:7],
				[AOCExtent xMin:5 yMin:1 xMax:7 yMax:4],
				[AOCExtent xMin:5 yMin:8 xMax:7 yMax:10]];
	XCTAssertTrue([self verifyUnionResults: products expected:expected]);
	products = [e unionWith:[AOCExtent xMin:1 yMin:1 xMax:12 yMax:2]];
	expected = @[[AOCExtent xMin:1 yMin:1 xMax:10 yMax:2],
				[AOCExtent xMin:1 yMin:3 xMax:10 yMax:10],
				[AOCExtent xMin:11 yMin:1 xMax:12 yMax:2]];
	XCTAssertTrue([self verifyUnionResults: products expected:expected]);
	products = [e unionWith:[AOCExtent xMin:11 yMin:11 xMax:12 yMax:12]];
//	expected = @[[AOCExtent xMin:11 yMin:11 xMax:12 yMax:12]];
	expected = @[[AOCExtent xMin:1 yMin:1 xMax:10 yMax:10],
				[AOCExtent xMin:11 yMin:11 xMax:12 yMax:12]];
	XCTAssertTrue([self verifyUnionResults: products expected:expected]);
	products = [e unionWith:[AOCExtent xMin:1 yMin:10 xMax:10 yMax:20]];
	expected = @[[AOCExtent xMin:1 yMin:10 xMax:10 yMax:10],
				[AOCExtent xMin:1 yMin:1 xMax:10 yMax:9],
				[AOCExtent xMin:1 yMin:11 xMax:10 yMax:20]];
	XCTAssertTrue([self verifyUnionResults: products expected:expected]);
}


- (BOOL)verifyUnionResults:(NSArray<AOCExtent *> *)actual expected:(NSArray<AOCExtent *> *)expected {
	if (actual.count != expected.count) { return NO; }
	for (NSInteger i = 0; i < actual.count; i++) {
		if (![actual[i] isEqualToExtent:expected[i]]) {
			[[NSString stringWithFormat:@"actual[%ld]: %@, expected[%ld]: %@", i, actual[i], i, expected[i]] println];
			return NO;
		}
	}
	return YES;
}


@end
