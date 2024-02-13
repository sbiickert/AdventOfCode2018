//
//  AOCGridTests.m
//  AoC2018Tests
//
//  Created by Simon Biickert on 2024-02-08.
//

#import <XCTest/XCTest.h>
#import "AOCGrid.h"


@interface SampleRenderable : NSObject <AOCGridRepresentable>

- (SampleRenderable *)init:(NSString *)value;
- (SampleRenderable *)init:(NSString *)value hitPoints:(NSInteger)hp;

@property NSString *value;
@property NSInteger hp;

@end

@implementation SampleRenderable

- (SampleRenderable *)init:(NSString *)value {
	self = [self init:value hitPoints:0];
	return self;
}

- (SampleRenderable *)init:(NSString *)value hitPoints:(NSInteger)hp {
	self = [super init];
	self.value = value;
	self.hp = hp;
	return self;
}

- (NSString *)glyph {
	return [self.value substringToIndex:1];
}

@end

@interface AOCGridTests : XCTestCase

@end

@implementation AOCGridTests

- (void)testCreate {
	AOCGrid *g1 = [AOCGrid grid];
	XCTAssert([g1.defaultValue isEqualTo:@"."]);
	XCTAssert([g1.rule isEqualToString:ROOK]);
	XCTAssertNil(g1.extent);
	
	AOCGrid *g2 = [[AOCGrid alloc] initWithDefault:@"x" adjacency:QUEEN];
	XCTAssert([g2.defaultValue isEqualTo:@"x"]);
	XCTAssert([g2.rule isEqualToString:QUEEN]);
	XCTAssertNil(g2.extent);
}

- (void)testValues {
	AOCGrid *g1 = [AOCGrid grid];
	[g1 setObject:@"@" atCoord:[AOCCoord origin]];
	XCTAssertNotNil(g1.extent);
	XCTAssert(g1.extent.width == 1 && g1.extent.height == 1);
	NSString *at = [g1 stringAtCoord:[AOCCoord origin]];
	XCTAssert([at isEqualToString:@"@"]);
	
	[g1 setObject:@"!" atCoord:[AOCCoord x:5 y:4]];
	XCTAssertNotNil(g1.extent);
	XCTAssert(g1.extent.width == 6 && g1.extent.height == 5);
	NSString *excl = [g1 stringAtCoord:[AOCCoord x:5 y:4]];
	XCTAssert([excl isEqualToString:@"!"]);
	
	NSString *def = [g1 stringAtCoord:[AOCCoord x:10 y:10]];
	XCTAssert([def isEqualToString:(NSString *)g1.defaultValue]);
	
	SampleRenderable *sample = [[SampleRenderable alloc] init:@"Pig"];
	[g1 setObject:sample atCoord:[AOCCoord x:-1 y:-1]];
	NSString *sampleStr = [g1 stringAtCoord:[AOCCoord x:-1 y:-1]];
	XCTAssert([sampleStr isEqualToString:@"P"]);
	SampleRenderable *sampleVal = (SampleRenderable *)[g1 objectAtCoord:[AOCCoord x:-1 y:-1]];
	XCTAssert(sampleVal == sample);
	
	NSArray<AOCCoord *> *coords = g1.coords;
	XCTAssert(coords.count == 3); // Doesn't include defaults
	
	NSDictionary<NSString *, NSNumber*> *counts = g1.histogramIncludingDefaults;
	XCTAssert(counts.count == 4);
}

- (void)testLikePerl {
	AOCGrid *grid = [AOCGrid grid];
	NSArray<AOCCoord *> *coords = @[[AOCCoord x:1 y:1], [AOCCoord x:2 y:2], [AOCCoord x:3 y:3],
		[AOCCoord x:4 y:4], [AOCCoord x:1 y:4], [AOCCoord x:2 y:4], [AOCCoord x:3 y:4]];
	[grid setObject:@"A" atCoord:coords[0]];
	[grid setObject:@"B" atCoord:coords[1]];
	[grid setObject:@"D" atCoord:coords[3]];
	
	SampleRenderable *elf = [[SampleRenderable alloc] init:@"Elf" hitPoints:100];
	SampleRenderable *gob = [[SampleRenderable alloc] init:@"Goblin" hitPoints:95];
	SampleRenderable *santa = [[SampleRenderable alloc] init:@"Santa" hitPoints:1000];
	
	[grid setObject:elf atCoord:coords[4]];
	[grid setObject:gob atCoord:coords[5]];
	[grid setObject:santa atCoord:coords[6]];
	
	XCTAssert([[grid stringAtCoord:coords[4]] isEqualToString:@"E"]);
	XCTAssert([[grid stringAtCoord:coords[5]] isEqualToString:@"G"]);
	XCTAssert([[grid stringAtCoord:coords[6]] isEqualToString:@"S"]);

	AOCExtent *ext = grid.extent;
	XCTAssert([ext.nw isEqualToCoord:[AOCCoord x:1 y:1]]);
	XCTAssert([ext.ne isEqualToCoord:[AOCCoord x:4 y:1]]);
	XCTAssert([ext.sw isEqualToCoord:[AOCCoord x:1 y:4]]);
	XCTAssert([ext.se isEqualToCoord:[AOCCoord x:4 y:4]]);
	
	NSArray<AOCCoord *> *all = grid.coords;
	XCTAssert(all.count == 6);
	NSArray<AOCCoord *> *matching = [grid coordsWithValue:@"B"];
	XCTAssert(matching.count == 1);
	XCTAssert([matching[0] isEqualToCoord:coords[1]]);
	
	[grid setObject:@"B" atCoord:coords[2]];
	NSDictionary<NSString *, NSNumber*> *hist = grid.histogram;
	XCTAssert([hist[@"A"] isEqualToNumber:@1]);
	XCTAssert([hist[@"B"] isEqualToNumber:@2]);
	XCTAssertNil(hist[@"."]);
	hist = grid.histogramIncludingDefaults;
	XCTAssert([hist[@"."] isEqualToNumber:@9]);
	
	NSArray<AOCCoord *> *n = [grid adjacentTo:coords[1]];
	XCTAssert([n[0] isEqualToCoord:[AOCCoord x:2 y:1]]);
	XCTAssert([n[1] isEqualToCoord:[AOCCoord x:3 y:2]]);
	XCTAssert([n[2] isEqualToCoord:[AOCCoord x:2 y:3]]);
	XCTAssert([n[3] isEqualToCoord:[AOCCoord x:1 y:2]]);
	
	[grid print];
	
	NSString *str = grid.toString;
	XCTAssert([str isEqualToString:@"A...\n.B..\n..B.\nEGSD\n"]);
	
	NSDictionary<AOCCoord *, NSString *> *markers = @{[AOCCoord x:4 y:1]: @"*"};
	str = [grid toStringWithOverlay:markers];
	XCTAssert([str isEqualToString:@"A..*\n.B..\n..B.\nEGSD\n"]);
	
	grid.isTiledInfinitely = YES;
	AOCExtent *drawExt = [AOCExtent xMin:0 yMin:0 xMax:5 yMax:5];
	[grid printWithOverlay:markers drawExtent:drawExt];
	str = [grid toStringWithOverlay:markers drawExtent:drawExt];
	XCTAssert([str isEqualToString:@"DEGSDE\n.A..*A\n..B...\n...B..\nDEGSDE\n.A...A\n"]);
	
	[grid clearAtCoord:coords[2]];
	XCTAssert([[grid stringAtCoord:coords[2]] isEqualToString:@"."]);
	AOCExtent *original = grid.extent;
	AOCCoord *outlier = [AOCCoord x:100 y:100];
	[grid setObject:@"X" atCoord:outlier];
	XCTAssert([grid.extent.se isEqualToCoord:outlier]);
	[grid clearAtCoord:outlier resetExtent:YES];
	XCTAssert([grid.extent isEqualToExtent:original]);
}

@end
