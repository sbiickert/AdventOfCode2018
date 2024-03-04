//
//  AOCDay22.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-25.
//

#import <Foundation/Foundation.h>
#import "AOCDay.h"
#import "AOCSpatial.h"
#import "AOCGrid.h"
#import "AOCStrings.h"
#import "AOCArrays.h"

@interface CaveBlock : NSObject <AOCGridRepresentable>

+ (NSInteger)depth;
+ (void)setDepth:(NSInteger)value ;

@property (readonly) NSInteger geoIndex;

- (CaveBlock *)init:(NSInteger)geoIndex;
- (NSInteger)erosionLevel;
- (NSInteger)regionType;
- (BOOL)canEnter:(int)activeTool;

@end



@interface CaveCost : NSObject

+ (CaveCost *)caveCost;

@property NSInteger withNone;
@property NSInteger withTorch;
@property NSInteger withClimb;

- (NSInteger)cost:(int)activeTool;
- (void)setCost:(NSInteger)cost tool:(int)activeTool;

@end


@interface CaveMove : NSObject

+ (CaveMove *)moveTo:(AOCCoord *)coord tool:(int)tool totalCost:(NSInteger)cost;
- (CaveMove *)init:(AOCCoord *)coord tool:(int)tool totalCost:(NSInteger)cost;

@property (readonly) int tool;
@property (readonly) AOCCoord *coord;
@property (readonly) NSInteger totalCost;

@end



@interface CaveMoveQ : NSObject

@property NSMutableDictionary<NSNumber *, NSMutableArray<CaveMove *> *> *q;

- (void)insert:(CaveMove *)move;
- (CaveMove *)next;
- (BOOL)isEmpty;

@end

@interface AOCGrid (Day22Grid)

- (NSInteger)cost:(AOCCoord *)coord forTool:(int)activeTool;
- (void)setCost:(NSInteger)cost at:(AOCCoord *)coord forTool:(int)activeTool;

@end


static const int TOOL_NONE = 0;
static const int TOOL_TORCH = 1;
static const int TOOL_CLIMB = 2;

@implementation AOCDay22

- (AOCDay22 *)init {
	self = [super initWithDay:22 name:@"Mode Maze"];
	return self;
}

- (struct AOCResult)solveInputIndex:(int)index inFile:(NSString *)filename {
	struct AOCResult result = [super solveInputIndex:index inFile:filename];
	
	NSArray<NSString *> *input = [AOCInput readGroupedInputFile:filename atIndex:index];
	
	AOCCoord *target = [self parseInput:input];
	AOCGrid *map = [self buildGrid:target];
	
	result.part1 = [self solvePartOne:target inMap:map];
	result.part2 = [self solvePartTwo:target inMap:map];
	
	return result;
}

- (NSString *)solvePartOne:(AOCCoord *)target inMap:(AOCGrid *)map {
	NSInteger risk = 0;
	AOCExtent *ext = [AOCExtent min:map.extent.min max:target];
	for (AOCCoord *c in ext.allCoords) {
		CaveBlock *cb = (CaveBlock *)[map objectAtCoord:c];
		risk += cb.regionType;
	}

	return [NSString stringWithFormat: @"The risk sum is %ld", risk];
}

- (NSString *)solvePartTwo:(AOCCoord *)target inMap:(AOCGrid *)map {
	AOCGrid *costGrid = [AOCGrid grid];
	for (AOCCoord *c in map.extent.allCoords) {
		CaveCost *cc = [CaveCost caveCost];
		[costGrid setObject:cc atCoord:c];
	}
	
	AOCCoord *start = [AOCCoord origin];
	
	NSInteger leastCost = [self findLeastCostPathFrom:start to:target inMap:map costGrid:costGrid];
	
	return [NSString stringWithFormat: @"The least time to reach person is %ld", leastCost];
}

- (NSInteger)findLeastCostPathFrom:(AOCCoord *)start to:(AOCCoord *)target inMap:(AOCGrid *)map costGrid:(AOCGrid *)costGrid {
	[costGrid setCost:0 at:start forTool:TOOL_TORCH];
	
	CaveMoveQ *moves = [[CaveMoveQ alloc] init];
	
	[self possibleNextMovesFrom:start inMap:map costGrid:costGrid withTool:TOOL_TORCH queue:moves];
	
	while (moves.isEmpty == NO) {
		CaveMove *cm = moves.next;
		if ([cm.coord isEqualToCoord:target]) {
			if (cm.tool == TOOL_TORCH) {
				return cm.totalCost; // 984 too low
			}
		}
		if ([costGrid cost:cm.coord forTool:cm.tool] > cm.totalCost) {
//			NSLog(@"Setting cost at %@ to %ld for tool %d", cm.coord, cm.totalCost, cm.tool);
			[costGrid setCost:cm.totalCost at:cm.coord forTool:cm.tool];
			[self possibleNextMovesFrom:cm.coord inMap:map costGrid:costGrid withTool:cm.tool queue:moves];
		}
	}
	
	return INT_MAX;
}

- (void)possibleNextMovesFrom:(AOCCoord *)loc inMap:(AOCGrid *)map costGrid:(AOCGrid *)costGrid withTool:(int)activeTool queue:(CaveMoveQ *)q {
	NSArray<AOCCoord *> *neighbors = [map adjacentTo:loc];
	NSInteger currentCost = [costGrid cost:loc forTool:activeTool];
//	NSLog(@"From %@ with cost %ld:", loc, currentCost);
	
	for (int tool = TOOL_NONE; tool <= TOOL_CLIMB; tool++) {
		NSInteger switchCost = 7;
		if (tool == activeTool) { switchCost = 0; }
		NSInteger nextCost = currentCost + 1 + switchCost;
		
		for (AOCCoord *coord in neighbors) {
			NSObject *obj = [map objectAtCoord:coord];
			if ([obj isEqual:map.defaultValue]) { continue; }
			CaveBlock *cb = (CaveBlock *)obj;
			if ([cb canEnter:tool] && nextCost < [costGrid cost:coord forTool:tool]) {
				CaveMove *cm = [CaveMove moveTo:coord tool:tool totalCost:nextCost];
//				NSLog(@"%@", cm);
				[q insert:cm];
			}
		}
	}
}

- (AOCCoord *)parseInput:(NSArray<NSString *> *)input {
	NSArray<NSString *> *m = [input[0] matchPattern:@"(\\d+)" caseSensitive:NO];
	[CaveBlock setDepth:m[1].integerValue];
	m = [input[1] matchPattern:@"(\\d+),(\\d+)" caseSensitive:NO];
	AOCCoord *c = [AOCCoord x:m[1].integerValue y:m[2].integerValue];
	return c;
}

- (AOCGrid *)buildGrid:(AOCCoord *)target {
	AOCGrid *grid = [AOCGrid grid];
	for (NSInteger x = 0; x <= target.x + 100; x++) {
		for (NSInteger y = 0; y <= target.y + 20; y++) {
			AOCCoord *c = [AOCCoord x:x y:y];
			NSInteger gi = -1;
			if (x == 0) { gi = y * 48271; }
			else if (y == 0) { gi = x * 16807; }
			else if (x == target.x && y == target.y) {
				gi = 0;
			}
			else {
				CaveBlock *cb1 = (CaveBlock *)[grid objectAtCoord:[AOCCoord x:x-1 y:y]];
				CaveBlock *cb2 = (CaveBlock *)[grid objectAtCoord:[AOCCoord x:x y:y-1]];
				gi = cb1.erosionLevel * cb2.erosionLevel;
			}
			[grid setObject:[[CaveBlock alloc] init:gi] atCoord:c];
		}
	}
	//[grid print];
	return grid;
}

@end


@implementation CaveBlock

static NSInteger _depth = 1;

+ (NSInteger)depth {
	return _depth;
}

+ (void)setDepth:(NSInteger)value {
	_depth = value;
}

- (CaveBlock *)init:(NSInteger)geoIndex {
	self = [super init];
	_geoIndex = geoIndex;
	return self;
}

- (NSInteger)erosionLevel {
	return (self.geoIndex + [CaveBlock depth]) % 20183;
}

- (NSInteger)regionType {
	return self.erosionLevel % 3;
}

- (NSString *)glyph {
	if (self.regionType == 0) { return @"."; }
	if (self.regionType == 1) { return @"="; }
	return @"|";
}

- (BOOL)canEnter:(int)activeTool {
	if (self.regionType == 0) { // Rocky
		return (activeTool & TOOL_TORCH) || (activeTool & TOOL_CLIMB);
	}
	if (self.regionType == 1) { // Wet
		return (activeTool == TOOL_NONE) || (activeTool & TOOL_CLIMB);
	}
	// Narrow
	return (activeTool == TOOL_NONE) || (activeTool & TOOL_TORCH);
}

@end


@implementation CaveCost

+ (CaveCost *)caveCost {
	return [[CaveCost alloc] init];
}

- (CaveCost *)init {
	self = [super init];
	_withNone = INT_MAX;
	_withClimb = INT_MAX;
	_withTorch = INT_MAX;
	return self;
}


- (NSInteger)cost:(int)activeTool {
	if (activeTool == 0) { return self.withNone; }
	if (activeTool & TOOL_TORCH) { return self.withTorch; }
	return self.withClimb;
}

- (void)setCost:(NSInteger)cost tool:(int)activeTool {
	if (activeTool == 0) { _withNone = cost; }
	else if (activeTool & TOOL_TORCH) { _withTorch = cost; }
	else { _withClimb = cost; }
}

@end

@implementation AOCGrid (Day22Grid)

- (NSInteger)cost:(AOCCoord *)coord forTool:(int)activeTool {
	CaveCost *cc = (CaveCost *)[self objectAtCoord:coord];
	return [cc cost:activeTool];
}

- (void)setCost:(NSInteger)cost at:(AOCCoord *)coord forTool:(int)activeTool {
	CaveCost *cc = (CaveCost *)[self objectAtCoord:coord];
	[cc setCost:cost tool:activeTool];
}

@end


@implementation CaveMove

+ (CaveMove *)moveTo:(AOCCoord *)coord tool:(int)tool totalCost:(NSInteger)cost {
	return [[CaveMove alloc] init:coord tool:tool totalCost:cost];
}

- (CaveMove *)init:(AOCCoord *)coord tool:(int)tool totalCost:(NSInteger)cost {
	self = [super init];
	_coord = coord;
	_tool = tool;
	_totalCost = cost;
	return self;
}

- (NSString *)description {
	return self.debugDescription;
}

- (NSString *)debugDescription {
	return [NSString stringWithFormat:@"--> %@ tool %d $%ld", self.coord, self.tool, self.totalCost];
}

@end



@implementation CaveMoveQ

- (CaveMoveQ *)init {
	self = [super init];
	self.q = [NSMutableDictionary dictionary];
	return self;
}
- (CaveMove *)next {
	NSNumber *cost = [AOCArrayUtil sortedNumbers:self.q.allKeys ascending:YES].firstObject;
	if (!cost) { return nil; }
	NSMutableArray<CaveMove *> *movesWithCost = self.q[cost];
	CaveMove *last = movesWithCost.lastObject;
	[movesWithCost removeLastObject];
	
	if (movesWithCost.count == 0) {
		[self.q removeObjectForKey:cost];
	}
	
	//[self print];
	return last;
}

- (void)insert:(CaveMove *)move {
	NSNumber *cost = [NSNumber numberWithInteger:move.totalCost];
	if (!self.q[cost]) {
		self.q[cost] = [NSMutableArray array];
	}
	[self.q[cost] addObject:move];
//	[self print];
}

- (BOOL)isEmpty {
	return self.q.count == 0;
}

//- (void)print {
//	if (self.isEmpty) { [@"Empty" println]; return; }
//	LLContainer *ptr = self.q;
//	while (ptr != nil) {
//		[ptr.description println];
//		ptr = ptr.next;
//	}
//}

@end
