//
//  AOCDay24.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-07-23.
//

#import <Foundation/Foundation.h>
#import "AOCDay.h"
#import "AOCStrings.h"

@interface ArmyGroup : NSObject

- (ArmyGroup *)initFrom:(NSString *)defn inArmy:(NSString *)armyName withName:(NSString *)name;

@property (readonly) NSString *name;
@property (readonly) NSString *armyName;

@property (readonly) NSInteger initiative;
@property (readonly) NSSet<NSString *> *weaknesses;
@property (readonly) NSSet<NSString *> *immunities;
@property (readonly) NSInteger damage;
@property (readonly) NSString *damageType;

@property (readonly) NSInteger hitPoints;
@property (readwrite) NSInteger unitCount;

- (NSInteger) effectivePower;
- (NSInteger) theoreticalDamageTo:(ArmyGroup *)target;
- (void) attack:(ArmyGroup *)other;
- (void) boost:(NSInteger)amt;

@end



@interface Army : NSObject

- (Army *)initFrom:(NSArray<NSString *> *)defn;

@property (readonly) NSString *name;
@property (readonly) NSMutableArray<ArmyGroup *> *groups;

- (NSInteger) totalUnits;
- (void) boost:(NSInteger)amt;
@end



@implementation AOCDay24

- (AOCDay24 *)init {
	self = [super initWithDay:24 name:@"Immune System Simulator 20XX"];
	return self;
}

- (NSArray<Army *> *)makeArmiesFromInput:(NSArray<NSArray<NSString *> *> *)inputs {
	Army *immune = [[Army alloc] initFrom:inputs[0]];
	Army *infect = [[Army alloc] initFrom:inputs[1]];
	return @[immune, infect];
}

- (struct AOCResult)solveInputIndex:(int)index inFile:(NSString *)filename {
	struct AOCResult result = [super solveInputIndex:index inFile:filename];
	
	NSArray<NSArray<NSString *> *> *inputs = [AOCInput readGroupedInputFile:filename];
	
	result.part1 = [self solvePartOne: [self makeArmiesFromInput:inputs]];
	result.part2 = [self solvePartTwo: inputs];
	
	return result;
}

- (NSArray<NSNumber *> *)chooseTargetsIn:(NSMutableArray<ArmyGroup *> *) fightingGroups {
	NSMutableArray<NSNumber *> *result = [NSMutableArray arrayWithCapacity:fightingGroups.count];
	
	for (NSInteger attackerIndex = 0; attackerIndex < fightingGroups.count; attackerIndex++) {
		ArmyGroup *attacker = fightingGroups[attackerIndex];
		
		ArmyGroup *target = nil;
		for (NSInteger targetIndex = 0; targetIndex < fightingGroups.count; targetIndex++) {
			// On the other team and hasn't been selected yet
			if ([attacker.name hasPrefix:fightingGroups[targetIndex].armyName] == NO &&
				[result containsObject:[NSNumber numberWithInteger:targetIndex]] == NO) {
				
				NSInteger currentTargetDamage = 0;
				if (target != nil) { currentTargetDamage = [attacker theoreticalDamageTo:target]; }
				
				NSInteger targetDamage = [attacker theoreticalDamageTo:fightingGroups[targetIndex]];
				
				if (targetDamage > currentTargetDamage) {
					target = fightingGroups[targetIndex];
				}
			}
		}
		NSUInteger index = [fightingGroups indexOfObject:target];
		[result addObject:(index == NSNotFound ? @-1 : [NSNumber numberWithUnsignedInteger:index])]; // No attack is -1
	}
	return result;
}

- (void)doBattle:(NSArray<Army *> *)armies {
	NSInteger previousIterationUnitCount = NSIntegerMax; // Can get into a situation where neither team can damage each other.
	
	while (armies[0].groups.count > 0 && armies[1].groups.count > 0) {
		NSMutableArray<ArmyGroup *> *fightingGroups = [NSMutableArray arrayWithArray:armies.firstObject.groups];
		[fightingGroups addObjectsFromArray:armies.lastObject.groups];
		
		[fightingGroups sortUsingComparator:^NSComparisonResult(ArmyGroup *g1, ArmyGroup *g2) {
			if (g1.effectivePower != g2.effectivePower) {
				return [[NSNumber numberWithInteger:g2.effectivePower] compare:[NSNumber numberWithInteger:g1.effectivePower]];
			}
			return [[NSNumber numberWithInteger:g2.initiative] compare:[NSNumber numberWithInteger:g1.initiative]];
		}];
		
		// Select Targets
		NSArray<NSNumber *> *targets = [self chooseTargetsIn:fightingGroups];
		
		// Attack
		NSArray<ArmyGroup *> *groupsInAttackingOrder = [fightingGroups sortedArrayUsingComparator:^NSComparisonResult(ArmyGroup *g1, ArmyGroup *g2) {
			return [[NSNumber numberWithInteger:g2.initiative] compare:[NSNumber numberWithInteger:g1.initiative]];
		}];
		for (ArmyGroup *g in groupsInAttackingOrder) {
			NSInteger attackerIndex = [fightingGroups indexOfObject:g];
			NSInteger targetIndex = targets[attackerIndex].integerValue;
			if (targetIndex >= 0) {
				[fightingGroups[attackerIndex] attack:fightingGroups[targetIndex]];
			}
		}
		
		[armies[0].groups filterUsingPredicate:[NSPredicate predicateWithFormat:@"unitCount > 0"]];
		[armies[1].groups filterUsingPredicate:[NSPredicate predicateWithFormat:@"unitCount > 0"]];
		
		NSInteger thisIterationUnitCount = armies[0].totalUnits + armies[1].totalUnits;
		if (thisIterationUnitCount == previousIterationUnitCount) {break;}
		previousIterationUnitCount = thisIterationUnitCount;
	}
}

- (NSString *)solvePartOne:(NSArray<Army *> *)armies {
	[self doBattle:armies];
	
	Army *winner = armies[0].groups.count > 0 ? armies[0] : armies[1];
	NSInteger winningUnitCount = winner.totalUnits;
	
	return [NSString stringWithFormat: @"The winning army is %@ with %ld remaining units", winner.name, winningUnitCount];
}

- (NSString *)solvePartTwo:(NSArray<NSArray<NSString *> *> *)inputs {
	NSInteger boostAmt = 0;
	NSArray<Army *> *armies;
	
	while (YES) {
		armies = [self makeArmiesFromInput:inputs];
		boostAmt++;
		[armies.firstObject boost:boostAmt];
		
		[self doBattle:armies];
		if (armies[0].totalUnits > 0 && armies[1].totalUnits == 0) { break; }
	}
	
	Army *winner = armies[0].groups.count > 0 ? armies[0] : armies[1];
	NSInteger winningUnitCount = winner.totalUnits; // 61 is wrong
	
	return [NSString stringWithFormat: @"The winning army is %@ with %ld remaining units when boosted by %ld.", winner.name, winningUnitCount, boostAmt];
}

@end

@implementation Army

- (Army *)initFrom:(NSArray<NSString *> *)defn {
	self = [super init];
	
	_name = defn.firstObject;
	_groups = [NSMutableArray arrayWithCapacity:defn.count-1];
	
	for (NSInteger i = 1; i < defn.count; i++) {
		NSString *line = defn[i];
		ArmyGroup *group = [[ArmyGroup alloc] initFrom:line inArmy:_name withName:[NSString stringWithFormat:@"%@%ld", _name, i]];
		[_groups addObject:group];
	}
	
	[_groups sortUsingSelector:@selector(initiative)];
	
	return self;
}

- (NSInteger)totalUnits {
	NSInteger unitCount = 0;
	for (ArmyGroup *g in self.groups) {
		unitCount += g.unitCount;
	}
	return unitCount;
}

- (void)boost:(NSInteger)amt {
	for (ArmyGroup *g in self.groups) {
		[g boost:amt];
	}
}

@end


@implementation ArmyGroup

- (ArmyGroup *)initFrom:(NSString *)defn inArmy:(NSString *)armyName withName:(NSString *)name {
	self = [super init];
	
	_armyName = armyName;
	_name = name;
	
	NSArray<NSString *> *m = [defn matchPattern:@"(\\d+) units each with (\\d+) hit points (\\([\\w;, ]+\\)) with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)" caseSensitive:YES];
	
	_unitCount = [m[1] integerValue];
	_hitPoints = [m[2] integerValue];
	_damage = [m[4] integerValue];
	_damageType = m[5];
	_initiative = [m[6] integerValue];
	
	NSString *adjustments = m[3];
	
	m = [adjustments matchPattern:@"weak to ([^;)]+)" caseSensitive:YES];
	_weaknesses = [NSSet setWithArray:[m[1] componentsSeparatedByString:@", "]];
	
	m = [adjustments matchPattern:@"immune to ([^;)]+)" caseSensitive:YES];
	_immunities = [NSSet setWithArray:[m[1] componentsSeparatedByString:@", "]];

	return self;
}


- (NSInteger)effectivePower {
	return self.unitCount * self.damage;
}

- (NSInteger)theoreticalDamageTo:(ArmyGroup *)target {
	NSInteger damage = self.effectivePower;
	if ([target.immunities containsObject:self.damageType]) {
		damage = 0;
	}
	else if ([target.weaknesses containsObject:self.damageType]) {
		damage *= 2;
	}
	return damage;
}

- (void)attack:(ArmyGroup *)other {
	NSInteger damage = [self theoreticalDamageTo:other];
	NSInteger lostUnitCount = damage / other.hitPoints;
	if (lostUnitCount > other.unitCount) {
		lostUnitCount = other.unitCount;
	}
//	NSLog(@"%@ attacking %@ doing %ld damage, killing %ld units", self, other, damage, lostUnitCount);
	other.unitCount -= lostUnitCount;
//	NSLog(@"%@ has %ld units left.", other, other.unitCount);
}

- (NSString *)description {
	return [NSString stringWithFormat:@"(%@ [%ld][%ld %@]->%ld)", self.name, self.unitCount, self.damage, self.damageType, self.effectivePower];
}

- (void)boost:(NSInteger)amt {
	_damage += amt;
}

@end
