//
//  AOCInput.m
//  AoC2015
//
//  Created by Simon Biickert on 2023-01-27.
//

#import <Foundation/Foundation.h>
#import "AOCInput.h"

NSString * const INPUT_FOLDER = @"~/Developer/Advent of Code/2018/AdventOfCode2018/input";

@implementation AOCInput

+ (NSArray<AOCInput *> *)getInputsFor:(AOCSolution *)solution {
	NSMutableArray<AOCInput *> *inputs = [NSMutableArray array];
	
	// Challenge Input
	NSString *name = [AOCInput makeFilenameForDay:solution.day isTest:false];
	[inputs addObject: [[AOCInput alloc] initWithSolution:solution
												 filename:name
												  atIndex:0]];
	
	name = [AOCInput makeFilenameForDay:solution.day isTest:true];
	if (solution.emptyLinesIndicateMultipleInputs) {
		NSArray<NSArray<NSString *> *> *gInput = [AOCInput readGroupedInputFile:name];
		for (int i = 0; i < gInput.count; i++) {
			[inputs addObject:[[AOCInput alloc] initWithSolution:solution filename:name atIndex:i]];
		}
	}
	else {
		[inputs addObject:[[AOCInput alloc] initWithSolution:solution filename:name atIndex:0]];
	}
	
	return inputs;
}

+ (NSArray<AOCInput *> *)getTestsForSolution:(AOCSolution *)solution {
	NSArray<AOCInput *> *inputs = [AOCInput getInputsFor:solution];
	NSMutableArray<AOCInput *> *tests = [NSMutableArray array];
	
	for (int i = 1; i < inputs.count; i++) {
		[tests addObject: [inputs objectAtIndex:i]];
	}
	return tests;
}

+ (AOCInput *)getChallengeForSolution:(AOCSolution *)solution {
	return [[AOCInput getInputsFor:solution] objectAtIndex:0];
}


+ (NSString *)makeFilenameForDay:(int)day isTest:(BOOL)isTest {
	NSString *label = isTest ? @"test" : @"challenge";
	return [NSString stringWithFormat:@"%02d.%@.txt", day, label]; // <------ Format for input file names
}

+ (NSURL *)getInputPathFor:(NSString *)filename {
	NSString *inputFolder = [INPUT_FOLDER stringByExpandingTildeInPath];
	NSURL *folderPath = [NSURL fileURLWithPath:inputFolder isDirectory:YES];
	NSURL *filePath = [folderPath URLByAppendingPathComponent:filename];
	return filePath;
}

+ (NSArray<NSString *> *)readInputFile:(NSString *)name
					removingEmptyLines:(BOOL)removeEmptyLines {
	NSURL *url = [AOCInput getInputPathFor:name];
	
	NSError *error = nil;
	NSString *input = [NSString stringWithContentsOfFile:url.path
												encoding:NSUTF8StringEncoding
												   error:&error];
	if (error != nil) {
		NSLog(@"Error reading file %@: %@", name, error);
		return nil;
	}
	
	NSArray<NSString *> *results = [input componentsSeparatedByString:@"\n"];
	
	if (removeEmptyLines) {
		NSPredicate *p = [NSPredicate predicateWithFormat:@"SELF.length > 0"];
		return [results filteredArrayUsingPredicate:p];
	}
	return results;
}

+ (NSArray<NSString *> *)readGroupedInputFile:(NSString *)name atIndex:(int)index {
	NSArray<NSArray<NSString *> *> *groups = [AOCInput readGroupedInputFile:name];
	
	if (index < 0 || index >= groups.count) {
		return [NSArray array];
	}
	return [groups objectAtIndex:index];
}

+ (NSArray<NSArray<NSString *> *> *)readGroupedInputFile:(NSString *)name {
	NSMutableArray<NSArray<NSString *> *> *results = [NSMutableArray array];
	NSArray<NSString *> *lines = [AOCInput readInputFile:name removingEmptyLines:NO];
	
	NSMutableArray<NSString *> *group = [NSMutableArray array];
	for (NSString *line in lines) {
		if (line.length > 0) {
			[group addObject:line];
		}
		else {
			[results addObject:group];
			group = [NSMutableArray array];
		}
	}
	
	if (group.count > 0) {
		[results addObject:group];
	}
	
	return results;
}

- (AOCInput *)initWithSolution:(AOCSolution *)solution
					  filename:(NSString *)filename
					   atIndex:(int)index {
	self = [super init];
	
	_solution = solution;
	_filename = filename;
	_index = index;
	
	return self;
}

- (NSString *)getID {
	return [NSString stringWithFormat:@"%@[%d]", self.filename, self.index];
}

- (NSURL *)inputPath {
	return [AOCInput getInputPathFor:self.filename];
}

- (NSArray<NSString *> *)textLines {
	if (self.solution.emptyLinesIndicateMultipleInputs == YES) {
		return [AOCInput readGroupedInputFile:self.filename atIndex:self.index];
	}
	return [AOCInput readInputFile:self.filename removingEmptyLines:NO];
}


@end
