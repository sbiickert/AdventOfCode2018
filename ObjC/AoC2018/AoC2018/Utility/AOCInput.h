//
//  AOCInput.h
//  AoC2015
//
//  Created by Simon Biickert on 2023-01-27.
//
#import "AOCSolution.h"

extern NSString * const INPUT_FOLDER;

@interface AOCInput : NSObject

+ (NSArray<AOCInput *> *)getInputsFor:(AOCSolution *)solution;
+ (NSArray<AOCInput *> *)getTestsForSolution:(AOCSolution *)solution;
+ (AOCInput *)getChallengeForSolution:(AOCSolution *)solution;

+ (NSString *)makeFilenameForDay:(int)day isTest:(BOOL)isTest;

+ (NSURL *)getInputPathFor:(NSString *)filename;

+ (NSArray<NSString *> *)readInputFile:(NSString *)name
						 removingEmptyLines:(BOOL)removeEmptyLines;

+ (NSArray<NSString *> *)readGroupedInputFile:(NSString *)name atIndex:(int)index;

+ (NSArray<NSArray<NSString *> *> *)readGroupedInputFile:(NSString *)name;

@property (readonly) AOCSolution *solution;
@property (readonly) NSString *filename;
@property (readonly) int index;

- (AOCInput *)initWithSolution:(AOCSolution *)solution filename:(NSString *)filename atIndex:(int)index;

- (NSString *)getID;
- (NSURL *)inputPath;
- (NSArray<NSString *> *)textLines;

@end
