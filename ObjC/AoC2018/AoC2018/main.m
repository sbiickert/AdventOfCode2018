//
//  main.m
//  AoC2018
//
//  Created by Simon Biickert on 2024-02-04.
//

#import <Foundation/Foundation.h>
#import "AOCSolution.h"
#import "AOCInput.h"
#import "AOCDay.h"

int main(int argc, const char * argv[]) {
	@autoreleasepool {
		printf("%s", [@"Advent of Code 2018\n" cStringUsingEncoding:NSUTF8StringEncoding]);
		
		AOCSolution *s = [[AOCDay23 alloc] init];
		
//		AOCInput *i = [[AOCInput getTestsForSolution:s] objectAtIndex:1];
		AOCInput *i = [AOCInput getChallengeForSolution:s];
		
		struct AOCResult r = [s solveInputIndex:i.index inFile:i.filename];
		
		NSLog(@"Part 1: %@, Part 2: %@", r.part1, r.part2);
	}
	return 0;
}
