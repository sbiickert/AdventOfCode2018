//
//  AOCStrings.m
//  AoC2015
//
//  Created by Simon Biickert .
//

#import <Foundation/Foundation.h>
#import <CommonCrypto/CommonDigest.h>
#import "AOCStrings.h"
#import "AOCArrays.h"

@implementation NSString (AOCString)

NSString * const ALPHABET = @"abcdefghijklmnopqrstuvwxyz";

+ (NSString *)binaryStringFromInteger:(int)number width:(int)width
{
	NSMutableString * string = [[NSMutableString alloc] init];

	int binaryDigit = 0;
	int integer = number;
	
	while( binaryDigit < width )
	{
		binaryDigit++;
		[string insertString:( (integer & 1) ? @"1" : @"0" )atIndex:0];
		integer = integer >> 1;
	}
	
	return string;
}


-(NSArray<NSString *> *)allCharacters {
	NSMutableArray *characters = [[NSMutableArray alloc] initWithCapacity:[self length]];
	for (int i=0; i < [self length]; i++) {
		NSString *ichar  = [NSString stringWithFormat:@"%c", [self characterAtIndex:i]];
		[characters addObject:ichar];
	}
	return characters;
}

- (void)print
{
	printf("%s", [self cStringUsingEncoding:NSUTF8StringEncoding]);
}

- (void)println
{
	printf("%s\n", [self cStringUsingEncoding:NSUTF8StringEncoding]);
}

- (BOOL)isAllDigits
{
	NSMutableCharacterSet* nonNumbers = [[[NSCharacterSet decimalDigitCharacterSet] invertedSet] mutableCopy];
	[nonNumbers removeCharactersInString:@"-"];
	NSRange r = [self rangeOfCharacterFromSet: nonNumbers];
	return r.location == NSNotFound && self.length > 0;
}

- (NSArray<NSString *> *)splitOnSpaces {
	NSPredicate *p = [NSPredicate predicateWithBlock:^BOOL(id object, NSDictionary *bindings) {
		return [[object description] isEqualToString:@""] == NO;
	}];
	NSArray<NSString *> *strings = [[self componentsSeparatedByString:@" "] filteredArrayUsingPredicate:p];
	return strings;
}

- (NSArray<NSNumber *> *)integersFromCSV {
	NSArray<NSString *> *strings = [self componentsSeparatedByString:@","];
	NSArray<NSNumber *> *numbers = [AOCArrayUtil stringArrayToNumbers:strings];
	return numbers;
}

- (NSArray<NSString *> *)matchPattern:(NSString *)pattern caseSensitive:(BOOL)isCaseSensitive {
	NSError *err;
	NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:pattern 
																		   options:isCaseSensitive ? 0 : NSRegularExpressionCaseInsensitive
																			 error:&err];
	NSArray<NSString *> *result = nil;
	if (err == noErr) {
		result = [self match:regex];
	}
	return result;
}

- (NSArray<NSString *> *)match:(NSRegularExpression *)regex {
	NSMutableArray<NSString *> *result = nil;
	long n = [regex numberOfMatchesInString:self options:0 range:NSMakeRange(0, self.length)];
	if (n > 0) {
		NSArray *matches = [regex matchesInString:self options:0 range:NSMakeRange(0, self.length)];
		result = [NSMutableArray array];
		
		for (NSTextCheckingResult *match in matches) {
			NSString *matchText = [self substringWithRange:[match range]];
			[result addObject:matchText];
			if (regex.numberOfCaptureGroups > 0) {
				for (NSUInteger i = 1; i <= regex.numberOfCaptureGroups; i++) {
					matchText = [self substringWithRange:[match rangeAtIndex:i]];
					[result addObject:matchText];
				}
			}
		}
	}
	return result == nil ? nil : [NSArray arrayWithArray:result];
}

- (NSString *)replaceMatching:(NSString *)pattern with:(NSString *)newString caseSensitive:(BOOL)isCaseSensitive {
	NSError *err;
	NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:pattern
																		   options:isCaseSensitive ? 0 : NSRegularExpressionCaseInsensitive
																			 error:&err];
	if (err != noErr) { return nil; }
	return [self replaceMatching:regex with:newString];
}

- (NSString *)replaceMatching:(NSRegularExpression *)regex with:(NSString *)newString {
	return [regex stringByReplacingMatchesInString:self
										   options:0
											 range:NSMakeRange(0, self.length)
									  withTemplate:newString];
}


- (NSDictionary<NSString *, NSNumber *> *)histogram {
	NSDictionary<NSString *, NSNumber *> *result = [NSMutableDictionary dictionary];
	
	for (NSString *c in [self allCharacters]) {
		if ([result.allKeys containsObject:c] == NO) {
			[result setValue:@0 forKey:c];
		}
		NSInteger count = [[result valueForKey:c] integerValue] + 1;
		[result setValue:[NSNumber numberWithInteger:count] forKey:c];
	}
	
	return result;
}

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
- (NSString *)md5Hex {
	//https://stackoverflow.com/questions/2018550/how-do-i-create-an-md5-hash-of-a-string-in-cocoa
	const char *cStr = [self UTF8String];
	unsigned char result[CC_MD5_DIGEST_LENGTH];
	CC_MD5( cStr, (CC_LONG)strlen(cStr), result ); // Deprecation warning suppressed by pragmas

	char buffer[64];
	sprintf(buffer, "%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X",
		result[0],	result[1],	result[2],	result[3],
		result[4],	result[5],	result[6],	result[7],
		result[8],	result[9],	result[10],	result[11],
		result[12],	result[13],	result[14],	result[15]);
	return [[NSString stringWithCString:buffer encoding:NSUTF8StringEncoding] lowercaseString];
	
	// This is slow compared to using sprintf
//	return [[NSString stringWithFormat:@"%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X",
//		   result[0],	result[1],	result[2],	result[3],
//		   result[4],	result[5],	result[6],	result[7],
//		   result[8],	result[9],	result[10],	result[11],
//		   result[12],	result[13],	result[14],	result[15]
//	   ] lowercaseString];
}
#pragma clang diagnostic pop

- (NSString *)reverse {
	NSMutableString *reversedString = [NSMutableString stringWithCapacity:[self length]];

	[self enumerateSubstringsInRange:NSMakeRange(0,[self length])
							 options:(NSStringEnumerationReverse | NSStringEnumerationByComposedCharacterSequences)
						  usingBlock:^(NSString *substring, NSRange substringRange, NSRange enclosingRange, BOOL *stop) {
								[reversedString appendString:substring];
							}];
	return reversedString;
}

@end
