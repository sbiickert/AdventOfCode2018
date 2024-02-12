//
//  AOCStrings.h
//  AoC2015
//
//  Created by Simon Biickert .
//

extern NSString * const ALPHABET;

@interface NSString (AOCString)

+ (NSString *)binaryStringFromInteger:(int)number width:(int)width;
- (NSArray<NSString *> *)allCharacters;
- (void)print;
- (void)println;
- (BOOL)isAllDigits;
- (NSArray<NSString *> *)splitOnSpaces;
- (NSArray<NSNumber *> *)integersFromCSV;
- (NSArray<NSString *> *)matchPattern:(NSString *)pattern;
- (NSArray<NSString *> *)match:(NSRegularExpression *)regex;
- (NSString *)replaceMatching:(NSString *)pattern with:(NSString *)newString;
- (NSDictionary<NSString *, NSNumber *> *)histogram;
- (NSString *)md5Hex;
- (NSString *)reverse;

@end
