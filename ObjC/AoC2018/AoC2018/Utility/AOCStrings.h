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
- (NSArray<NSString *> *)matchPattern:(NSString *)pattern caseSensitive:(BOOL)isCaseSensitive;
- (NSArray<NSString *> *)match:(NSRegularExpression *)regex;
- (NSString *)replaceMatching:(NSRegularExpression *)regex with:(NSString *)newString;
- (NSString *)replaceMatching:(NSString *)pattern with:(NSString *)newString caseSensitive:(BOOL)isCaseSensitive;
- (NSDictionary<NSString *, NSNumber *> *)histogram;
- (NSString *)md5Hex;
- (NSString *)reverse;

@end
