Program AOC_2018_Day05;
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor
{$h+}          // ANSI long strings

Uses SysUtils, StrUtils, AoCUtils, Classes, RegExpr;

Const
    //IN_FILE = '../Input/05.test.txt';
    //LETTERS = 'abcd';
    IN_FILE = '../Input/05.challenge.txt';
    LETTERS = 'abcdefghijklmnopqrstuvwxyz';

Function ReactPolymer(polymer: String; unitIgnoringPolarity: String): String;
Var
	i, len: Integer;
    re: array of String;
    letter: String;
    rrOpts: TRegexReplaceOptions;
Begin
    SetLength(re, Length(LETTERS)*2); // 26 letters, lc and uc
    
    For i := 0 To Length(LETTERS)-1 Do
    Begin
    	letter := LETTERS[i+1];
    	re[i*2]   := letter + UpperCase(letter);
    	re[i*2+1] := UpperCase(letter) + letter;
    End;
    
    If unitIgnoringPolarity <> '' Then
    Begin
    	rrOpts := [rroModifierI];
    	polymer := ReplaceRegExpr(unitIgnoringPolarity, polymer, '', rrOpts);
    End;
    
    len := 100000; // Just a big number
    While (len <> Length(polymer)) Do
    Begin
    	len := Length(polymer);
    	For i := 0 To Length(LETTERS)-1 Do
    	Begin
    		letter := LETTERS[i+1];
    		If letter <> unitIgnoringPolarity Then
    		Begin
    			polymer := ReplaceRegExpr(re[i*2], polymer, '', True);
    			polymer := ReplaceRegExpr(re[i*2+1], polymer, '', True);
    		End;
    	End;
    End;
    
    //WriteLn(polymer);
    result := polymer;
End;

Procedure SolvePart1(polymer: String);
Begin
    WriteLn('Part 1: Fully react polymer. How many units remain?');
    
    polymer := ReactPolymer(polymer, '');
    
    WriteLn(Format('Part One Solution: %d', [Length(polymer)]));
End;

Procedure SolvePart2(polymer: String);
Var
	i: Integer;
	letter, reacted, shortest: String;
Begin
    WriteLn('Part 2: Fully react polymer, trying polarity-ignoring.');

	shortest := polymer;
	
	For i := 0 To Length(LETTERS)-1 Do
	Begin
		letter := LETTERS[i+1];
		reacted := ReactPolymer(polymer, letter);
		If Length(reacted) < Length(shortest) Then
			shortest := reacted;
		WriteLn(letter, ' --> ', Length(reacted));
	End;

    WriteLn(Format('Part Two Solution: %d', [Length(shortest)]));
End;

Var
    input: TStringList;
    polymer: String;
Begin
    input := ReadInput(IN_FILE);
    polymer := input[0];
    SolvePart1(polymer);
    SolvePart2(polymer);
End.
