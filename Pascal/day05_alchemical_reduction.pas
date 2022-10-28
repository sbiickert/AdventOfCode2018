Program AOC_2018_Day05;
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor
{$h+}          // ANSI long strings

Uses SysUtils, StrUtils, AoCUtils, Classes, RegExpr;

Const
    //IN_FILE = '../Input/05.test.txt';
    IN_FILE = '../Input/05.challenge.txt';

Procedure SolvePart1(polymer: String);
Var
	i, len: Integer;
	letters: String;
    re: array of String;
Begin
    WriteLn('Part 1: Fully react polymer. How many units remain?');
    
    SetLength(re, 52); // 26 letters, lc and uc
    letters := 'abcdefghijklmnopqrstuvwxyz';
    
    For i := 0 To 25 Do
    Begin
    	re[i*2]   := letters[i+1] + UpperCase(letters[i+1]);
    	re[i*2+1] := UpperCase(letters[i+1]) + letters[i+1];
    End;
    
    len := 100000; // Just a big number
    While (len <> Length(polymer)) Do
    Begin
    	len := Length(polymer);
    	For i := 0 To Length(re)-1 Do
    	Begin
    		polymer := ReplaceRegExpr(re[i], polymer, '', True);
    	End;
    End;
    
    WriteLn(Format('Part One Solution: %d', [Length(polymer)]));
End;

Procedure SolvePart2(polymer: String);
Var
	i: Integer;
Begin
    WriteLn('Part 2: DESCRIPTION');
    WriteLn(Format('Part Two Solution: %d', [13]));
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
