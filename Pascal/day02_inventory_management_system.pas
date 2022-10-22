Program AOC_2018_Day02;
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Uses SysUtils, StrUtils, AoCUtils, Classes;

Const
    //IN_FILE = '../Input/02.test1.txt';
    //IN_FILE = '../Input/02.test2.txt';
    IN_FILE = '../Input/02.challenge.txt';

Procedure SolvePart1(values: TStringList);
Var
    i, j: Integer;
    id, letter: String;
    hist: AoCIntegerMap;
    isTwo, isThree: Boolean;
    twoCount, threeCount: Integer;
Begin
    WriteLn('Part 1: Find the product of ids with exactly 2 and exactly 3 of a letter.');
    
    twoCount := 0;
    threeCount := 0;
    
    For i := 0 To values.Count-1 Do
    Begin
    	id := values[i];
    	hist := AoCIntegerMap.Create;
    	isTwo := False;
    	isThree := False;
    	
    	For j := 1 To Length(id) Do
    	Begin
    		letter := id[j];
    		If hist.IndexOf(letter) = -1 Then
    			hist[letter] := 0;
    		hist[letter] := hist[letter] + 1;
    	End;
    	
    	for j := 0 To hist.Count-1 Do
    	Begin
    		If hist[hist.Keys[j]] = 2 Then
    			isTwo := True;
    		If hist[hist.Keys[j]] = 3 Then
    			isThree := True;
    	End;
    	
    	If isTwo Then twoCount := twoCount + 1;
    	If isThree Then threeCount := threeCount + 1;
    	
    	hist.Free;
    End;
    
    WriteLn('Twos: ', twoCount, ' Threes: ', threeCount);
    WriteLn(Format('Part One Solution: %d', [twoCount * threeCount]));
End;

Procedure SolvePart2(values: TStringList);
Var
    i,j: Integer;
    id1, id2: String;
    ptr, diffIndex: Integer;
    commonLetters: String;
Begin
    WriteLn('Part 2: Find 2 ids that differ by one letter. Return the common letters.');
    
    commonLetters := '';
    
    For i := 0 To values.Count-2 Do
    Begin
    	For j := i+1 To values.Count-1 Do
    	Begin
    		id1 := values[i];
    		id2 := values[j];
    		diffIndex := -1;
    		For ptr := 1 To Length(id1) Do
    		Begin
    			If id1[ptr] <> id2[ptr] Then
    			Begin
    				If diffIndex = -1 Then
    					diffIndex := ptr
    				Else
    				Begin
    					diffIndex := -1;
    					Break;
    				End;
    			End;
    		End;
    		
    		If diffIndex <> -1 Then
    		Begin
    			WriteLn(id1, ', ', id2, ' diff: ', diffIndex);
    			commonLetters := LeftStr(id1, diffIndex-1);
    			commonLetters := commonLetters + MidStr(id1, diffIndex+1, 100);
    			Break;
    		End;
    	End;
    	
    	If commonLetters <> '' Then Break;
    End;
    WriteLn(Format('Part Two Solution: %s', [commonLetters]));
End;

Var
    input: TStringList;
Begin
    input := ReadInput(IN_FILE);
    SolvePart1(input);
    SolvePart2(input);
End.
