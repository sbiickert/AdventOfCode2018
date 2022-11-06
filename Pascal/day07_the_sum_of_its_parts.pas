Program AOC_2018_Day07;
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Uses SysUtils, StrUtils, AoCUtils, Classes, RegExpr;

Const
    //IN_FILE = '../Input/07.test.txt';
    IN_FILE = '../Input/07.challenge.txt';

Procedure SolvePart1(prereqs: AoCStringMap);
Var
    i, j: Integer;
    key, keyWithNoPRs, solution: String;
Begin
    WriteLn('Part 1: What is the correct order to do the steps?');
    
    solution := '';
    
    While prereqs.Count > 0 Do
    Begin
    	For i := 0 To prereqs.Count-1 Do
    	Begin
    		key := prereqs.Keys[i];
    		If Length(prereqs[key]) = 0 Then
    		Begin
    			keyWithNoPRs := key;
    			solution := solution + keyWithNoPRs;
    			For j := 0 To prereqs.Count-1 Do
    			Begin
    				key := prereqs.Keys[j];
    				prereqs[key] := ReplaceStr(prereqs[key], keyWithNoPRs, '');
    			End;
    			prereqs.Remove(keyWithNoPRs);
    			Break;
    		End;
    	End;
    	//PrintAoCStringMap(prereqs);
    End;
    
    WriteLn(Format('Part One Solution: %s', [solution]));
End;

Procedure SolvePart2(prereqs: AoCStringMap);
Var
    a, b, c: Integer;
Begin
    WriteLn('Part 2: DESCRIPTION');
    WriteLn(Format('Part Two Solution: %d', [13]));
End;

Function ParsePrerequisites(input: TStringList): AoCStringMap;
Var
	i: Integer;
	re: TRegExpr;
	step, pre: String;
Begin
	result := AoCStringMap.Create;
	result.Sorted := True;

	re := TRegExpr.Create('Step (\w) must be finished before step (\w)');
	
	For i := 0 To input.Count-1 Do
		If re.Exec(input[i]) Then
		Begin
			pre := re.Match[1];
			step := re.Match[2];
			If result.IndexOf(step) = -1 Then
				result[step] := '';
			If result.IndexOf(pre) = -1 Then
				result[pre] := '';
			result[step] := result[step] + pre;
		End;
	
	re.Free;
End;

Var
    input: TStringList;
    prereqs: AoCStringMap;
Begin
    input := ReadInput(IN_FILE);
    prereqs := ParsePrerequisites(input);
    PrintAoCStringMap(prereqs);
    SolvePart1(prereqs);
    SolvePart2(prereqs);
End.
