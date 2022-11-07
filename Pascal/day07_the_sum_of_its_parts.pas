Program AOC_2018_Day07;
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Uses SysUtils, StrUtils, AoCUtils, Classes, RegExpr, Math;

Const
{
    IN_FILE = '../Input/07.test.txt';
    WORKER_COUNT = 2;
    STEP_TIME = 0;
}
    IN_FILE = '../Input/07.challenge.txt';
    WORKER_COUNT = 5;
    STEP_TIME = 60;

Function GetNextStep(var prereqs: AoCStringMap): String;
Var
    i, j: Integer;
    key, keyWithNoPRs: String;
Begin
	result := '';
	
	For i := 0 To prereqs.Count-1 Do
	Begin
		key := prereqs.Keys[i];
		If Length(prereqs[key]) = 0 Then
		Begin
			keyWithNoPRs := key;
			prereqs.Remove(keyWithNoPRs);
			result := keyWithNoPRs;
			Break;
		End;
	End;
End;

Procedure ClearPrerequisite(var prereqs: AoCStringMap; toClear: String);
Var
	i: Integer;
	key: String;
Begin
	For i := 0 To prereqs.Count-1 Do
	Begin
		key := prereqs.Keys[i];
		prereqs[key] := ReplaceStr(prereqs[key], toClear, '');
	End;	
End;

Procedure SolvePart1(prereqs: AoCStringMap);
Var
	solution: String;
	step: String;
Begin
    WriteLn('Part 1: What is the correct order to do the steps?');
    
    solution := '';
    
    While prereqs.Count > 0 Do
    Begin
    	step := GetNextStep(prereqs);
    	ClearPrerequisite(prereqs, step);
    	solution := solution + step;
    	//PrintAoCStringMap(prereqs);
    End;
    
    WriteLn(Format('Part One Solution: %s', [solution]));
End;

Function TimeForStep(step: String): Integer;
Var
	i: Integer;
Begin
	For i := 1 To Length(LETTERS) Do
		If step = LETTERS[i] Then
		Begin
			result := i + STEP_TIME;
			Break;
		End;
End;

Procedure SolvePart2(prereqs: AoCStringMap);
Var
    solution, step: String;
    i, time: Integer;
    workers: AoCStringArray;
    timers: AoCIntArray;
    done: Boolean;
Begin
    WriteLn('Part 2: How long if ', WORKER_COUNT, ' workers are cooperating?');
    
    solution := '';
    time := -1;
    done := False;
    workers := []; timers := [];
    SetLength(workers, WORKER_COUNT);
    SetLength(timers, WORKER_COUNT);
    For i := 0 To WORKER_COUNT-1 Do
    Begin
    	workers[i] := ' ';
    	timers[i] := 0;
    End;
    
    While done = False Do
    Begin
    	// Time Passes
    	Inc(time);
    	For i := 0 To WORKER_COUNT-1 Do
    		timers[i] := Max(timers[i]-1, 0);    	
    	
    	// Finish Jobs
    	For i := 0 To WORKER_COUNT-1 Do
    	Begin
    		If (workers[i] <> ' ') And (timers[i] = 0) Then
    		Begin
    			solution := solution + workers[i];
    			ClearPrerequisite(prereqs, workers[i]);
    			workers[i] := ' ';
    		End;
		End;
		    	
    	// Find New Jobs
    	done := True;
    	For i := 0 To WORKER_COUNT-1 Do
    	Begin
    		If workers[i] = ' ' Then
    		Begin
    			step := GetNextStep(prereqs);
    			If step <> '' Then
    			Begin
    				workers[i] := step;
    				timers[i] := TimeForStep(step);
    				done := False;
    			End;
    		End
    		Else
    			done := False;
    	End;
    	
{
    	Write(time, ': ');
    	For i := 0 To WORKER_COUNT-1 Do
    		Write(' ', workers[i], ' ');
    	WriteLn;
}
	End;    
    
    WriteLn(Format('Part Two Solution: %d', [time]));
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
    //PrintAoCStringMap(prereqs);
    SolvePart1(prereqs);
    prereqs := ParsePrerequisites(input);
    SolvePart2(prereqs);
End.
