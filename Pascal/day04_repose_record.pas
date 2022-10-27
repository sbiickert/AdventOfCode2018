Program AOC_2018_Day04;
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Uses SysUtils, StrUtils, AoCUtils, Classes, RegExpr, DateUtils;

Const
    //IN_FILE = '../Input/04.test.txt';
    IN_FILE = '../Input/04.challenge.txt';

Type
	GuardEventType = (start, wake, sleep);
	GuardEvent = Record
		guardID: Integer;
		time: TDateTime;
		eType: GuardEventType;
	End;
	GuardEventArray = Array of GuardEvent;
	
Procedure SolvePart1(events: GuardEventArray);
Var
    i, mostHours, m, freq, sleepiestGuardID: Integer;
    map, hist: AoCIntegerMap;
    diff: Integer;
    key, mStr: String;
Begin
    WriteLn('Part 1: Find the guard who slept the most and when he slept the most');
    
    map := AoCIntegerMap.Create;
    
    For i := 0 To Length(events) Do
    	If events[i].eType = sleep Then
    	Begin
			diff := MinuteOf(events[i+1].time) - MinuteOf(events[i].time);
			
			key := IntToStr(events[i].guardID);
			If map.IndexOf(key) = -1 Then
				map[key] := 0;
			map[key] := map[key] + diff;
		End;
		
	PrintAoCIntegerMap(map);
	
	mostHours := 0;
	For i := 0 To map.Count-1 Do
		If map[map.Keys[i]] > mostHours Then
		Begin
			key := map.Keys[i];
			mostHours := map[key];
		End;
	
    map.Free;
	
	// key is the id of the sleepiest guard
	sleepiestGuardID := StrToInt(key);
	WriteLn('Guard #', key, ' is the sleepiest.');
	
	hist := AoCIntegerMap.Create;
	
	For i := 0 To Length(events)-1 Do
	Begin
		If (events[i].eType = sleep) And (events[i].guardID = sleepiestGuardID) Then
		Begin
			For m := MinuteOf(events[i].time) To MinuteOf(events[i+1].time)-1 Do
			Begin
				mStr := IntToStr(m);
				If hist.IndexOf(mStr) = -1 Then
					hist[mStr] := 0;
				hist[mStr] := hist[mStr] + 1;
			End;
		End;
	End;
	
	PrintAoCIntegerMap(hist);
	
	freq := 0;
	For i := 0 To hist.Count-1 Do
		If hist[hist.Keys[i]] > freq Then
		Begin
			key := hist.Keys[i];
			freq := hist[key];
		End;
		
	WriteLn('The guard slept most at minute ', key);
	
	hist.Free;
    
    WriteLn(Format('Part One Solution: %d', [StrToInt(key) * sleepiestGuardID]));
End;

Procedure SolvePart2(events: GuardEventArray);
Var
    a, b, c: Integer;
Begin
    WriteLn('Part 2: DESCRIPTION');
    WriteLn(Format('Part Two Solution: %d', [13]));
End;

Procedure PrintGuardEvent(g: GuardEvent);
Begin
	Case g.eType Of
		start:
			WriteLn('Guard #', g.guardID, ' started shift at ', DateTimeToStr(g.time));
		sleep:
			WriteLn('Guard #', g.guardID, ' fell asleep at ', DateTimeToStr(g.time));
		wake:
			WriteLn('Guard #', g.guardID, ' woke at at ', DateTimeToStr(g.time));
	End;
End;

Function ParseInput(input: TStringList): GuardEventArray;
Var
	dtRE, gRE: TRegExpr;
	i, idx, gID: Integer;
	key: String;
	gEvent: GuardEvent;
	s: String;
	map: AoCIntegerMap;
	tempEvents: GuardEventArray;
Begin
	result := [];
	SetLength(result, input.Count);
	SetLength(tempEvents, input.Count);
	
	map := AoCIntegerMap.Create;
	map.Sorted := True;
	
	dtRE := TRegExpr.Create('\[([^\]]+)\]');
	gRE := TRegExpr.Create('Guard #(\d+)');
	
	ShortDateFormat := 'y/m/d';
	For i := 0 To input.Count-1 Do
		If dtRE.Exec(input[i]) Then
		Begin
			s := dtRE.Match[1];
			gEvent.time := StrToDateTime(s);
			
			gEvent.guardID := -1;
			If gRE.Exec(input[i]) Then
			Begin
				gEvent.guardID := StrToInt(gRE.Match[1]);
				gEvent.eType := start;
			End
			Else
			Begin
				gEvent.eType := sleep;
				If ContainsStr(input[i], 'wake') Then
					gEvent.eType := wake;
			End;
			
			tempEvents[i] := gEvent;
			map[s] := i;
		End;
	
	// map keys are sorted by date
	For i := 0 To input.Count-1 Do
	Begin
		key := map.Keys[i];
		idx := map[key];
		result[i] := tempEvents[idx];
	End;
	map.Free;
	
	// Assign guard ids, correct start times to midnight
	gID := -1;
	For i := 0 To Length(result)-1 Do
	Begin
		If result[i].guardID = -1 Then
			result[i].guardID := gID
		Else
			gID := result[i].guardID;
			
		If result[i].eType = start Then
		Begin
			// The next event (sleep) will be on the right date
			result[i].time := result[i+1].time;
			// Zero out the hour & minute (midnight)
			result[i].time := RecodeHour(result[i].time, 0);
			result[i].time := RecodeMinute(result[i].time, 0);
		End;
		
		//PrintGuardEvent(result[i]);
	End;
End;

Var
    input: TStringList;
    events: GuardEventArray;
Begin
    input := ReadInput(IN_FILE);
    events := ParseInput(input);
    SolvePart1(events);
    SolvePart2(events);
End.
