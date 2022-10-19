Program AOC_2018_Day01;
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

// TFPHashList uses ContNrs
Uses SysUtils, StrUtils, AoCUtils, Classes, ContNrs;

Const
    IN_FILE = '../input/01.challenge.txt';

Procedure SolvePart1(values: AoCIntArray);
Var
    sum: Integer;
Begin
    WriteLn('Part 1: Resulting frequency after all changes');
    sum := SumIntArray(values);
    WriteLn(Format('Part One Solution: %d', [sum]));
End;

Procedure SolvePart2(values: AoCIntArray);
Var
    i, freq: Integer;
    fStr: String;
    map: AoCStringMap;
    bFound: Boolean;
Begin
    WriteLn('Part 2: Find the first frequency found twice');
    
    map := AoCStringMap.Create;
    freq := 0;
    map[IntToStr(freq)] := ''; // Using the map keys as a set
    bFound := False;
    
    While (bFound = False) Do
    Begin
    	For i := 0 To Length(values)-1 Do
    	Begin
    		freq := freq + values[i];
    		fStr := IntToStr(freq);
    		If map.IndexOf(fStr) <> -1 Then
    		Begin
    			WriteLn('found ', fStr);
    			bFound := True;
    			Break;
    		End;
    		map[fStr] := '';
    	End;
    	WriteLn(IntToStr(map.Count));
    End;
    
    map.Free;
    
    WriteLn(Format('Part Two Solution: %d', [freq]));
End;

Procedure SolvePart2Faster(values: AoCIntArray);
Var
    i, freq: Integer;
    ptr: Pointer;
    fStr: String;
    map: TFPHashList;
    bFound: Boolean;
Begin
    WriteLn('Part 2: Find the first frequency found twice');
    
    map := TFPHashList.Create;
    freq := 0;
    ptr := @freq;
    fStr := IntToStr(freq);
    map.Add(fStr, ptr); // Using the map keys as a set
    bFound := False;
    
    While (bFound = False) Do
    Begin
    	For i := 0 To Length(values)-1 Do
    	Begin
    		freq := freq + values[i];
    		fStr := IntToStr(freq);
    		If map.FindIndexOf(fStr) <> -1 Then
    		Begin
    			bFound := True;
    			Break;
    		End;
    		ptr := @freq;
    		map.Add(fStr, ptr);
    	End;
    End;
    
    map.Free;
    
    WriteLn(Format('Part Two Solution: %d', [freq]));
End;

Var
    input: TStringList;
    iArr: AoCIntArray;
Begin
    input := ReadInput(IN_FILE);
    iArr := StrListToIntArray(input);
    SolvePart1(iArr);
    //SolvePart2(iArr);
    SolvePart2Faster(iArr);
End.
