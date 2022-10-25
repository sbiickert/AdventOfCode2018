Program AOC_2018_Day03;
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Uses SysUtils, StrUtils, AoCUtils, AoCSpatialUtils, Classes, RegExpr;

Const
    //IN_FILE = '../Input/03.test.txt';
    IN_FILE = '../Input/03.challenge.txt';
    NO_CLAIM = '.';
    MULTI = 'X';

Type
	Claim = Record
		id: Integer;
		ext: Extent2D;
	End;
	ClaimArray = Array of Claim;

Function SolvePart1(claims: ClaimArray): Grid2D;
Var
    i, j: Integer;
    coords: Coord2DArray;
    c: Coord2D;
    idStr: String;
Begin
    WriteLn('Part 1: Find the area claimed by two or more');
    result := Grid2D.Create(NO_CLAIM);
    coords := [];
    
    For i := 0 To Length(claims)-1 Do
    Begin
    	If (i mod 25 = 0) Then
	    	WriteLn(i);
	    	
    	coords := claims[i].ext.AllContainedCoords;
    	For j := 0 To Length(coords)-1 Do
    	Begin
    		c := coords[j];
    		idStr := IntToStr(claims[i].id);
    		If result.GetValue(c) = NO_CLAIM Then
    			result.SetValue(idStr, c)
    		Else
    			result.SetValue(MULTI, c);
    	End;
    	//result.Print;
    End;
    
    coords := result.GetCoords(MULTI);
    
    WriteLn(Format('Part One Solution: %d', [Length(coords)]));
End;

Procedure SolvePart2(claims: ClaimArray; cloth: Grid2D);
Var
    i, area, count: Integer;
    hist: AoCIntegerMap;
    id: String;
Begin
    WriteLn('Part 2: Which claim does not overlap?');
    
    hist := cloth.GetHistogram;
    //PrintAoCIntegerMap(hist);
    
    For i := 0 To Length(claims)-1 Do
    Begin
    	id := IntToStr(claims[i].id);
    	If (i mod 25 = 0) Then
	    	WriteLn(id);
	    
	    area := claims[i].ext.GetArea;
	    If hist.IndexOf(id) = -1 Then
	    	count := 0
	    Else
	    	count := hist[id];
	    
	    If (area = count) Then
	    	Break;
    End;
    
    WriteLn(Format('Part Two Solution: %s', [id]));
End;

Function ParseInput(values: TStringList): ClaimArray;
Var
	i: Integer;
	re: TRegExpr;
	id, x, y: Integer;
	c1, c2: Coord2D;
	ext: Extent2D;
Begin
	result := [];
	SetLength(result, values.Count);
	
	// Example line: #1 @ 1,3: 4x4
	re := TRegExpr.Create('#(\d+) @ (\d+),(\d+): (\d+)x(\d+)');
	
	For i := 0 To values.Count-1 Do
		If re.Exec(values[i]) Then
		Begin
			id := StrToInt(re.Match[1]);
			x := StrToInt(re.Match[2]);
			y := StrToInt(re.Match[3]);
			c1 := Coord2D.Create(x,y);
			x := x + StrToInt(re.Match[4]) -1;
			y := y + StrToInt(re.Match[5]) -1;
			c2 := Coord2D.Create(x,y);
			ext := Extent2D.Create([c1,c2]);
			result[i].id := id;
			result[i].ext := ext;
		End;
		
	re.Free;
End;

Var
    input: TStringList;
    claims: ClaimArray;
    cloth: Grid2D;
Begin
    input := ReadInput(IN_FILE);
    claims := ParseInput(input);
    cloth := SolvePart1(claims);
    SolvePart2(claims, cloth);
End.
