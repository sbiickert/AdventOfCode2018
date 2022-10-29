Program AOC_2018_Day06;
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Uses SysUtils, StrUtils, AoCUtils, AoCSpatialUtils, Classes, RegExpr;

Const
    //IN_FILE = '../Input/06.test.txt';
    IN_FILE = '../Input/06.challenge.txt';
    LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';

Procedure SolvePart1(coords: Coord2DArray);
Var
    i, r, c: Integer;
    grid: Grid2D;
    ext: Extent2D;
    dist, closestDist: Integer;
    closest: array of Integer;
    coord: Coord2D;
    infinite: AoCStringMap;
    hist: AoCIntegerMap;
    greatestArea: Integer;
    key: String;
    nums: array of Integer;
Begin
    WriteLn('Part 1: Find the largest non-infinite area');
    
    grid := Grid2D.Create('.', rook);
    For i:= 0 To Length(coords)-1 Do
    	grid.SetValue(LETTERS[i+1], coords[i]);
    
    //grid.Print;
    ext := grid.GetExtent;
    //ext.Print;
    
    For r := ext.GetMin.Y To ext.GetMax.Y Do
    	For c := ext.GetMin.X To ext.GetMax.X Do
    	Begin
    		coord := Coord2D.Create(c, r);
    		closest := [0];
    		closestDist := coords[0].MDistanceTo(coord);
    		
    		For i := 1 To Length(coords)-1 Do
    		Begin
    			dist := coords[i].MDistanceTo(coord);
    			If dist <= closestDist Then
    				If dist = closestDist Then
    				Begin
						SetLength(closest, Length(closest)+1);
						closest[Length(closest)-1] := i;
    				End
    				Else
    				Begin
						closestDist := dist;
						closest := [i];
    				End;
    		End;
    		
    		If Length(closest) = 1 Then
    			grid.SetValue(LETTERS[closest[0]+1], coord);
    		
    		coord.Free;
    	End;
    
    //WriteLn();	
    //grid.Print;
    
    // Get the values that are on the edge of the grid
    infinite := AoCStringMap.Create;
    
    nums := [ext.GetMin.Y, ext.GetMax.Y];
    For r In nums Do
		For c := ext.GetMin.X To ext.GetMax.X Do
		Begin
			coord := Coord2D.Create(c,r);
			key := grid.GetValue(coord);
			i := infinite.IndexOf(key);
			If i = -1 Then
				infinite[key] := '';
			coord.Free;
		End;
    
    nums := [ext.GetMin.X, ext.GetMax.X];
    For c In nums Do
		For r := ext.GetMin.Y To ext.GetMax.Y Do
		Begin
			coord := Coord2D.Create(c,r);
			key := grid.GetValue(coord);
			i := infinite.IndexOf(key);
			If i = -1 Then
				infinite[key] := '';
			coord.Free;
		End;
    
    //PrintAoCStringMap(infinite);
    
    hist := grid.GetHistogram;
    
    //PrintAoCIntegerMap(hist);
    
    greatestArea := 0;
    For i := 0 To Length(coords)-1 Do
    Begin
    	key := LETTERS[i+1];
    	If (infinite.IndexOf(key) = -1) And (hist[key] > greatestArea) Then
    		greatestArea := hist[key];
    End;
    
    infinite.Free;
    
    WriteLn(Format('Part One Solution: %d', [greatestArea]));
End;

Procedure SolvePart2(values: TStringList);
Var
    a, b, c: Integer;
Begin
    WriteLn('Part 2: DESCRIPTION');
    WriteLn(Format('Part Two Solution: %d', [13]));
End;

Function ParseInput(input: TStringList): Coord2DArray;
Var
	i: Integer;
	re: TRegExpr;
	c: Coord2D;
Begin
	result := [];
	SetLength(result, input.Count);
	re := TRegExpr.Create('(\d+), (\d+)');
	
	For i := 0 To input.Count-1 Do
		If re.Exec(input[i]) Then
		Begin
			c := Coord2D.Create(StrToInt(re.Match[1]), StrToInt(re.Match[2]));
			result[i] := c;
		End;
End;

Var
    input: TStringList;
    coords: Coord2DArray;
Begin
    input := ReadInput(IN_FILE);
    coords := ParseInput(input);
    SolvePart1(coords);
    SolvePart2(input);
End.
