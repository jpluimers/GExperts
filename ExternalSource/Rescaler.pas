unit Rescaler;

interface

uses
  Graphics;

function Rescale(Source, Destination: TBitmap; NearestFit: Boolean): Boolean;
// Will rescale the Source image and put the rescaled version into the Destination
// bitmap. If NearestFit is set to True, this is the same kind of rescaling that TImage.Stretch
// will perform. If it's set to False, the procedure will use a resampling algorithm called
// "Fant's Resampling Algorithm". This algorithm makes sure that instead of just picking single
// pixels and either skipping some or duplicating other to make the image smaller or larger
// all pixels contribute to the image and thus makes a much better end result, albeit a little
// slower than the nearest fit algorithm (stretchdraw).

function Resize(Bitmap: TBitmap; NewWidth, NewHeight: Integer; NearestFit: Boolean): Boolean;
// Will effectively call Rescale above with Source=Destination (not quite, but that's effect),
// and resize the Bitmap variable in-place. That means that if this function returns True, the
// Bitmap object will have the new width and height and its contents will have been rescaled to
// match (not cropped)

function Zoom(Bitmap: TBitmap; Factor: Integer; NearestFit: Boolean): Boolean;
// Will just call Resize with the width and height of Bitmap multiplied with the factor

implementation

uses
  Classes;

procedure ResampleLine(SourcePtr: PAnsiChar; SourceWidth: Integer; DestPtr: PAnsiChar; DestWidth, ChannelCount: Integer);
var
  Scale: Extended;
  Insfac: Extended;
  Outseg: Extended;
  Inseg: Extended;
  InvInseg: Extended;
  Accumulator: array[0..3] of Extended;
  Intensity: array[0..3] of Extended;
  ix, ox: Integer;
  OldIX: Integer;
  Channel: Integer;
  P: PAnsiChar;
begin
  Scale := DestWidth / SourceWidth;
  Insfac := 1 / Scale;
  Outseg := Insfac;
  Inseg := 1.0;
  for Channel := 0 to 3 do
    Accumulator[Channel] := 0.0;
  ix := 0;
  ox := 0;
  OldIX := -1;

  while (ox < DestWidth) do
  begin
    if (OldIX <> ix) then
    begin
      P := SourcePtr + ix * ChannelCount;
      InvInseg := 1.0 - Inseg;
      for Channel := 0 to ChannelCount - 1 do
        Intensity[Channel] := (Inseg * Ord(P[Channel])) + (InvInseg * (Ord(P[Channel + ChannelCount])));

      OldIX := ix;
    end; // if

    if (Inseg < Outseg) then
    begin
      for Channel := 0 to ChannelCount - 1 do
        Accumulator[Channel] := Accumulator[Channel] + (Intensity[Channel] * Inseg);
      Outseg := Outseg - Inseg;
      Inseg := 1.0;
      Inc(ix);
    end // if
    else begin
      P := Destptr + ox * ChannelCount;
      for Channel := 0 to ChannelCount - 1 do
      begin
        Accumulator[Channel] := Accumulator[Channel] + (Intensity[Channel] * Outseg);
        P[Channel] := AnsiChar(Trunc(Accumulator[Channel] * Scale));
        Accumulator[Channel] := 0.0;
      end; // for Channel
      Inseg := Inseg - Outseg;
      Outseg := Insfac;
      Inc(ox);
    end; // else
  end; // while
end; // procedure RescaleLine

function Resample(Source, Destination: TBitmap): Boolean;

  function ResampleWidth(Source, Destination: TBitmap): Boolean;
  var
    y: Integer;
    ChannelCount: Integer;
    Line: PAnsiChar;
  begin
    ChannelCount := 3 + Ord(Source.PixelFormat = pf32bit);
    GetMem(Line, (Source.Width + 1) * ChannelCount);

    for y := 0 to Source.Height - 1 do
    begin
      Move(Source.ScanLine[y]^, Line^, Source.Width * ChannelCount);
      // Duplicate last pixel
      Move((Line + (Source.Width - 1) * ChannelCount)^, (Line + Source.Width * ChannelCount)^,
        ChannelCount);
      ResampleLine(
        Line, Source.Width,
        Destination.ScanLine[y], Destination.Width,
        ChannelCount);
    end; // for y

    FreeMem(Line, (Source.Width + 1) * ChannelCount);
    Result := True;
  end;

  function ResampleHeight(Source, Destination: TBitmap): Boolean;
  var
    SourceLine: PAnsiChar;
    DestinationLine: PAnsiChar;
    P: PAnsiChar;
    x, y, yy: Integer;
    ChannelCount: Integer;
  begin
    ChannelCount := 3 + Ord(Source.PixelFormat = pf32bit);
    GetMem(SourceLine, (Source.Height + 1) * ChannelCount);
    GetMem(DestinationLine, Destination.Height * ChannelCount);

    for x := 0 to Source.Width - 1 do
    begin
      // Transform the source row to a line
      P := SourceLine;
      yy := 0;
      for y := 0 to Source.Height do
      begin
        // Duplicate last line
        if (y < Source.Height) then
          yy := y;
        Move((PAnsiChar(Source.ScanLine[yy]) + x * ChannelCount)^, P^, ChannelCount);
        Inc(P, ChannelCount);
      end; // for y

      ResampleLine(SourceLine, Source.Height, DestinationLine, Destination.Height, ChannelCount);

      // Transform the line to a destination row
      P := DestinationLine;
      for y := 0 to Destination.Height - 1 do
      begin
        Move(P^, (PAnsiChar(Destination.ScanLine[y]) + x * ChannelCount)^, ChannelCount);
        Inc(P, ChannelCount);
      end; // for y
    end; // for x

    FreeMem(DestinationLine, Destination.Width * ChannelCount);
    FreeMem(SourceLine, (Source.Width + 1) * ChannelCount);
    Result := True;
  end;

  function ResampleSize(Source, Destination: TBitmap): Boolean;
  var
    Temp: TBitmap;
  begin
    Temp := TBitmap.Create;
    Temp.Width := Destination.Width;
    Temp.Height := Source.Height;
    Temp.PixelFormat := Source.PixelFormat;

    if not ResampleWidth(Source, Temp) then
    begin
      Temp.Free;
      Result := False;
      Exit;
    end; // if not ok

    if not ResampleHeight(Temp, Destination) then
    begin
      Temp.Free;
      Result := False;
      Exit;
    end; // if not ok

    Temp.Free;
    Result := True;
  end;

begin
  if (Source.Width = Destination.Width) and (Source.Height = Destination.Height) then
  begin
    Destination.Assign(Source);
    Result := True;
    Exit;
  end; // if same size

  if not (Source.PixelFormat in [pf24bit, pf32bit]) then
    Source.PixelFormat := pf32bit;

  Destination.PixelFormat := Source.PixelFormat;

  if (Source.Width = Destination.Width) then
    Result := ResampleHeight(Source, Destination)
  else if (Source.Height = Destination.Height) then
    Result := ResampleWidth(Source, Destination)
  else
    Result := ResampleSize(Source, Destination);
end; // function Resample

function DoRescale(Source, Destination: TBitmap): Boolean;
begin
  Destination.Canvas.StretchDraw(Rect(0, 0, Destination.Width, Destination.Height), Source);
  Result := True;
end; // function DoRescale

function Rescale(Source, Destination: TBitmap; NearestFit: Boolean): Boolean;
begin
  if NearestFit then
    Result := DoRescale(Source, Destination)
  else
    Result := Resample(Source, Destination);
end; // function Rescale

function Resize(Bitmap: TBitmap; NewWidth, NewHeight: Integer; NearestFit: Boolean): Boolean;
var
  TempBitmap: TBitmap;
begin
   // Create a temporary bitmap of the needed size
  TempBitmap := TBitmap.Create;
  TempBitmap.Width := NewWidth;
  TempBitmap.Height := NewHeight;
  TempBitmap.PixelFormat := Bitmap.PixelFormat;

   // Now do the rescaling
  Result := Rescale(Bitmap, TempBitmap, NearestFit);

   // Assign the new bitmap if all went ok
  if Result then
    Bitmap.Assign(TempBitmap);

  // And free up the temporary bitmap object
  TempBitmap.Free;
end; // function resize

function Zoom(Bitmap: TBitmap; Factor: Integer; NearestFit: Boolean): Boolean;
begin
  Result := Resize(Bitmap, Bitmap.Width * Factor, Bitmap.Height * Factor, NearestFit);
end; // function Zoom

end. // unit Rescaler

