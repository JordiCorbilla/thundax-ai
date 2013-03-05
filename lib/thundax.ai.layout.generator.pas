// Copyright (c) 2012-2013, Jordi Corbilla
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// - Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
// - Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// - Neither the name of this library nor the names of its contributors may be
// used to endorse or promote products derived from this software without
// specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

unit thundax.ai.layout.generator;

interface

uses
  ExtCtrls, types;

type
  TLayoutGenerator = class(TObject)
  private
    FImage: TImage;
    FNumPoints : Integer;
    procedure ClearCanvas;
    procedure DrawPoint(point: TPoint; colorPoint: integer);
  public
    constructor Create(image: TImage; numPoints : integer);
    procedure Generate();
  end;

implementation

uses
  Graphics, windows;

{ TLayoutGenerator }

procedure TLayoutGenerator.ClearCanvas;
begin
  FImage.Canvas.Brush.Style := bsSolid;
  FImage.Canvas.Brush.Color := RGB(Random(256), Random(256), Random(256));
  FImage.Canvas.Pen.Width := 1;
  FImage.Canvas.Pen.Color := FImage.Canvas.Brush.Color;
  FImage.Canvas.Rectangle(0, 0, FImage.Width, FImage.Height);
end;

constructor TLayoutGenerator.Create(image: TImage; numPoints : integer);
begin
  FImage := image;
  FNumPoints := numPoints;
end;

procedure TLayoutGenerator.Generate;
var
  i, int1, int2: integer;
  pixColour: TColor;
begin
  ClearCanvas;

  FImage.Canvas.CleanupInstance;
  FImage.Picture.Bitmap.SetSize(FImage.Width, FImage.Height);
  FImage.Canvas.Brush.Style := bsSolid;
  FImage.Canvas.Brush.Color := RGB(Random(256), Random(256), Random(256));
  FImage.Canvas.Pen.Width := 1;
  FImage.Canvas.Pen.Color := FImage.Canvas.Brush.Color;
  FImage.Canvas.Rectangle(0, 0, FImage.Width, FImage.Height);

  for i := 0 to FNumPoints - 1 do
  begin
    int1 := 1 + Random(FImage.Width);
    int2 := 1 + Random(FImage.Height);
    pixColour := FImage.Canvas.Brush.Color;
    while FImage.Canvas.Brush.Color = pixColour do
      pixColour := RGB(Random(256), Random(256), Random(256));
    DrawPoint(point(int1, int2), pixColour);
  end;
end;

procedure TLayoutGenerator.DrawPoint(point: TPoint; colorPoint: integer);
begin
  FImage.Canvas.Pen.Color := colorPoint;
  FImage.Canvas.Pen.Width := 1;
  FImage.Canvas.Pixels[point.x, point.Y] := colorPoint;
end;

end.
