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

unit testConvexHull;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, thundax.ai.convexHull, thundax.ai.coordenate;

type
  TfrmConvexHull = class(TForm)
    imgDisplay: TImage;
    btnFindConvexHull: TButton;
    edtNumPoints: TEdit;
    Label1: TLabel;
    mLog: TMemo;
    procedure btnFindConvexHullClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFirstTime: Boolean;
    procedure Log(s: string);
    procedure DrawLine(p1, p2: TCoordenate; colorLine: integer);
  public
    procedure FillInVertices(convexHull: TConvexHull);
  end;

var
  frmConvexHull: TfrmConvexHull;

implementation

uses
  Diagnostics, thundax.ai.layout.generator, Contnrs, Generics.Collections;

{$R *.dfm}

procedure TfrmConvexHull.btnFindConvexHullClick(Sender: TObject);
var
  convexHull: TConvexHull;
  st: TStopwatch;
  layout: TLayoutGenerator;
  cumulative: Int64;
  closurePoints: TObjectList<TCoordenate>;
  i: integer;
begin
  convexHull := TConvexHull.Create;
  closurePoints := nil;
  try
    if not FFirstTime then
    begin
      layout := TLayoutGenerator.Create(imgDisplay, StrToInt(edtNumPoints.text));
      try
        layout.Generate;
      finally
        FreeAndNil(layout);
      end;
    end;
    FFirstTime := false;

    st := TStopwatch.StartNew;
    Log('Start Filling Vertices');
    FillInVertices(convexHull);
    cumulative := st.ElapsedMilliseconds;
    Log('Finish Filling Vertices. Ellapsed time ' + IntToStr(st.ElapsedMilliseconds) + ' ms');

    st := TStopwatch.StartNew;
    Log('Start Calculating Convex Hull');
    closurePoints := convexHull.Chain();
    cumulative := cumulative + st.ElapsedMilliseconds;
    Log('Finish Calculating Convex Hull. Ellapsed time ' + IntToStr(st.ElapsedMilliseconds) + ' ms');

    st := TStopwatch.StartNew;
    Log('Start Displaying Closure');
    for i := 0 to closurePoints.Count - 2 do
      DrawLine(closurePoints.Items[i], closurePoints.Items[i + 1], clred);
    cumulative := cumulative + st.ElapsedMilliseconds;
    Log('Finish Displaying Closure. Ellapsed time ' + IntToStr(st.ElapsedMilliseconds) + ' ms');
    Log('Total time in ' + IntToStr(cumulative) + ' ms');

  finally
    closurePoints.Free;
    convexHull.Free;
  end;
end;

procedure TfrmConvexHull.DrawLine(p1: TCoordenate; p2: TCoordenate; colorLine: integer);
begin
  imgDisplay.Canvas.Brush.Style := bsSolid;
  imgDisplay.Canvas.Brush.Color := colorLine;
  imgDisplay.Canvas.Pen.Width := 1;
  imgDisplay.Canvas.Pen.Color := colorLine;
  imgDisplay.Canvas.MoveTo(p1.x, p1.y);
  imgDisplay.Canvas.LineTo(p2.x, p2.y);
end;

procedure TfrmConvexHull.FillInVertices(convexHull: TConvexHull);
var
  i: integer;
  j: integer;
  TransparentColor, color1: integer;
  NewRect: TRect;
  vertices: TCoordenate;
begin
  color1 := imgDisplay.Picture.Bitmap.Canvas.Pixels[0, 0];
  for i := 0 to imgDisplay.Picture.Bitmap.Width - 1 do
  begin
    for j := 0 to imgDisplay.Picture.Bitmap.Height - 1 do
    begin
      TransparentColor := imgDisplay.Picture.Bitmap.Canvas.Pixels[i, j];
      if color1 <> TransparentColor then
        convexHull.AddVertice(i, j);
    end;
  end;

  imgDisplay.Canvas.Brush.Color := clFuchsia;
  imgDisplay.Canvas.Brush.Style := bsSolid;
  imgDisplay.Canvas.Pen.Color := clFuchsia;

  for vertices in convexHull.vertices do
  begin
    NewRect := Rect(vertices.x - 2, vertices.y - 2, vertices.x + 2, vertices.y + 2);
    imgDisplay.Canvas.FillRect(NewRect);
  end;
end;

procedure TfrmConvexHull.FormCreate(Sender: TObject);
begin
  FFirstTime := true;
end;

procedure TfrmConvexHull.Log(s: string);
begin
  mLog.Lines.Add(DateTimeToStr(Now) + ' ' + s);
end;

end.
