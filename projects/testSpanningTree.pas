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

unit testSpanningTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, thundax.ai.spanningTree, thundax.ai.coordenate;

type
  TfrmSpanning = class(TForm)
    imgDisplay: TImage;
    btnFindMinimum: TButton;
    mLog: TMemo;
    edtNumPoints: TEdit;
    Label1: TLabel;
    procedure btnFindMinimumClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFirstTime : Boolean;
    procedure MoveCoordenate(const p: TCoordenate);
    procedure DrawCoordenate(const p: TCoordenate);
  public
    procedure FillInVertices(spanningTree: TSpanningTree);
    procedure Log(s: string);
  end;

var
  frmSpanning: TfrmSpanning;

implementation

uses
  Diagnostics, thundax.ai.spanningTree.generator;

{$R *.dfm}

procedure TfrmSpanning.btnFindMinimumClick(Sender: TObject);
var
  spanningTree: TSpanningTree;
  st: TStopwatch;
  minimumDistance: double;
  layout: TLayoutGenerator;
  cumulative : Int64;
begin
  spanningTree := TSpanningTree.Create;
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
    FillInVertices(spanningTree);
    cumulative := st.ElapsedMilliseconds;
    Log('Finish Filling Vertices. Ellapsed time ' + IntToStr(st.ElapsedMilliseconds) + ' ms');

    st := TStopwatch.StartNew;
    Log('Start Calculating Adjacent List');
    spanningTree.CalculateAdjacentList;
    cumulative := cumulative + st.ElapsedMilliseconds;
    Log('Finish Calculating Adjacent List. Ellapsed time ' + IntToStr(st.ElapsedMilliseconds) + ' ms');

    st := TStopwatch.StartNew;
    Log('Start Calculating Minimum Spannig Tree');
    minimumDistance := spanningTree.Minimum(MoveCoordenate, DrawCoordenate);
    cumulative := cumulative + st.ElapsedMilliseconds;
    Log('Finish Calculating Minimum Spannig Tree. Ellapsed time ' + IntToStr(st.ElapsedMilliseconds) + ' ms');
    Log('Minimum Distance: ' + FloatToStr(minimumDistance) + ' in ' + IntToStr(cumulative) + ' ms');
  finally
    FreeAndNil(spanningTree);
  end;
end;

procedure TfrmSpanning.DrawCoordenate(const p: TCoordenate);
var
  randomColour: TColor;
  value: integer;
begin
  value := Random(4);
  randomColour := clRed;
  case value of
    0:
      randomColour := clRed;
    1:
      randomColour := clBlue;
    2:
      randomColour := clLime;
    3:
      randomColour := clWhite;
  end;
  imgDisplay.Canvas.Pen.Color := randomColour;
  imgDisplay.Canvas.LineTo(p.x, p.y);
end;

procedure TfrmSpanning.FillInVertices(spanningTree: TSpanningTree);
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
        spanningTree.AddVertice(i, j);
    end;
  end;

  imgDisplay.Canvas.Brush.Color := clFuchsia;
  imgDisplay.Canvas.Brush.Style := bsSolid;
  imgDisplay.Canvas.Pen.Color := clFuchsia;

  for vertices in spanningTree.vertices do
  begin
    NewRect := Rect(vertices.x - 2, vertices.y - 2, vertices.x + 2, vertices.y + 2);
    imgDisplay.Canvas.FillRect(NewRect);
  end;
end;

procedure TfrmSpanning.FormCreate(Sender: TObject);
begin
  FFirstTime := true;
end;

procedure TfrmSpanning.Log(s: string);
begin
  mLog.Lines.Add(DateTimeToStr(Now) + ' ' + s);
end;

procedure TfrmSpanning.MoveCoordenate(const p: TCoordenate);
begin
  imgDisplay.Canvas.MoveTo(p.x, p.y);
end;

end.
