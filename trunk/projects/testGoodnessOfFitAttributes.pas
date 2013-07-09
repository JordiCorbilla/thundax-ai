// Copyright (c) 2013, Jordi Corbilla
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

unit testGoodnessOfFitAttributes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    edtPath: TEdit;
    btnOpenFile: TButton;
    btnCalcGoodness: TButton;
    chkverbosity: TCheckBox;
    sgAttributes: TStringGrid;
    Splitter1: TSplitter;
    log: TMemo;
    OpenDialog1: TOpenDialog;
    ComboBox1: TComboBox;
    procedure btnOpenFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox1Enter(Sender: TObject);
    procedure ComboBox1Exit(Sender: TObject);
    procedure sgAttributesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure sgAttributesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure btnCalcGoodnessClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  Math, thundax.ai.attribute.ordinal, thundax.ai.attribute.numerical, thundax.ai.attribute.list,
  thundax.ai.attribute.dictionary, thundax.ai.attribute.average.list.sorted, thundax.ai.attribute.average.list,
  Generics.Defaults;

{$R *.dfm}

//This method will do all the necessary calculations to find the goodness of the attributes.
//It will load the data from the grid and pupulate the necessary classes.
//Once done, it will report the values into the log section.
//First it will work on the ordinal values and the on the numerical ones.
procedure TForm2.btnCalcGoodnessClick(Sender: TObject);
var
  i, j: integer;
  attrList: TAttributeList;
  attribute: TOrdinalAttribute;
  numericalAttribute: TNumericalAttribute;
  IndividualItems: TDictionaryAttributes;
  treatment: string;
  ClassIndex: integer;
  numericalList: TDistinctSortedAttributeList;
  averageList: TAverageAttributeList;
begin
  log.Lines.Clear;
  // Get the class column
  ClassIndex := 0;
  for i := 1 to sgAttributes.ColCount - 1 do
  begin
    treatment := sgAttributes.Cells[i, 1];
    if treatment = 'Class' then
    begin
      ClassIndex := i;
      Break;
    end;
  end;

  //If the column "class" is not selected an error will be thrown
  if ClassIndex = 0 then
    raise Exception.Create('You need to select a column as a Class');

  // Tractar cada columna amb les dades obtingudes
  for i := 1 to sgAttributes.ColCount - 1 do
  begin
    treatment := sgAttributes.Cells[i, 1];
    if treatment = 'Ordinal' then
    begin
      attrList := TAttributeList.Create(sgAttributes.Cells[i, 2]);
      try
        for j := 3 to sgAttributes.RowCount - 1 do
        begin
          if sgAttributes.Cells[i, j] <> '' then
          begin
            attribute := TOrdinalAttribute.Create(sgAttributes.Cells[i, j], sgAttributes.Cells[ClassIndex, j]);
            attrList.Add(attribute);
          end;
          attrList.TotalNumberOfItems := attrList.TotalNumberOfItems + 1;
        end;
        IndividualItems := attrList.GetListOfIndividualItems;
        try
          log.Lines.Add(sgAttributes.Cells[i, 2]);
          if chkverbosity.Checked then
            log.Lines.Add(IndividualItems.ToString)
          else
            log.Lines.Add(IndividualItems.SimpleOutput);
          log.Lines.Add('');
        finally
          IndividualItems.Free;
        end;
      finally
        attrList.Free;
      end;
    end;
    if treatment = 'Numerical' then
    begin
      numericalList := TDistinctSortedAttributeList.Create(sgAttributes.Cells[i, 2]);
      try
        for j := 3 to sgAttributes.RowCount - 1 do
        begin
          if sgAttributes.Cells[i, j] <> '' then
          begin
            numericalAttribute := TNumericalAttribute.Create(StrToFloat(sgAttributes.Cells[i, j]), sgAttributes.Cells[ClassIndex, j]);
            numericalList.Add(numericalAttribute);
            numericalList.AddDefault(numericalAttribute);
          end;
        end;
        numericalList.Sort(TComparer<TNumericalAttribute>.Construct( function(const L, r: TNumericalAttribute): integer begin result := CompareValue(L.Value, r.Value, 0.000001); end));
        averageList := numericalList.GetAverageList;
        try
          log.Lines.Add(sgAttributes.Cells[i, 2]);
          if chkverbosity.Checked then
            log.Lines.Add(averageList.ToString)
          else
            log.Lines.Add(averageList.SimpleOutput);
          log.Lines.Add('');
        finally
          averageList.Free;
        end;
      finally
        numericalList.Free;
      end;
    end;
  end;
end;

//This method opens the file dialog, loads a CSV file in text format and it populates the string grid.
//Each row must be separated in a new line.
//The character delimiter must be a ','.
//The file must contain the description of the columns.
procedure TForm2.btnOpenFileClick(Sender: TObject);
  procedure ExtractValues(const aStrings: TStrings; const aValue: string; const aDelimiter: string);
  var
    position: integer;
    individual: string;
    gap: integer;
    remaining: string;
  begin
    gap := Length(aDelimiter);
    remaining := aValue + aDelimiter;
    aStrings.BeginUpdate;
    aStrings.Clear;
    try
      while Length(remaining) > 0 do
      begin
        position := pos(aDelimiter, remaining);
        individual := Copy(remaining, 0, position - 1);
        aStrings.Add(individual);
        remaining := Copy(remaining, position + gap, MaxInt);
      end;
    finally
      aStrings.EndUpdate;
    end;
  end;

var
  f: TextFile;
  myLine: string;
  list: TStringList;
  i, j: integer;
begin
  sgAttributes.RowCount := 5;
  sgAttributes.ColCount := 5;
  OpenDialog1.InitialDir := GetCurrentDir;
  if not OpenDialog1.Execute then
    ShowMessage('Open file was cancelled')
  else
  begin
    edtPath.Text := OpenDialog1.Files[0];
  end;

  if edtPath.Text <> '' then
  begin
    AssignFile(f, edtPath.Text);
    Reset(f);
    i := 1;
    while not Eof(f) do
    begin
      ReadLn(f, myLine);
      Inc(i);
      list := TStringList.Create;
      try
        ExtractValues(list, myLine, ',');
        sgAttributes.ColCount := list.Count + 1;
        sgAttributes.RowCount := i + 1;
        for j := 0 to list.Count - 1 do
          sgAttributes.Cells[j + 1, i - 0] := list[j];
      finally
        list.Free;
      end;
    end;
    CloseFile(f);

    for j := 1 to sgAttributes.ColCount - 1 do
      sgAttributes.Cells[j, 1] := 'Ordinal';

    sgAttributes.Cells[0, 1] := 'Treatment';
    sgAttributes.Cells[0, 2] := 'Description';
    btnCalcGoodness.Enabled := true;
  end;
end;

procedure TForm2.ComboBox1Change(Sender: TObject);
begin
  sgAttributes.Cells[sgAttributes.Col, sgAttributes.Row] := ComboBox1.Items[ComboBox1.ItemIndex];
  ComboBox1.Visible := False;
  sgAttributes.SetFocus;
end;

procedure TForm2.ComboBox1Enter(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to ComboBox1.Items.Count - 1 do
  begin
    if ComboBox1.Items[i] = sgAttributes.Cells[sgAttributes.Col, sgAttributes.Row] then
      ComboBox1.ItemIndex := i;
  end;
end;

procedure TForm2.ComboBox1Exit(Sender: TObject);
begin
  if ComboBox1.ItemIndex <> -1 then
    sgAttributes.Cells[sgAttributes.Col, sgAttributes.Row] := ComboBox1.Items[ComboBox1.ItemIndex]
  else
  begin
    if ComboBox1.Text = '' then
      sgAttributes.Cells[sgAttributes.Col, sgAttributes.Row] := ComboBox1.Text
    else
      sgAttributes.Cells[sgAttributes.Col, sgAttributes.Row] := 'Ordinal';
  end;
  ComboBox1.Visible := False;
  sgAttributes.SetFocus;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  sgAttributes.DefaultRowHeight := ComboBox1.Height;
  ComboBox1.Visible := False;
  ComboBox1.Items.Add('Ordinal');
  ComboBox1.Items.Add('Numerical');
  ComboBox1.Items.Add('No action');
  ComboBox1.Items.Add('Class');
  ComboBox1.Text := 'Ordinal';
  ComboBox1.ItemIndex := 0;
end;

procedure TForm2.sgAttributesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if (ARow in [1, 2]) and (ACol > 0) then
  begin
    if ARow = 1 then
      sgAttributes.Canvas.Brush.Color := $006167DC //$00DF766A //; //$000080FF
    else
      sgAttributes.Canvas.Brush.Color := clGray;
    sgAttributes.Canvas.FillRect(Rect);
    sgAttributes.Canvas.TextRect(Rect, Rect.Left, Rect.top, ' ' + sgAttributes.Cells[ACol, ARow]);
  end;
end;

procedure TForm2.sgAttributesSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  r: TRect;
  Value: string;
begin
  if (ACol <> 0) and (ARow = 1) then
  begin
    r := sgAttributes.CellRect(ACol, ARow);
    Value := sgAttributes.Cells[ACol, ARow];
    r.Left := r.Left + sgAttributes.Left;
    r.Right := r.Right + sgAttributes.Left;
    r.top := r.top + sgAttributes.top;
    r.Bottom := r.Bottom + sgAttributes.top;
    ComboBox1.Left := r.Left + 1;
    ComboBox1.top := r.top + 1;
    ComboBox1.Width := (r.Right + 1) - r.Left;
    ComboBox1.Height := (r.Bottom + 1) - r.top;
    ComboBox1.Text := Value;
    ComboBox1.Visible := true;
    ComboBox1.SetFocus;
  end;
  CanSelect := true;
end;

end.
