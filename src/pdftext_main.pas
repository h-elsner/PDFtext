(* Unter LINUX eine Textebene zu PDF-Dateien hinzufügen

Benötigte Programme installieren:
---------------------------------
* ImageMagick (sollte bei Ubuntu bereits installiert sein)
* pdfsandwich                 http://www.tobias-elze.de/pdfsandwich/
* tesseract-ocr               https://github.com/tesseract-ocr/tesseract/
* tesseract language codes    https://tesseract-ocr.github.io/tessdoc/Data-Files-in-different-versions.html
*)

unit pdftext_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, Process, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnAddTxt: TBitBtn;
    btnInfo: TBitBtn;
    btnClose: TBitBtn;
    ImageList1: TImageList;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    procedure btnAddTxtClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure Memo1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Memo1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private

  public
    procedure PDFAddText(fn: string);                {Process one PDF file}
  end;

{.$I pdftext_de.inc}
{$I pdftext_en.inc}

var
  Form1: TForm1;

const
  infofile='pdftext.txt';
  app='pdfsandwich';
  paralang='-lang';
  pdfext='.pdf';
  separ='===================================';       {Visual separator for multiple files processed}

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);        {GUI Initialization}
begin
  Memo1.Lines.Clear;
  Caption:=capForm;
  btnAddTxt.Caption:=capAddTxt;
  btnAddTxt.Hint:=hntAddTxt;
  btnInfo.Caption:=capInfo;
  btnInfo.Hint:=hntInfo;
  btnClose.Caption:=capClose;
  btnClose.Hint:=hntClose;
  Memo1.Hint:=hntMemo;
end;

procedure TForm1.PDFAddText(fn: string);             {Process one PDF file, TProcess}
var
  pdftxt: TProcess;
  outlist: TStringList;
  i: integer;

begin
  Memo1.Lines.Add(fn);
  Memo1.Lines.Add('');
  outlist:=TStringlist.Create;
  pdftxt:=TProcess.Create(nil);
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  try
    pdftxt.Executable:=app;
    pdftxt.Parameters.Add(paralang);
    pdftxt.Parameters.Add(Sprache);
    pdftxt.Parameters.Add(fn);
    pdftxt.Options:=pdftxt.Options+[poWaitOnExit, poUsePipes];
    pdftxt.Execute;
    outlist.LoadFromStream(pdftxt.Output);
    for i:=0 to outlist.Count-1 do
      Memo1.Lines.Add(outlist[i]);
  finally
    outlist.Free;
    pdftxt.Free;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.btnAddTxtClick(Sender: TObject);    {Button Create text layer to process
                                                      files selected with open dialog}
var
  i: Integer;

begin
  OpenDialog1.Title:=capOpenPDF;                     {Option ofAllowMultiSelect must be set}
  if OpenDialog1.Execute then begin
    btnClose.Enabled:=false;
    try
      Memo1.Lines.Clear;                             {Empty protocol output}
      for i:=0 to OpenDialog1.Files.Count-1 do begin
        PDFAddText(Opendialog1.Files[i]);
        Memo1.Lines.Add(separ);
      end;
      Memo1.Lines.Add('');
    finally
      btnClose.Enabled:=true;
    end;
  end;
end;

                                                     {Drag & Drop files to process}
procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  i: integer;
  pdflist: TStringList;

begin
  pdflist:=TStringList.Create;
  try
    for i:=0 to high(FileNames) do begin
      if Lowercase(ExtractFileExt(FileNames[i]))=pdfext then begin {Select only PDF files}
        pdflist.Add(FileNames[i]);
      end;
    end;
    Memo1.Lines.Clear;
    if pdflist.Count>0 then begin
      btnClose.Enabled:=false;
      for i:=0 to pdflist.Count-1 do begin
        PDFAddText(pdflist[i]);
        Memo1.Lines.Add(separ);
      end;
    end;
    Memo1.Lines.Add('');
  finally
    btnClose.Enabled:=true;
    pdflist.Free;
    Screen.Cursor:=crDefault;
  end;
end;

                                                     {Change font size of text with ctrl+mousewheel}
procedure TForm1.Memo1MouseWheelDown(Sender: TObject; Shift: TShiftState;
                                     MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    Memo1.Font.Size:=Memo1.Font.Size-1;
end;

procedure TForm1.Memo1MouseWheelUp(Sender: TObject; Shift: TShiftState;
                                   MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    Memo1.Font.Size:=Memo1.Font.Size+1;
end;

procedure TForm1.btnInfoClick(Sender: TObject);      {Button Info}
begin
  if FileExists(Application.Location+infofile) then begin
    Memo1.Lines.LoadFromFile(Application.Location+infofile);
  end else begin
    Memo1.Lines.Add(infofile+errInfo);
    Memo1.Lines.Add(hntInfofile);
  end;
end;

procedure TForm1.btnCloseClick(Sender: TObject);     {Button Close}
begin
  Close;
end;

end.

