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

{$I pdftext_de.inc}
{ $I pdftext_en.inc}

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

procedure TForm1.PDFAddText(fn: string);             {Process one PDF file}
var
  pdftxt: TProcess;
  outlist: TStringList;
  i: integer;

begin
  if Lowercase(ExtractFileExt(fn))=pdfext then begin
    Memo1.Lines.Add(fn);
    Memo1.Lines.Add('');
    outlist:=TStringList.Create;
    pdftxt:=TProcess.Create(nil);
    Application.ProcessMessages;
    try
      pdftxt.Executable:=app;
      pdftxt.Parameters.Add(paralang);
      pdftxt.Parameters.Add(Sprache);
      pdftxt.Parameters.Add(fn);
      pdftxt.Options:=pdftxt.Options+[poWaitOnExit, poUsePipes];
      pdftxt.Execute;
      outlist.LoadFromStream(pdftxt.Output);         {Get commandline output}
      for i:=0 to outlist.Count-1 do                 {Append to log}
        Memo1.Lines.Add(outlist[i]);
      Memo1.Lines.Add(separ);
      Memo1.Lines.Add('');
    finally
      pdftxt.Free;
      outlist.Free;
    end;
  end else begin
    Memo1.Lines.Add(ExtractFileName(fn)+errPDF);
  end;
end;

procedure TForm1.btnAddTxtClick(Sender: TObject);    {Button Create text layer to process
                                                      files selected with open dialog}
var
  i: Integer;

begin
  OpenDialog1.Title:=capOpenPDF;                     {Option ofAllowMultiSelect must be set}
  if OpenDialog1.Execute then begin
    Screen.Cursor:=crHourGlass;
    btnClose.Enabled:=false;
    Memo1.Lines.Clear;                               {Empty log output}
    try
      for i:=0 to OpenDialog1.Files.Count-1 do
        PDFAddText(Opendialog1.Files[i]);
    finally
      btnClose.Enabled:=true;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

                                                     {Drag & Drop files to process}
procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  i: integer;

begin
  Screen.Cursor:=crHourGlass;
  btnClose.Enabled:=false;
  Memo1.Lines.Clear;
  Application.BringToFront;                          {Set focus to this program}
  try
    for i:=0 to high(FileNames) do
      PDFAddText(FileNames[i]);
  finally
    btnClose.Enabled:=true;
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

