(* LINUX: Extract text layer from PDF with pdftotext
=================================================

See: https://en.m.wikipedia.org/wiki/Pdftotext
     https://poppler.freedesktop.org/
     https://linux.die.net/man/1/pdftotext

Preparation: Install needed tools:
----------------------------------
You probably need to install poppler-utils (Ubuntu/Mint usually has it already):
    sudo apt install poppler-utils -y

Usage: pdftotext [options] <PDF-file> [<text-file>]
  -f <int>             : first page to convert
  -l <int>             : last page to convert
  -layout              : maintain original physical layout
  -raw                 : keep strings in content stream order
  -nopgbrk             : don't insert page breaks between pages
  -nodiag              : discard diagonal text  (set as default for all actions)
  -v                   : print copyright and version info
  -h                   : print usage information

*)

unit pdfextext_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ComCtrls, XMLPropStorage, Spin, Menus, Process, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnToTxt: TBitBtn;
    btnInfo: TBitBtn;
    btnClose: TBitBtn;
    cbLayout: TCheckBox;
    cbRaw: TCheckBox;
    cbNopgbrk: TCheckBox;
    cbPost: TCheckBox;
    edOptional: TEdit;
    ImageList1: TImageList;
    lblFile: TLabel;
    lblOptional: TLabel;
    lblProt: TLabel;
    lblTo: TLabel;
    lblFrom: TLabel;
    lblPagesHint: TLabel;
    lblBatchHint: TLabel;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    pcMain: TPageControl;
    PopupMenu1: TPopupMenu;
    speFrom: TSpinEdit;
    speTo: TSpinEdit;
    tbAll: TToggleBox;
    tsBatch: TTabSheet;
    tsPages: TTabSheet;
    tsProt: TTabSheet;
    XMLPropStorage1: TXMLPropStorage;
    procedure btnToTxtClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure Memo1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Memo1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure pcMainChange(Sender: TObject);
    procedure speFromChange(Sender: TObject);
    procedure speToChange(Sender: TObject);
    procedure tbAllChange(Sender: TObject);
  private

  public
    procedure PDF2Text(fn: string; mode: integer);   {Process one PDF file}
    procedure Processtext(fn: string);               {Process one text file}
  end;

{$I pdfextext_de.inc}
{ $I pdfextext_en.inc}

var
  Form1: TForm1;

const
  infofile='pdfextext.txt';
  app='pdftotext';
  pdfext='.pdf';
  txtext='.txt';
  separ='===================================';       {Visual separator for multiple files processed}
  tab2='  ';
  tab4='    ';

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);        {GUI Initialization}
begin
  Memo1.Lines.Clear;
  Caption:=capForm;
  btnToTxt.Caption:=capAddTxt;
  btnToTxt.Hint:=hntAddTxt;
  btnInfo.Caption:=capInfo;
  btnInfo.Hint:=hntInfo;
  btnClose.Caption:=capClose;
  btnClose.Hint:=hntClose;
  Memo1.Hint:=hntMemo;
  lblBatchHint.Caption:=hntBatch;
  lblPagesHint.Caption:=hntPages;
  lblFrom.Caption:=capFrom;
  lblTo.Caption:=capTo;
  tbAll.Caption:=capNotAll;
  tbAll.Hint:=hntAll;
  edOptional.Hint:=hntOptional;
  lblOptional.Caption:=capOptional;
  lblOptional.Hint:=hntOptional;

  tsBatch.Caption:=capBatch;
  pcMain.ActivePage:=tsBatch;
  tsPages.Caption:=capPages;
  tsProt.Caption:=capProt;

  cbLayout.Caption:=capLayout;
  cbRaw.Caption:=capRaw;
  cbNopgbrk.Caption:=capNopgbrk;
  cbLayout.Hint:=hntLayout;
  cbRaw.Hint:=hntRaw;
  cbNopgbrk.Hint:=hntNopgbrk;
  cbPost.Caption:=capPost;
  cbPost.Hint:=hntPost;

end;

function ofn(fn: string): string;                    {Define output file name raw}
begin
  result:=ChangeFileExt(fn, '')+'_raw.txt';
end;

procedure TForm1.ProcessText(fn: string);            {Process one raw text file}
var
  inlist, outlist: TStringList;
  i: integer;
  s: string;

begin
  if not cbPost.Checked then begin
    s:=ChangeFileExt(fn, '.txt');
    if RenameFile(ofn(fn), s) then
      Memo1.Lines.Add(hdrFin+s);
    exit;
  end;
  inlist:=TStringList.Create;
  outlist:=TStringList.Create;
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  try
    s:=ofn(fn);
    inlist.LoadFromFile(s);                          {Load raw text file}
    Memo1.Lines.Add(hdrRaw+s);

    for i:=0 to inlist.Count-1 do begin              {Process raw text file}
      s:=inlist[i];

      outlist.Add(s);
    end;

    s:=ChangeFileExt(fn, '.txt');
    if outlist.Count>0 then begin                    {Save processed file}
      outlist.SaveToFile(s);
      Memo1.Lines.Add(hdrFin+s);
    end;
    Memo1.Lines.Add(separ);
  finally
    inlist.Free;
    outlist.Free;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.PDF2Text(fn: string; mode: integer);  {Process one PDF file}
var
  cmd: TProcess;
  outlist: TStringList;
  i: integer;

begin
  if Lowercase(ExtractFileExt(fn))=pdfext then begin
    outlist:=TStringList.Create;
    Screen.Cursor:=crHourGlass;
    Application.ProcessMessages;
    cmd:=TProcess.Create(nil);
    lblFile.Caption:=fn;
    Application.ProcessMessages;
    try
      cmd.Executable:=app;
      cmd.Parameters.Clear;
      cmd.Options:=cmd.Options+[poWaitOnExit, poUsePipes];
      Memo1.Lines.Add(hdrPDF+fn);

      if (mode=1) and (not tbAll.Checked) then begin {Restrict to selected pages}
        cmd.Parameters.Add('-f');
        cmd.Parameters.Add(IntToStr(speFrom.Value));
        cmd.Parameters.Add('-l');
        cmd.Parameters.Add(IntToStr(speTo.Value));
      end;

      if cbLayout.Checked then
        cmd.Parameters.Add('-layout');
      if cbRaw.Checked then
        cmd.Parameters.Add('-raw');
      if cbNopgbrk.Checked then
        cmd.Parameters.Add('-nopgbrk');
      if edOptional.Text<>'' then
        cmd.Parameters.Add(edOptional.Text);
      cmd.Parameters.Add('-nodiag');                 {Default no diagonal text}
      cmd.Parameters.Add(fn);
      cmd.Parameters.Add(ofn(fn));
      cmd.Execute;

      outlist.LoadFromStream(cmd.Output);            {Get commandline output}
      for i:=0 to outlist.Count-1 do                 {Append to log}
        Memo1.Lines.Add(outlist[i]);

      ProcessText(fn);                               {Process text file}
    finally
      cmd.Free;
      outlist.Free;
      Screen.Cursor:=crDefault;
    end;
  end else begin
    Memo1.Lines.Add(ExtractFileName(fn)+errPDF);
  end;
end;

procedure TForm1.btnToTxtClick(Sender: TObject);     {Button Create text layer to process
                                                      files selected with open dialog}
var
  i: Integer;

begin
  OpenDialog1.Title:=capOpenPDF;                     {Option ofAllowMultiSelect must be set}
  if OpenDialog1.Execute then begin
    Screen.Cursor:=crHourGlass;
    btnClose.Enabled:=false;
    Memo1.Lines.Clear;                               {Empty log output}
    lblProt.Caption:=capNumFiles+IntToStr(OpenDialog1.Files.Count)+tab4+
                     capProc+'0';
    try
      for i:=0 to OpenDialog1.Files.Count-1 do begin
        PDF2Text(Opendialog1.Files[i], pcMain.ActivePageIndex);
        lblProt.Caption:=capNumFiles+IntToStr(OpenDialog1.Files.Count)+tab4+
                         capProc+IntToStr(i+1);
      end;
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
    pcMain.ActivePage:=tsBatch;
    for i:=0 to high(FileNames) do begin
      PDF2Text(FileNames[i], 0);
      lblProt.Caption:=capNumFiles+IntToStr(OpenDialog1.Files.Count)+tab4+
                       capProc+IntToStr(i+1);
    end;
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

procedure TForm1.pcMainChange(Sender: TObject);
begin
  if pcMain.ActivePage=tsPages then begin
    OpenDialog1.Options:=OpenDialog1.Options-[ofAllowMultiSelect];
  end else
    OpenDialog1.Options:=OpenDialog1.Options+[ofAllowMultiSelect];
end;

procedure TForm1.speFromChange(Sender: TObject);
begin
  if speTo.Value<speFrom.Value then
    speTo.Value:=speFrom.Value;
end;

procedure TForm1.speToChange(Sender: TObject);
begin
  if speFrom.Value>speTo.Value then
    speFrom.Value:=speTo.Value;
end;

procedure TForm1.tbAllChange(Sender: TObject);
begin
  if tbAll.Checked then begin
    speFrom.Enabled:=false;
    speTo.Enabled:=false;
    tbAll.Caption:=capAll;
    tbAll.Hint:=hntAll;
  end else begin
    speFrom.Enabled:=true;
    speTo.Enabled:=true;
    tbAll.Caption:=capNotAll;
    tbAll.Hint:='';
  end;
end;

procedure TForm1.btnInfoClick(Sender: TObject);      {Button Info}
begin
  pcMain.ActivePage:=tsProt;
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

