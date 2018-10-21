// Demonstration of using TOM.pas, the windowless rich-edit control, and
// the text object model
// http://www.cs.wisc.edu/~rkennedy/windowless-rtf

// Copyright © 2003-2006 Rob Kennedy. Some rights reserved.
// For license information, see http://www.cs.wisc.edu/~rkennedy/license

unit RTFPaint;

interface

uses Windows, Graphics;

// The RTF parameter should be a string containing a full RTF document. It
// will not work if it is just an RTF fragment.
procedure DrawRTF(Canvas: TCanvas; const RTF: string; const Rect: TRect; const Transparent, WordWrap: Boolean);

implementation

uses SysUtils, ComObj, ActiveX, RichEdit, Messages, TOM;

function StrCpyN(dest: PChar; const src: PChar; cchMax: Integer): PChar; stdcall; external 'shlwapi.dll' name 'StrCpyNA';
function StrCpyNA(dest: PAnsiChar; const src: PAnsiChar; cchMax: Integer): PAnsiChar; stdcall; external 'shlwapi.dll';
function StrCpyNW(dest: PWideChar; const src: PWideChar; cchMax: Integer): PWideChar; stdcall; external 'shlwapi.dll';

type
  TDrawRTFTextHost = class(TTextHostImpl)
  private
    FDefaultCharFormat: PCharFormatW;
    FDefaultParaFormat: PParaFormat;
    FRect: TRect;
    FTransparent, FWordWrap: Boolean;
  protected
    // TTextHostImpl
    function TxGetClientRect(out prc: TRect): HResult; override;
    function TxGetCharFormat(out ppCF: PCharFormatW): HResult; override;
    function TxGetParaFormat(out ppPF: PParaFormat): HResult; override;
    function TxGetBackStyle(out pstyle: TTxtBackStyle): HResult; override;
    function OnTxCharFormatChange(const pcf: TCharFormatW): HResult; override;
    function OnTxParaFormatChange(const ppf: TParaFormat): HResult; override;
    function TxGetPropertyBits(dwMask: DWord; out pdwBits: DWord): HResult; override;
    function TxNotify(iNotify: DWord; pv: Pointer): HResult; override;
  public
    constructor Create(const ARect: TRect; const ATransparent, AWordWrap: Boolean);
    destructor Destroy; override;
  end;

  PCookie = ^TCookie;
  TCookie = record
    dwSize, dwCount: Cardinal;
    Text: PChar;
  end;

function EditStreamInCallback(dwCookie: Longint; pbBuff: PByte; cb: Longint; var pcb: Longint): Longint; stdcall;
var
  Cookie: PCookie;
begin
  Result := 0;

  Cookie := PCookie(dwCookie);
	if Cookie.dwSize - Cookie.dwCount < Cardinal(cb) then pcb := Cookie.dwSize - Cookie.dwCount
	else pcb := cb;

  if pcb <= 0 then exit;

	CopyMemory(pbBuff, Cookie.Text, pcb);
	Inc(Cookie.dwCount, pcb);
  Inc(Cookie.Text, pcb);
end;

procedure DrawRTF(Canvas: TCanvas; const RTF: string; const Rect: TRect; const Transparent, WordWrap: Boolean);
var
  Host: ITextHost;
  Unknown: IUnknown;
  Services: ITextServices;
  HostImpl: TTextHostImpl;
  Stream: TEditStream;
  Cookie: TCookie;
  res: Integer;
begin
  HostImpl := TDrawRTFTextHost.Create(Rect, Transparent, WordWrap);
  Host := CreateTextHost(HostImpl);
  OleCheck(CreateTextServices(nil, Host, Unknown));
  Services := Unknown as ITextServices;
  Unknown := nil;
  PatchTextServices(Services);

  Cookie.dwCount := 0;
  Cookie.dwSize := Length(RTF);
  Cookie.Text := PChar(RTF);
  Stream.dwCookie := Integer(@Cookie);
  Stream.dwError := 0;
  Stream.pfnCallback := EditStreamInCallback;
  OleCheck(Services.TxSendMessage(em_StreamIn, sf_RTF or sff_PlainRTF, lParam(@Stream), res));

  OleCheck(Services.TxDraw(dvAspect_Content, 0, nil, nil, Canvas.Handle, 0, Rect, PRect(nil)^, PRect(nil)^, nil, 0, txtView_Inactive));
  Services := nil;
  Host := nil;
end;

{ TDrawRTFTextHost }

constructor TDrawRTFTextHost.Create(const ARect: TRect; const ATransparent, AWordWrap: Boolean);
begin
  inherited Create;
  FRect := ARect;
  FTransparent := ATransparent;
  FWordWrap := AWordWrap;

  GetMem(FDefaultCharFormat, SizeOf(FDefaultCharFormat^));
  FillChar(FDefaultCharFormat^, SizeOf(FDefaultCharFormat^), 0);
  FDefaultCharFormat.cbSize := SizeOf(FDefaultCharFormat^);

  Cardinal(FDefaultCharFormat.dwMask) := cfm_Bold or cfm_Charset or {cfm_Color or} cfm_Face or cfm_Italic or cfm_Offset or cfm_Protected or {cfm_Size or} cfm_Strikeout or cfm_Underline;
  FDefaultCharFormat.dwEffects := 0;
  FDefaultCharFormat.yHeight := 8 * 20;
  FDefaultCharFormat.crTextColor := ColorToRGB(clBlack);
  FDefaultCharFormat.bCharSet := Default_Charset;
  FDefaultCharFormat.bPitchAndFamily := Default_Pitch or ff_DontCare;
  StrCpyNW(FDefaultCharFormat.szFaceName, 'Tahoma', SizeOf(FDefaultCharFormat.szFaceName) div SizeOf(FDefaultCharFormat.szFaceName[0]));

  GetMem(FDefaultParaFormat, SizeOf(FDefaultParaFormat^));
  FillChar(FDefaultParaFormat^, SizeOf(FDefaultParaFormat^), 0);
  FDefaultParaFormat.cbSize := SizeOf(FDefaultParaFormat^);

  FDefaultParaFormat.dwMask := pfm_All;
  FDefaultParaFormat.wAlignment := pfa_Left;
  FDefaultParaFormat.cTabCount := 1;
  FDefaultParaFormat.rgxTabs[0] := lDefaultTab;
end;

destructor TDrawRTFTextHost.Destroy;
begin
  FreeMem(FDefaultCharFormat);
  FreeMem(FDefaultParaFormat);
  inherited;
end;

function TDrawRTFTextHost.OnTxCharFormatChange(const pcf: TCharFormatW): HResult;
var
  NewCharFormat: PCharFormatW;
begin
  try
    GetMem(NewCharFormat, pcf.cbSize);
    Move(pcf, NewCharFormat^, pcf.cbSize);
    FreeMem(FDefaultCharFormat);
    PCharFormatW(FDefaultCharFormat) := NewCharFormat;
    Result := S_OK;
  except
    Result := E_Fail;
  end;
end;

function TDrawRTFTextHost.OnTxParaFormatChange(const ppf: TParaFormat): HResult;
var
  NewParaFormat: PParaFormat;
begin
  try
    GetMem(NewParaFormat, ppf.cbSize);
    Move(ppf, NewParaFormat^, ppf.cbSize);
    FreeMem(FDefaultParaFormat);
    PParaFormat(FDefaultParaFormat) := NewParaFormat;
    Result := S_OK;
  except
    Result := E_Fail;
  end;
end;

function TDrawRTFTextHost.TxGetBackStyle(out pstyle: TTxtBackStyle): HResult;
begin
  if FTransparent then
    pstyle := txtBack_Transparent
  else
    pstyle := txtBack_Opaque;
  Result := S_OK;
end;

function TDrawRTFTextHost.TxGetCharFormat(out ppCF: PCharFormatW): HResult;
begin
  ppCF := PCharFormatW(FDefaultCharFormat);
  Result := S_OK;
end;

function TDrawRTFTextHost.TxGetClientRect(out prc: TRect): HResult;
begin
  prc := FRect;
  Result := S_OK;
end;

function TDrawRTFTextHost.TxGetParaFormat(out ppPF: PParaFormat): HResult;
begin
  ppPF := PParaFormat(FDefaultParaFormat);
  Result := S_OK;
end;

function TDrawRTFTextHost.TxGetPropertyBits(dwMask: DWord; out pdwBits: DWord): HResult;
begin
  pdwBits := txtBit_DisableDrag or txtBit_Multiline or txtBit_RichText;
  if FWordWrap then
    pdwBits := pdwBits or txtBit_WordWrap;
  pdwBits := pdwBits and dwMask;
  Result := S_OK;
end;

function TDrawRTFTextHost.TxNotify(iNotify: DWord; pv: Pointer): HResult;
begin
  case iNotify of
    en_Update: Result := S_OK;
    else Result := inherited TxNotify(iNotify, pv);
  end;
end;

end.
