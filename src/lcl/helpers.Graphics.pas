{*******************************************************************
 Project: Classes helpers

  Copyright (C) 2017 Gilson Nunes Rodrigues

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  Gilson Nunes Rodrigues - gilson.gnr@gmail.com
*******************************************************************}

unit helpers.Graphics;

{$i gdefines.inc}

interface

uses
  Classes, SysUtils, Windows,
  {$IFnDEF FPC}
  pngimage, pnglang,
  {$else}
  GraphType, LCLType, LCLIntf,
  {$ENDIF}
  Graphics, Types, math;

type
  {$IFDEF FPC}
  TPngImage = TPortableNetworkGraphic;
  {$else}
  TRasterImage  = TPngImage;
  TPngImage     = TRasterImage;
  {$ENDIF}

  TProcDraw = procedure (aDC: HDC; const aRectSrc, aRectDst: TRect; const aCopyMode: TCopyMode) of object;

  { THelperCanvas }

  THelperCanvas = class helper for TCanvas
  public
    //procedure LineX(X, Y, X2 : Integer);
    //procedure LineY(X, Y, Y2 : Integer);
    //procedure LineToX(X : Integer);
    //procedure LineToY(Y : Integer);
    //{$IFNDEF FPC}
    //procedure Line (x1,y1,x2,y2:integer); overload;
    //procedure Line (const p1,p2:TPoint); overload;
    //procedure Line (const points:TRect); overload;
    //{$else}
    //procedure TextRect(const aRect: TRect; const aText: string; aTextFormat: TTextFormat = []); overload;
    //{$ENDIF}
    //function GetAveCharSize(): TSize;
    //procedure FillRect(const aRect: TRect; aColor: TColor); overload;
    procedure FillRect(const aRect: TRect; aColor, aTransparentColor: TColor; aBlend: Byte); overload;
    //procedure FillRect(const aRect: TRect; aTransparentColor : TColor; aBlend: Byte = 255);
    procedure DrawSkin(aProcDraw: TProcDraw; const aRectDst: TRect; const aOrigX, aOrigY, aOrigW, aOrigH, aLeft,
                                                                              aTop, aRight, aBottom: Integer); overload;
    procedure DrawSkin(aProcDraw: TProcDraw; const aRectDst: TRect; aOrigW, aOrigH: Integer; const aCenter: TPoint); overload;
  end;

  { THelperPicture }

  THelperPicture = class Helper for TPicture
  public
    function IsEmpty() : Boolean;
    function ForceToPNG(): TPngImage;
  end;

  THelperGraphic = class Helper for TGraphic
  public
    function GetBounds(): TRect;
    //procedure AssignToAs(aDest: TRasterImage; aMaxW, aMaxH: Integer; aForceMax: Boolean = True);
  end;

  { TRasterImageHeper }

  TRasterImageHeper = class Helper(THelperGraphic) for TRasterImage
  private
    //procedure DoFillRect(const r: TRect; const AColor : TColorRef; aType: TFillColorType);
    //procedure DoMerge(ImgTop: TRasterImage; X, Y: Integer);
  public
    //procedure FastCopy(ImgSrc: TRasterImage; RecSrc: TRect; xDst, yDst: Integer; aGrayScale : TGrayScaleType; aColorTransp : TColorRef); overload;
    //procedure FastCopy(ImgSrc : TRasterImage; const RecSrc: TRect; xDst, yDst: Integer; aGrayScale : TGrayScaleType);  overload;
    //procedure FastCopy(ImgSrc : TRasterImage; xDst, yDst: Integer; aGrayScale : TGrayScaleType); overload;
    //procedure StretchSmooth(aImgSrc: TRasterImage; const aRectSrc, aRectDst: TRect); overload;
    //procedure StretchSmooth(aImgSrc: TRasterImage; const aRectSrc: TRect); overload;
    //procedure StretchSmooth(aImgSrc: TRasterImage); overload;
    //procedure Merge(aImgTop: TRasterImage; X, Y : Integer);
    //procedure Obfuscate(const aRect: TRect; const aDistance: Byte); overload;
    //procedure Obfuscate(const aDistance: Byte); overload;
    //procedure FillRect(const aRect: TRect; const AColor: TColorRef; aType : TFillColorType);
    //procedure Fill(const AColor: TColorRef; aType : TFillColorType);
    procedure DrawDC(aDC: HDC; const aRectSrc, aRectDst: TRect; const aCopyMode: TCopymode{$IFnDEF FPC}; aGrayScale: Boolean = False{$ENDIF}); overload;
    procedure Draw(aCanvas: TCanvas; const aRectSrc, aRectDst: TRect{$IFnDEF FPC}; aGrayScale: Boolean = False{$ENDIF}); overload;
  public
    {$IFDEF FPC}
    //const SuportGrayScaleDraw = False;
    {$else}
    //const SuportGrayScaleDraw = True;
    {$IF CompilerVersion <= 18.5}
    function SupportsPartialTransparency(): Boolean;
    {$IFEND}
    procedure SupportsPartialTransparencyCheck();
    {$ENDIF}
    procedure MakeHalfTransparent();
  end;

  TColorRefRec = packed record
    Red   : Byte;
    Green : Byte;
    Blue  : Byte;
    Alpha : Byte;
  end;

function RGBToColorRef(R, G, B: Byte): TColorRef;
function ColorToGrayScale(aColor: TColor) : TColorRef;
procedure ColorRGBToHLS(const clrRGB : TColorRef; out Hue, Luminance, Saturation : Word);
function HueToRGB(Lum, Sat, Hue : Double) : Integer;
function ColorHLSToRGB(Hue, Luminance, Saturation : Word) : TColorRef;
function ColorAdjustLuma(aColorRGB: TColorRef; n: Integer; Scale: Boolean) : TColorRef;

implementation

procedure _Exchange(var a, b: Integer);
var
  c: Integer;
begin
  c := a;
  a := b;
  b := c;
end;

function _RGBToGray(R, G, B : Byte) : Byte;
var
  i : Integer;
begin
  i := (R * 30 + G * 59 + B * 11) div 100;
  if i > 255 then
    Result := 255
  else Result := i;
end;

{ THelperCanvas }

procedure THelperCanvas.FillRect(const aRect : TRect; aColor, aTransparentColor: TColor; aBlend: Byte);
begin
  //CopyRectDC(Self.Handle, Self.Handle, aRect, aRect.Left, aRect.Top, aTransparentColor, aColor, aBlend);
end;

procedure THelperCanvas.DrawSkin(aProcDraw : TProcDraw; const aRectDst : TRect; const aOrigX, aOrigY, aOrigW, aOrigH,
                                                                                aLeft, aTop, aRight, aBottom : Integer);

  procedure _Draw(const srcRect, DestRect: TRect);
  begin
    aProcDraw(Self.Handle, srcRect, DestRect, 0);
  end;

var
  R2: TRect;
  R1: TRect;
  vOrigRight : Integer;
  vOrigBottom: Integer;
begin
  vOrigRight  := aOrigX + aOrigW;
  vOrigBottom := aOrigY + aOrigH;
  //Superior Esquerdo
  R1 := Bounds(aOrigX, aOrigY, aLeft, aTop);
  R2 := Bounds(aRectDst.Left, aRectDst.Top, aLeft, aTop);
  _Draw(R1, R2);
  //superior meio
  R1.Left  := R1.Right;
  R2.Left  := R2.Right;
  R1.Right := vOrigRight - aRight;
  R2.Right := aRectDst.Right - aRight;
  _Draw(R1, R2);
  //superior direito
  R1 := Bounds(vOrigRight - aRight, aOrigY, aRight, aTop);
  R2 := Bounds(aRectDst.Right - aRight, aRectDst.Top, aRight, aTop);
  _Draw(R1, R2);
  //meio esquerdo
  R1.Left   := aOrigX;
  R1.Right  := R1.Left + aLeft;
  R1.Top    := R1.Bottom;
  R2.Left   := aRectDst.Left;
  R2.Right  := aRectDst.Left + aLeft;
  R2.Top    := R2.Bottom;
  R1.Bottom := vOrigBottom - aBottom;
  R2.Bottom := aRectDst.Bottom - aBottom;
  _Draw(R1, R2);
  //meio meio
  R1.Left   := R1.Right;
  R2.Left   := R2.Right;
  R1.Right  := vOrigRight - aRight;
  R2.Right  := aRectDst.Right - aRight;
  _Draw(R1, R2);
  //meio direito
  R1.Left   := R1.Right;// := Rect(vOrigRight - aRight, aTop, vOrigRight, aTop);
  R2.Left   := R2.Right;// Rect(DWidth - aRight, aTop, DWidth, aTop);
  R1.Right  := vOrigRight;
  R2.Right  := aRectDst.Right;
  _Draw(R1, R2);
  //inferiror esquerdo
  R1 := Rect(aOrigX, vOrigBottom - aBottom, aOrigX + aLeft, vOrigBottom);
  R2 := Rect(aRectDst.Left, aRectDst.Bottom - aBottom, aRectDst.Left + aLeft, aRectDst.Bottom);
  _Draw(R1, R2);
  //inferior meio
  R1.Left   := R1.Right;
  R2.Left   := R2.Right;
  R1.Right  := vOrigRight - aRight;
  R2.Right  := aRectDst.Right - aRight;
  _Draw(R1, R2);
  //inferior direito
  R1 := Rect(vOrigRight - aRight, vOrigBottom - aBottom, vOrigRight, vOrigBottom);
  R2 := Rect(aRectDst.Right - aRight, aRectDst.Bottom - aBottom, aRectDst.Right, aRectDst.Bottom);
  _Draw(R1, R2);
end;

procedure THelperCanvas.DrawSkin(aProcDraw : TProcDraw; const aRectDst : TRect; aOrigW, aOrigH : Integer; const aCenter : TPoint);
begin
  DrawSkin(aProcDraw, aRectDst, 0, 0, aOrigW, aOrigH, aCenter.x, aCenter.y, aOrigW - aCenter.x - 1, aOrigH - aCenter.y - 1);
end;

{ THelperPicture }

function THelperPicture.IsEmpty: Boolean;
begin
  Result := (Graphic = nil) or Graphic.Empty;
end;

function THelperPicture.ForceToPNG : TPngImage;
begin
  {$IFDEF FPC}
  Result := Self.PNG;
  {$ELSE}
  if Self.Graphic is TPngImage then
    Result := TPngImage(Self.Graphic)
  else
  begin
    Result := TPngImage.CreateBlank(COLOR_RGBALPHA, 16, 1, 1);
    try
      Self.Assign(Result);
    finally
      Result.Free;
    end;
    Result := Self.Graphic as TPngImage;
  end;
  {$ENDIF}
end;

function THelperGraphic.GetBounds: TRect;
begin
  Result := Types.Rect(0, 0, Width, Height);
end;

{ TRasterImageHeper }

{$IFDEF FPC}
procedure TRasterImageHeper.DrawDC(aDC: HDC; const aRectSrc, aRectDst : TRect; const aCopyMode: TCopymode);
var
  UseMaskHandle: HBitmap;
  SrcDC: hDC;
begin
  if (aRectSrc.Right - aRectSrc.Left <= 0) or (aRectSrc.Bottom - aRectSrc.Top <= 0) then Exit;

  BitmapHandleNeeded;
  if not BitmapHandleAllocated then
    Exit;

  if Masked then
    UseMaskHandle := MaskHandle
  else
    UseMaskHandle := 0;

  SrcDC := Self.Canvas.GetUpdatedHandle([csHandleValid]);
  //DestCanvas.Changing;
  //DestDC := DestCanvas.GetUpdatedHandle([csHandleValid]);
  StretchMaskBlt(aDC,
          aRectDst.Left, aRectDst.Top,
          aRectDst.Right-aRectDst.Left, aRectDst.Bottom-aRectDst.Top,
          SrcDC, aRectSrc.Left, aRectSrc.Top,
          aRectSrc.Right - aRectSrc.Left, aRectSrc.Bottom - aRectSrc.Top,
          UseMaskHandle, aRectSrc.Left, aRectSrc.Top, aCopyMode);
  //DestCanvas.Changed;
end;

//procedure TRasterImageHeper.DrawDC(aDC: HDC; const aRectSrc, aRectDst : TRect; const aCopyMode: TCopymode{; aGrayScale: Boolean});
//begin
//  if aGrayScale then
//    if not SuportGrayScaleDraw then
//      raise Exception.Create('GrayScaleDraw n�o suportado');
//  DrawDC(aDC, aRectSrc, aRectDst, aCopyMode);
//end;

procedure TRasterImageHeper.Draw(aCanvas: TCanvas; const aRectSrc, aRectDst: TRect);
begin
  DrawDC(aCanvas.Handle, aRectSrc, aRectDst, aCanvas.CopyMode);
end;

//procedure TRasterImageHeper.Draw(aCanvas: TCanvas; const aRectSrc, aRectDst: TRect; aGrayScale: Boolean);
//begin
//  DrawDC(aCanvas.Handle, aRectSrc, aRectDst, aCanvas.CopyMode, aGrayScale);
//end;

procedure TRasterImageHeper.MakeHalfTransparent();
var
  vBpp          : Byte;
  vpPixel       : PByte;
  vpRow         : PByte;
  vBytesPerLine : Integer;
  vBottomToTop  : Boolean;

  procedure _IncPixel();
  begin
    Inc(vpPixel, vBpp);
  end;

  procedure _IncRow();
  var
    i : Integer;
  begin
    i := vBytesPerLine;

    if vBottomToTop then
      i := -i;

    Inc(vpRow, i);

    vpPixel := vpRow;
  end;

var
  vRawPtr: PRawImage;
  x, y: Integer;
begin
  //if (Self.ColorType = COLOR_PALETTE) then Exit;
  vRawPtr        := GetRawImagePtr();
  vBpp           := vRawPtr^.Description.BitsPerPixel div 8;
  vBytesPerLine  := vRawPtr^.Description.BytesPerLine;
  vBottomToTop   := (vRawPtr^.Description.LineOrder = riloBottomToTop);

  y := 0;//FY;
  if vRawPtr^.Description.LineOrder = riloBottomToTop then
    y := vRawPtr^.Description.Height - y;

  vpRow   := PByte(vRawPtr^.Data) {+ (vBpp * FX)} + (vBytesPerLine * y);
  vpPixel := vpRow;
  Self.BeginUpdate(False);
  try
    for Y := 0 to Self.Height - 1 do
    begin
      for X := 0 to Self.Width - 1 do
      begin
        with PRGBAQuad(vpPixel)^ do
          Alpha := Alpha div 3;

        _IncPixel();
      end;

      _IncRow();
    end;
  finally
    Self.EndUpdate(False);
  end;
end;

{$else ~fpc}
type
  TChunkIHDRHelper = class helper for TChunkIHDR
  public
    function GetBytesPerRow() : Integer; inline;
    function GetbiBitCount: Word;
  end;

{ TChunkIHDRHelper }

function TChunkIHDRHelper.GetBytesPerRow: Integer;
begin
  Result := BytesPerRow;
end;

function TChunkIHDRHelper.GetbiBitCount : Word;
begin
  Result := BytesPerRow;

  case ColorType of
    COLOR_GRAYSCALE, COLOR_PALETTE, COLOR_GRAYSCALEALPHA:
      case BitDepth of
        1, 4, 8: Result := BitDepth;
        2      : Result := 4;
        16     : Result := 8;
      end;
    COLOR_RGB, COLOR_RGBALPHA:  Result := 24;
  end;
end;

//procedure TRasterImageHeper.DrawDC(aDC: HDC; const aRectSrc, aRectDst: TRect; const aCopyMode: TCopymode);
//begin
//   DrawDC(aDC, aRectSrc, aRectDst, aCopyMode, False);
//end;

procedure TRasterImageHeper.DrawDC(aDC: HDC; const aRectSrc, aRectDst: TRect; const aCopyMode: TCopymode; aGrayScale: Boolean);

  procedure _AdjustRect(var Rect : TRect);
  begin
    if Rect.Right < Rect.Left then
      _Exchange(Rect.Right, Rect.Left);

    if Rect.Bottom < Rect.Top then
      _Exchange(Rect.Bottom, Rect.Top);
  end;

type
  TPixelLine = array[Word] of TRGBQuad;
  pPixelLine = ^TPixelLine;

var
  vBitmapInfo      : TBitmapInfo;
  vBufferDC : HDC;
  vBufferBits : Pointer;
  OldBitmap,
    vBufferBitmap : HBitmap;

  TransparencyChunk : TChunktRNS;
  PaletteChunk : TChunkPLTE;
  TransValue, vPaletteIndex : Byte;
  CurBit : Integer;
  Data : PByte;
  vBytesPerRowDest,
    vBytesPerRowSrc,
    vBytesPerRowAlpha : Integer;
  vImageSource1, {ImageSource2,}
    vAlphaSource1{, AlphaSource2} : pByteArray;
  vImageSourceOrgI   : Integer;
  vImageSourceStartI : LongInt;
  vAlphaSourceStartI : LongInt;
  vImageData : pPixelLine;
  vI, vJ, vI2, vJ2 : Integer;

  vdW, vdH : Integer;
  StretchX, StretchY : Boolean;
  FactorX : Double;
  FactorY : Double;
  vvR, vvG, vvB : Byte;
  vWInvert : Boolean;
  vHInvert : Boolean;
  vII : Integer;
  vAlpha : Byte;
  vHeader : TChunkIHDR;
  vRectSrc, vRectDst: TRect;
begin
  SupportsPartialTransparencyCheck();
  if (aRectSrc.Right = aRectSrc.Left) or (aRectSrc.Bottom = aRectSrc.Top) then exit;
  if (aRectDst.Right = aRectDst.Left) or (aRectDst.Bottom = aRectDst.Top) then exit;
  vRectSrc := aRectSrc;
  vRectDst := aRectDst;
  _AdjustRect(vRectSrc);
  vWInvert := vRectDst.Left > vRectDst.Right;
  vHInvert := vRectDst.Top > vRectDst.Bottom;
  _AdjustRect(vRectDst);

  vdW := vRectDst.Right - vRectDst.Left;
  vdH := vRectDst.Bottom - vRectDst.Top;

  StretchX := vdW <> (vRectSrc.Right - vRectSrc.Left);
  StretchY := vdH <> (vRectSrc.Bottom - vRectSrc.Top);

  if StretchX then
    FactorX := vdW / (vRectSrc.Right - vRectSrc.Left)
  else
    FactorX := 1;

  if StretchY then
    FactorY := vdH / (vRectSrc.Bottom - vRectSrc.Top)
  else
    FactorY := 1;

  if vRectSrc.Top < 0 then
  begin
    Dec(vdH, Trunc((-vRectSrc.Top) / FactorY));
    if not vHInvert then
      Inc(vRectDst.Top, Trunc((-vRectSrc.Top) / FactorY));

    vRectSrc.Top := 0;
  end;

  if vRectSrc.Left < 0 then
  begin
    Dec(vdW, Trunc((-vRectSrc.Left) / FactorX));
    if not vWInvert then
      Inc(vRectDst.Left, Trunc((-vRectSrc.Left) / FactorY));

    vRectSrc.Left := 0;
  end;

  Fillchar(vBitmapInfo, sizeof(vBitmapInfo), #0);

  with vBitmapInfo.bmiHeader do
  begin
    biSize          := sizeof(vBitmapInfo.bmiHeader);
    biWidth         := vdW;
    biHeight        := -Integer(vdH);
    biPlanes        := 1;
    biBitCount      := 32;
    biCompression   := BI_RGB;
    biSizeImage     := 0;
    biXPelsPerMeter := 0;
    biYPelsPerMeter := 0;
    biClrUsed       := 0;
    biClrImportant  := 0;
  end;

  vBufferDC := CreateCompatibleDC(0);
  if (vBufferDC = 0) then raise EPNGOutMemory.Create(EPNGOutMemoryText);
  vBufferBitmap := CreateDIBSection(vBufferDC, vBitmapInfo, DIB_RGB_COLORS, vBufferBits, 0, 0);
  if (vBufferBitmap = 0) or (vBufferBits = nil) then
  begin
    if vBufferBitmap <> 0 then DeleteObject(vBufferBitmap);
    DeleteDC(vBufferDC);
    raise EPNGOutMemory.Create(EPNGOutMemoryText);
  end;

  OldBitmap := SelectObject(vBufferDC, vBufferBitmap);
  BitBlt(vBufferDC, 0, 0, vdW, vdH, aDC, vRectDst.Left, vRectDst.Top, SRCCOPY);

  vHeader                 := Self.Header;
  vBytesPerRowAlpha       := vHeader.Width;
  vBytesPerRowDest        := (((vBitmapInfo.bmiHeader.biBitCount * vdW) + 31) and not 31) div 8;
  vBytesPerRowSrc         := vHeader.GetBytesPerRow;
  vImageData              := vBufferBits;
  vAlphaSourceStartI      :=  Longint(Self.AlphaScanline[0]);
  Longint(vAlphaSource1)  := vAlphaSourceStartI + vBytesPerRowAlpha * vRectSrc.Top;
  vImageSourceOrgI        := Longint(Self.Scanline[Self.Height - 1]) + vBytesPerRowSrc * Longint(vHeader.Height - 1);
  Longint(vImageSource1)  := vImageSourceOrgI - vBytesPerRowSrc * vRectSrc.Top;

  case vHeader.GetbiBitCount of
    {RGB images}
    24 :
      for vJ := 1 to vdH do
      begin
        if vHInvert then
          Longint(vImageData) :=  Longint(vBufferBits) + vBytesPerRowDest * (vdH - vJ)
        else
          Longint(vImageData) :=  Longint(vBufferBits) + vBytesPerRowDest * (vJ - 1);//      s Inc(, vBytesPerRowDest);

        for vI := 0 to vdW - 1 do
        begin
          if StretchX then
            //vI2 := Round((vI * 1000) / iFactorX)
            vI2 := Trunc(vI / FactorX)
          else
            vI2 := vI;

          Inc(vI2, vRectSrc.Left);
          if vI2 >= Self.Width then
            Break;

          if vWInvert then
            vII := vdW - vI - 1
          else
            vII := vI;

          vAlpha := vAlphaSource1[vI2];

          if (vAlpha <> 0) then
          begin
            vvR := vImageSource1[2 + vI2 * 3];
            vvG := vImageSource1[1 + vI2 * 3];
            vvB := vImageSource1[0 + vI2 * 3];

            if aGrayScale then
            begin
              vvR := _RGBToGray(vvR, vvG, vvB);
              vvG := vvR;
              vvB := vvR;
            end;

            if (vAlpha = 255) then
            begin
              with vImageData[vII] do
              begin
                rgbRed   := vvR;
                rgbGreen := vvG;
                rgbBlue  := vvB;
              end;
            end
            else
            begin
              with vImageData[vII] do
              begin
                rgbRed   := (255 + vvR * vAlpha + rgbRed   * (not vAlpha)) shr 8;
                rgbGreen := (255 + vvG * vAlpha + rgbGreen * (not vAlpha)) shr 8;
                rgbBlue  := (255 + vvB * vAlpha + rgbBlue  * (not vAlpha)) shr 8;
              end;
            end;
          end;
        end;

        if StretchY then
          //vJ2 := Trunc(vJ / FactorY)
          vJ2 := Trunc(vJ / FactorY)
        else
          vJ2 := vJ;

        Inc(vJ2, vRectSrc.Top);

        if vJ2 >= Self.Height  then
          Break;

        Longint(vImageSource1) := vImageSourceOrgI - vBytesPerRowSrc * vJ2;
        Longint(vAlphaSource1) := vAlphaSourceStartI + vBytesPerRowAlpha * vJ2;
      end;
    1, 4, 8 :
      if vHeader.ColorType = COLOR_GRAYSCALEALPHA then
      begin
        for vJ := 1 to vdH do
        begin
          for vI := 0 to vdW - 1 do
            with vImageData[vI]{, vHeader.vBitmapInfo} do
            begin
              if StretchX then
                //vI2 := Round((vI * 1000) / iFactorX)
                vI2 := Round(vI / FactorX)
              else
                vI2 := vI;

              Inc(vI2, vRectSrc.Left);

              if vI2 >= Self.Width then
                Break;

              vAlpha := vAlphaSource1[vI2];
              vvR := vImageSource1[vI2];
              rgbRed :=   (255 + vvR * vAlpha + rgbRed   * (255 - vAlpha)) shr 8;
              rgbGreen := (255 + vvR * vAlpha + rgbGreen * (255 - vAlpha)) shr 8;
              rgbBlue :=  (255 + vvR * vAlpha + rgbBlue  * (255 - vAlpha)) shr 8;
            end;

          Longint(vImageData) := Longint(vImageData) + vBytesPerRowDest;

        if StretchY then
          //vJ2 := Round((vJ * 1000 + 1) / iFactorY)
            vJ2 := Round(vJ / FactorY)
        else
          vJ2 := vJ;

          Inc(vJ2, vRectSrc.Top);
          if vJ2 >= Self.Height then
            Break;

          Longint(vImageSource1) := vImageSourceOrgI - vBytesPerRowSrc * vJ2;
          Longint(vAlphaSource1) := vAlphaSourceStartI + vBytesPerRowAlpha * vJ2;
        end;
      end
      else
      begin
        TransparencyChunk := TChunktRNS(Self.Chunks.ItemFromClass(TChunktRNS));
        PaletteChunk      := TChunkPLTE(Self.Chunks.ItemFromClass(TChunkPLTE));

        for vJ := 1 to vdH do
        begin
          vI := 0;
          repeat
            CurBit := 0;
            if StretchX then
              //vI2 := Round((vI * 1000) / iFactorX)
              vI2 := Round(vI / FactorX)
            else
              vI2 := vI;

            Inc(vI2, vRectSrc.Left);
            if vI2 >= Self.Width then
              Break;

            Data := @vImageSource1[vI2];

            repeat
              case vHeader.BitDepth of
                1 : vPaletteIndex := (Data^ shr (7 - (vI2 mod 8))) and 1;
                2, 4 : vPaletteIndex := (Data^ shr ((1 - (vI2 mod 2)) * 4)) and $0F;
                else
                  vPaletteIndex := Data^;
              end;

              vvR :=  PaletteChunk.Item[vPaletteIndex].rgbRed;
              vvG :=  PaletteChunk.Item[vPaletteIndex].rgbGreen;
              vvB :=  PaletteChunk.Item[vPaletteIndex].rgbBlue;
              if aGrayScale then
              begin
                vvR := _RGBToGray(vvR, vvG, vvB);
                vvG := vvR;
                vvB := vvR;
              end;

              with vImageData[vI] do
              begin
                TransValue := TransparencyChunk.PaletteValues[vPaletteIndex];
                rgbRed := (255 + vvR * TransValue + rgbRed * (255 - TransValue)) shr 8;
                rgbGreen := (255 + vvG *  TransValue + rgbGreen * (255 - TransValue)) shr 8;
                rgbBlue := (255 + vvB * TransValue + rgbBlue * (255 - TransValue)) shr 8;
              end;

              Inc(vI);
              Inc(CurBit, vHeader.GetbiBitCount);
            until CurBit >= 8;
          until vI >= Integer(vdW);

          Longint(vImageData) := Longint(vImageData) + vBytesPerRowDest;

        if StretchY then
          //  vJ2 := Round((vJ * 1000 + 1) / iFactorY)
          vJ2 := Round(vJ / FactorY)
        else
          vJ2 := vJ;

          Inc(vJ2, vRectSrc.Top);
          if vJ2 >= Self.Height then
            Break;

          Longint(vImageSource1) := vImageSourceOrgI - vBytesPerRowSrc * vJ2;
        end
      end
  end;

  BitBlt(aDC, vRectDst.Left, vRectDst.Top, vdW, vdH, vBufferDC, 0, 0, SRCCOPY);
  SelectObject(vBufferDC, OldBitmap);
  DeleteObject(vBufferBitmap);
  DeleteDC(vBufferDC);
end;

//procedure TRasterImageHeper.Draw(aCanvas: TCanvas; const aRectSrc, aRectDst: TRect);
//begin
//  Draw(aCanvas, aRectSrc, aRectDst, False);
//end;

procedure TRasterImageHeper.Draw(aCanvas: TCanvas; const aRectSrc, aRectDst: TRect; aGrayScale: Boolean);
begin
  DrawDC(aCanvas.Handle, aRectSrc, aRectDst, aCanvas.CopyMode, aGrayScale);
end;

{$IF CompilerVersion <= 18.5}
function TRasterImageHeper.SupportsPartialTransparency(): Boolean;
begin
  Result := TransparencyMode <> ptmPartial;
end;
{$IFEND}

procedure TRasterImageHeper.SupportsPartialTransparencyCheck;
begin
  if not SupportsPartialTransparency then
    raise Exception.Create('Imagem n�o suporta transpar�ncia parcial');
end;

procedure TRasterImageHeper.MakeHalfTransparent();
var
  i, j: Integer;
  pScan: pngimage.pByteArray;
begin
  if (Self.Header.ColorType <> COLOR_PALETTE) then
    for j := 0 to Self.Height - 1 do
    begin
      pScan := Self.AlphaScanline[J];
      for i := 0 to Self.Width - 1 do
        pScan^[i] := pScan^[i] div 3;
    end;
end;
{$ENDIF}

function RGBToColorRef(R, G, B: Byte): TColorRef;
begin
  Result := (B shl 16) or (G shl 8) or R;
end;

function ColorToGrayScale(aColor : TColor) : TColorRef;
var
  b : Byte;
begin
  if aColor = aColor then //) or (ColorToRGB(Color) = ColorToRGB(clBtnFace)) then
    Result := aColor
  else
  begin
    Result := ColorToRGB(aColor);
    with TColorRefRec(aColor) do
      b := _RGBToGray(Red, Green, Blue);

    with TColorRefRec(Result) do
    begin
      Red    := b;
      Green  := b;
      Blue   := b;
      Alpha  := 0;
    end;
  end;
end;

const
  HLSMAX = 240;
  RGBMAX = 255;
  HLSUndefined = (HLSMAX * 2 / 3);

threadvar
  CachedRGBToHLSclrRGB : TColorRef;
  CachedRGBToHLSHue : WORD;
  CachedRGBToHLSLum : WORD;
  CachedRGBToHLSSat : WORD;

procedure ColorRGBToHLS(const clrRGB : TColorRef; out Hue, Luminance, Saturation : Word);
var
  H, L, S : Double;
  cMax, cMin : Double;
  Rdelta, Gdelta, Bdelta : Extended;
  c : TColorRefRec absolute clrRGB;
begin
  if clrRGB = CachedRGBToHLSclrRGB then
  begin
    Hue         := CachedRGBToHLSHue;
    Luminance   := CachedRGBToHLSLum;
    Saturation  := CachedRGBToHLSSat;
    exit;
  end;

  cMax := Math.Max(Math.Max(c.Red, c.Green), c.Blue);
  cMin := Math.Min(Math.Min(c.Red, c.Green), c.Blue);
  L := (((cMax + cMin) * HLSMAX) + RGBMAX) / (2 * RGBMAX);
  if cMax = cMin then { r=g=b --> achromatic case }
  begin
    Hue := Round(HLSUndefined);
    Luminance := Round(L);
    Saturation := 0;
  end
  else
  begin
    if L <= HLSMAX / 2 then
      S := (((cMax - cMin) * HLSMAX) + ((cMax + cMin) / 2)) / (cMax + cMin)
    else
      S := (((cMax - cMin) * HLSMAX) + ((2 * RGBMAX - cMax - cMin) / 2)) / (2 * RGBMAX - cMax - cMin);

    { hue }
    Rdelta := (((cMax - c.Red  ) * (HLSMAX / 6)) + ((cMax - cMin) / 2)) / (cMax - cMin);
    Gdelta := (((cMax - c.Green) * (HLSMAX / 6)) + ((cMax - cMin) / 2)) / (cMax - cMin);
    Bdelta := (((cMax - c.Blue ) * (HLSMAX / 6)) + ((cMax - cMin) / 2)) / (cMax - cMin);

    if (c.Red = cMax) then
      H := Bdelta - Gdelta
    else if (c.Green = cMax) then
      H := (HLSMAX / 3) + Rdelta - Bdelta
    else // B == cMax
      H := ((2 * HLSMAX) / 3) + Gdelta - Rdelta;

    if (H < 0) then
      H := H + HLSMAX;
    if (H > HLSMAX) then
      H := H - HLSMAX;
    Hue := Round(H);
    Luminance := Round(L);
    Saturation := Round(S);
  end;
  CachedRGBToHLSclrRGB := clrRGB;
  CachedRGBToHLSHue := Hue;
  CachedRGBToHLSLum := Luminance;
  CachedRGBToHLSSat := Saturation;
end;

function HueToRGB(Lum, Sat, Hue : Double) : Integer;
var
  ResultEx : Double;
begin
  { range check: note values passed add/subtract thirds of range }
  if (hue < 0) then
    hue := hue + HLSMAX;

  if (hue > HLSMAX) then
    hue := hue - HLSMAX;

  { return r,g, or b value from this tridrant }
  if (hue < (HLSMAX / 6)) then
    ResultEx := Lum + (((Sat - Lum) * hue + (HLSMAX / 12)) / (HLSMAX / 6))
  else if (hue < (HLSMAX / 2)) then
    ResultEx := Sat
  else if (hue < ((HLSMAX * 2) / 3)) then
    ResultEx := Lum + (((Sat - Lum) * (((HLSMAX * 2) / 3) - hue) + (HLSMAX / 12)) / (HLSMAX / 6))
  else
    ResultEx := Lum;
  Result := Round(ResultEx);
end;

function _SingleToByte(const Value : Single) : Byte;
begin
  if Value > 255.0 then
    Result := 255
  else if Value < 0.0 then
    Result := 0
  else
    Result := Round(Value);
end;

function ColorHLSToRGB(Hue, Luminance, Saturation : Word) : TColorRef;
var
  R, G, B : Double; { RGB component values }
  Magic1, Magic2 : Double; { calculated magic numbers (really!) }
begin
  if (Saturation = 0) then
  begin { achromatic case }
    R := (Luminance * RGBMAX) / HLSMAX;
    G := R;
    B := R;
    if (Hue <> HLSUndefined) then
      ; { ERROR }
  end
  else
  begin { chromatic case }
    { set up magic numbers }
    if (Luminance <= (HLSMAX / 2)) then
      Magic2 := (Luminance * (HLSMAX + Saturation) + (HLSMAX / 2)) / HLSMAX
    else
      Magic2 := Luminance + Saturation - ((Luminance * Saturation) + (HLSMAX / 2)) / HLSMAX;
    Magic1 := 2 * Luminance - Magic2;

    { get RGB, change units from HLSMAX to RGBMAX }
    R := (HueToRGB(Magic1, Magic2, Hue + (HLSMAX / 3)) * RGBMAX + (HLSMAX / 2)) / HLSMAX;
    G := (HueToRGB(Magic1, Magic2, Hue) * RGBMAX + (HLSMAX / 2)) / HLSMAX;
    B := (HueToRGB(Magic1, Magic2, Hue - (HLSMAX / 3)) * RGBMAX + (HLSMAX / 2)) / HLSMAX;
  end;
  Result := RGBToColorRef(_SingleToByte(R), _SingleToByte(G), _SingleToByte(B));
end;

function ColorAdjustLuma(aColorRGB: TColorRef; n: Integer; Scale: Boolean) : TColorRef;
var
  H, L, S : Word;
begin
  ColorRGBToHLS(aColorRGB, H, L, S);
  if Scale then
    Inc(n, L);

  if n < 0 then
    N := 0;

  Result := TColor(ColorHLSToRGB(H, n, S));
end;

end.
