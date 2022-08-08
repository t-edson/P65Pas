uses Commodore64;
//P65Pas adaptation of code for Kickc compiler. 
// Original: https://www.youtube.com/watch?v=CWlCidFRz3I
var 
  SCREEN    : [1000]byte absolute $0400; 
  BITMAP    : [1]byte    absolute $2000;  //Size doesn't matter
  BORDERCOL : byte       absolute $d020;
  D011      : byte       absolute $d011;
  VIC_MEMORY: byte       absolute $d018;

const 
  VIC_BMM =  %00100000;
  VIC_DEN =  %00010000;
  VIC_RSEL = %00001000; 
  BLUE = 6;
var
  bitmask: []byte = [128, 64, 32, 16, 8, 4, 2, 1];

procedure plot(x, y: word);
var 
  location: ^byte;
  tmp: word;
  tmp2: byte;
begin
  location := @BITMAP;
  location += x and $fff8;
  location += y.low and 7;
  //location += (y >> 3) * 320;
  tmp := y >> 3;
  location += tmp << 8;
  location += tmp << 6;
  //location^ = location^ or bitmask[x and 7];
  tmp2 := bitmask[x and 7];
  location^ := location^ or tmp2;
end;

procedure line_xdyi(x, y, x1, xd, yd: word);
var 
  e: word;
begin
  e := yd >> 1;
  repeat
      plot(x,y);
      inc(x);
      e :=  e + yd;
      if xd<e then
          inc(y);
          e := e - xd;
      end;
  until x = x1+1;
end;

procedure line_xdyd(x, y, x1, xd, yd: word);
var
  e: word;
begin
  e := yd>>1;
  repeat
      plot(x,y);
      inc(x);
      e := e + yd;
      if xd < e then
          dec(y);
          e := e - xd;
      end;
  until x = x1+1;
end;

procedure line_ydxi(y, x, y1, yd, xd: word);
var
  e: word;
begin
  e := xd>>1;
  repeat
      plot(x,y);
      inc(y);
      e := e + xd;
      if yd < e then
          inc(x);
          e := e - yd;
      end;
  until y = y1+1;
end;

procedure line_ydxd(y, x, y1, yd, xd: word);
var
  e: word;
begin
  e := xd>>1;
  repeat
      plot(x,y);
      inc(y);
      e := e + xd;
      if yd < e then
          dec(x);
          e := e - yd;
      end;
  until y = y1+1;
end;

// Draw a line on the bitmap
procedure line(x0, y0, x1, y1: word);
var
  xd, yd: word;
begin
    if(x0<x1) then
        xd := x1-x0;
        if y0<y1 then
            yd := y1-y0;
            if yd<xd then
                line_xdyi(x0, y0, x1, xd, yd);
            else
                line_ydxi(y0, x0, y1, yd, xd);
            end;
        else
            yd := y0-y1;
            if yd<xd then
                line_xdyd(x0, y0, x1, xd, yd);
            else
                line_ydxd(y1, x1, y0, yd, xd);
            end;
        end;
    else 
        xd := x0-x1;
        if y0<y1 then
            yd := y1-y0;
            if yd<xd then
                line_xdyd(x1, y1, x0, xd, yd);
            else
                line_ydxd(y0, x0, y1, yd, xd);
            end;
        else
            yd := y0-y1;
            if yd<xd then
                line_xdyi(x1, y1, x0, xd, yd);
            else
                line_ydxi(y1, x1, y0, yd, xd);
            end;
        end;
    end;
end;

// Fill some memory with a value
procedure fill(byte* start, word size, byte val);
begin
    byte* end = start + size;
    for(byte* addr = start; addr!=end; addr++) do
        *addr = val;
    end;
end;

begin
    fill(BITMAP,40*25*8,0);
    fill(SCREEN,40*25,$16);
    *BORDERCOL = BLUE;
    *D011 = VIC_BMM|VIC_DEN|VIC_RSEL|3;
    *VIC_MEMORY =  (byte)((((word)SCREEN&$3fff)/$40)|(((word)BITMAP&$3fff)/$400));
    for (word x = 0; x <= 100; x += 4) {
        line(150-x,0+x >> 1,50+x,50);
        line(225-x << 1,0+x,25+x << 1 ,100);
        line(300-(x*3),0+x+(x >> 1),0+(x * 3) ,150);
    }
    do {
    } while (true);
end.

