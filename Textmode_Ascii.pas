program Retro_Demo_Engine_By_ISO;

{$mode objfpc}
{$H+}
{$R-}
{$Q-}

uses
  SDL2, Classes, SysUtils, openmpt,vga_font;

const
  SCREEN_W_320 = 320;
  SCREEN_H_200 = 200;

  SCREEN_W_640 = 640;
  SCREEN_H_400 = 400;

  SCREEN_SIZE = 64000; { 13h flipit toimii vain tällä eli flip ja flip_with_palette }
  
  
  AUDIO_FREQ = 44100;
  AUDIO_SAMPLES = 2048;


type
  PByteArray = ^TByteArray;
  TByteArray = array[0..65535] of Byte;
  
  _256k_ByteArray = ^Big_ByteArray;
  Big_ByteArray = array[0..255999] of Byte;

var
  vs1, vs2, vs3, vs4, vs5, vs6: PByteArray;
  txt_vs1, txt_vs2, txt_vs3, txt_vs4 : _256k_ByteArray;
  
  { --- SDL --- }
  window: PSDL_Window;
  renderer: PSDL_Renderer;
  texture: PSDL_Texture;
  running: Boolean;
  event: TSDL_Event;
  FontTexture: PSDL_Texture; // Fontti textuuri --> SDL
  
  
  // Musiikkimuuttujat
  music_file: string;
  openmpt_mod: openmpt_module;
  current_order, current_row: Integer;

  { ----------- }
  
  palette: array[0..255] of Cardinal;
  pal1,pal2: array[0..767] of Byte;
  f1:file;
  paletti: Byte;
  x,y,z: word;
  a,b,c,e:word;
  d,f:integer;
  {q,w,ww,dd:word; }
  q:word;
  w,ww,dd:integer;
  xx,yy,zz:word;
  tdist,tangle:byte;
  sintable, costable: array[0..255] of Integer;
  sin2table, cos2table: array[0..255] of Integer;
  si1: array[0..255] of Byte;
  sini_kekkonen16:array[0..255] of byte;
  urho2:word;
  liika,puku:word;
  tunneli1,tunneli2,tunneli3,tunneli4:word;
  kokoa_tunneli:longword;
  
{ --- SDL2 Audio Callback --- }
procedure AudioCallback(userdata: Pointer; stream: PUInt8; len: LongInt); cdecl;
var
  samples_needed: NativeUInt;
  samples_rendered: NativeUInt;
  buffer_int: PSmallInt;
begin
  if openmpt_mod = nil then
  begin
    FillChar(stream^, len, 0);
    Exit;
  end;
  
  samples_needed := len div 4;  // 2 channels * 2 bytes per sample
  buffer_int := PSmallInt(stream);
  
  
  samples_rendered := openmpt_module_read_interleaved_stereo(
    openmpt_mod, 
    AUDIO_FREQ, 
    samples_needed, 
    PSingle(buffer_int)
  );
  
  
  if samples_rendered < samples_needed then
    FillChar(buffer_int[samples_rendered * 2], (samples_needed - samples_rendered) * 4, 0);
end;

{ --- Musiikin alustus --- }
function InitMusic(filename: string): Boolean;
var
  filedata: TMemoryStream;
  error: Integer;
  audio_spec: TSDL_AudioSpec;
begin
  Result := False;
  
  if not FileExists(filename) then
  begin
    WriteLn('Ei löydy musa tiedostoa... ', filename);
    Exit;
  end;
  
  // Lataa moduuli
  filedata := TMemoryStream.Create;
  try
    filedata.LoadFromFile(filename);
    
    error := 0;
    openmpt_mod := openmpt_module_create_from_memory2(
      filedata.Memory,
      filedata.Size,
      nil, nil, nil, nil,
      @error,
      nil,
      nil
    );
    
    if openmpt_mod = nil then
    begin
      WriteLn('Oho... Moduulin lataus epäonnistui! ', error);
      Exit;
    end;
  (*  
    WriteLn('Moduuli ladattu: ', filename);
    WriteLn('Kesto: ', openmpt_module_get_duration_seconds(openmpt_mod):0:2, ' sek');
  *) 
    // Aseta looppaamaan
    openmpt_module_set_repeat_count(openmpt_mod, -1);
    
  finally
    filedata.Free;
  end;
  
  // Alusta SDL audio
  FillChar(audio_spec, SizeOf(audio_spec), 0);
  audio_spec.freq := AUDIO_FREQ;
  audio_spec.format := AUDIO_S16SYS;
  audio_spec.channels := 2;
  audio_spec.samples := AUDIO_SAMPLES;
  audio_spec.callback := @AudioCallback;
  audio_spec.userdata := nil;
  
  if SDL_OpenAudio(@audio_spec, nil) < 0 then
  begin
    WriteLn('SDL Audio ei toimi: ', SDL_GetError());
    Exit;
  end;
  
  // Aloita toisto
  SDL_PauseAudio(0);
  
  WriteLn('Musa soi!');
  WriteLn('');
  Result := True;
end;

procedure CloseMusic;
begin
  SDL_CloseAudio;
  
  if openmpt_mod <> nil then
  begin
    openmpt_module_destroy(openmpt_mod);
    openmpt_mod := nil;
  end;
end;

{ MIDAS-tyylinen synkronointifunktio }
procedure duo(duppos, durpos: integer);
begin
  repeat
    current_order := openmpt_module_get_current_order(openmpt_mod);
    current_row := openmpt_module_get_current_row(openmpt_mod);
    SDL_Delay(1);  
  until ((current_order) = duppos) and ((current_row) >= durpos);
end;

function HaeVU(kanava: integer): integer;
var
  vu: Single;
begin
  if openmpt_mod = nil then
  begin
    HaeVU := 0;
    exit;
  end;
  vu := openmpt_module_get_current_channel_vu_mono(openmpt_mod, kanava);
  { Skaalataan 0.0-1.0 --> 0-63 }
  HaeVU := round(vu * 63);
end;

{ --- Musa jutut loppuvat --- }

procedure smooth320(src,dst:PByteArray);
 var
   x,y:integer;
   d:word;
begin
  for y:=0 to 199 do 
   for x:=0 to 319 do 
 begin
   d:=0;
   { Yläpuoli: Y-1 }
    if (y > 0) then d:=d + src^[(y-1)*320+x];
   { Alapuoli Y+1 } 
    if (y < 199) then d:=d + src^[(y+1)*320+x];
   { Vasen X-1 } 
    if (x > 0) then d:=d + src^[y*320+(x-1)];
   { Oikea X+1 } 
    if (x < 319) then d:=d + src^[y*320+(x+1)];
 { Keskiarvo kaikista ja tallennus kohteeseen." shr 2 = div 4 " }
  dst^[y*320+x]:=d shr 2
 end;
end;

procedure smooth256(src,dst:PByteArray);
 var
   x,y:integer;
   d:word;
begin
  for y:=0 to 255 do 
   for x:=0 to 255 do 
 begin
   d:=0;
   { Yläpuoli: Y-1 }
    if (y > 0) then d:=d + src^[(y-1)*256+x];
   { Alapuoli Y+1 } 
    if (y < 255) then d:=d + src^[(y+1)*256+x];
   { Vasen X-1 } 
    if (x > 0) then d:=d + src^[y*256+(x-1)];
   { Oikea X+1 } 
    if (x < 255) then d:=d + src^[y*256+(x+1)];
 { Keskiarvo kaikista ja tallennus kohteeseen." shr 2 = div 4 " }
  dst^[y*256+x]:=d shr 2
 end;
end;

procedure delupdown(dst: PByteArray; how: word);
var
  x, y: integer;
begin
  { Nollaa ylärivit: 0 .. how-1 }
  for y := 0 to how-1 do
    for x := 0 to 319 do
      dst^[y*320+x] := 0;

  { Nollaa alarivit: 200-how .. 199 }
  for y := 200-how to 199 do
    for x := 0 to 319 do
      dst^[y*320+x] := 0;
end;

(*
procedure smooth320_orkkis(src, dst: PByteArray);
var
  a,d:word;
begin
  for a:=0 to 63999 do 
   begin
   d:=0;
   if (a>=320) then d:=d+src^[a-320];
   if (a>0) then d:=d+src^[a-1];
   if (a<63999) then d:=d+src^[a+1];
   if (a<63680) then d:=d+src^[a+320];
   dst^[a]:=d shr 2;
  end;
end;

procedure smooth256_orkkis(src, dst: PByteArray);
var
  a,d:word;
begin
  for a:=0 to 65535 do 
   begin
   d:=0;
   if (a>=256) then d:=d+src^[a-256];
   if (a>0) then d:=d+src^[a-1];
   if (a<65535) then d:=d+src^[a+1];
   if (a<65280) then d:=d+src^[a+256];
   dst^[a]:=d shr 2;
  end;
end;

procedure delupdown_orkkis(dst: PByteArray; how: word);
var
  i: word;
begin
  { Nollaa alku: 0 .. how-1 }
  for i := 0 to how-1 do
    dst^[i] := 0;

  { Nollaa loppu: 64000-how .. 63999 }
  for i := 64000 - how to 63999 do
    dst^[i] := 0;
end;
*)

procedure flipnoBlack(src, dst: PByteArray);
var
  a: word;
begin
  for a := 0 to 63999 do
    if src^[a] <> 0 then
      dst^[a] := src^[a];
end;

procedure flipnoColor(src, dst: PByteArray; vari:byte);
var
  a: word;
begin
  for a := 0 to 63999 do
    if src^[a] <> vari then
      dst^[a] := src^[a];
end;

procedure overlay(src, dst: PByteArray);
var
  a: word;
begin
  for a := 0 to 63999 do
    if dst^[a] = 0 then
      dst^[a] := src^[a];
end;

procedure transship(src, dst: PByteArray);
var
  a: word;
begin
  for a := 0 to 63999 do
    dst^[a] := (src^[a] + dst^[a]) shr 1; { div 2 }
end;

procedure cls(msg: PByteArray);
begin
  FillChar(msg^, 63999, 0);
end;

{ --- PCX-lataus --- }
procedure pcx(fln: string; sg: PByteArray; kuva_x, kuva_y: Integer);
var
  f: file of Byte;
  data, pcount: Byte;
  readoff, cox, co: Integer;
begin
  Assign(f, fln);
  Reset(f);
  Seek(f, 128);
  
  readoff := 0;
  for co := 0 to (kuva_y - 1) do
  begin
    cox := 0;
    while cox < kuva_x do
    begin
      Read(f, data);
      if (data and $C0) = $C0 then
      begin
        pcount := data and $3F;
        Read(f, data);
        FillChar(sg^[readoff], pcount, data);
        Inc(readoff, pcount);
        Inc(cox, pcount);
      end
      else
      begin
        sg^[readoff] := data;
        Inc(readoff);
        Inc(cox);
      end;
    end;
  end;
  
  Seek(f, FileSize(f) - 768);
  for co := 0 to 767 do
  begin
    Read(f, data);
    pal1[co] := data;
  end;
  Close(f);
end;

{ --- Paletti jutut --- }
procedure pal(c, r, g, b: Byte);
begin
  palette[c] := (r shl 16) or (g shl 8) or b or $FF000000;
end;

procedure setpal;
var
  i: Integer;
begin
  for i := 0 to 255 do
    pal(i, pal1[i*3], pal1[i*3+1], pal1[i*3+2]);
end;

procedure setpal_2;
var
  i: integer;
begin
  for i := 0 to 255 do
    palette[i] := (pal1[i*3] shl 18) or     { R: 0-63 -> 0-255 }
                  (pal1[i*3+1] shl 10) or   { G }
                  (pal1[i*3+2] shl 2) or    { B }
                  $FF000000;                { Alpha = 255 }
end;

{ Fade to black }
procedure ftb(dl0: integer);
var
  pal2: array[0..767] of Byte;
begin
  Move(pal1, pal2, 768);
  for a := 0 to 63 do
  begin
    for b := 0 to 767 do
      if pal2[b] > 0 then Dec(pal2[b]);
    for b := 0 to 255 do
      pal(b, pal2[b*3], pal2[b*3+1], pal2[b*3+2]);
    SDL_Delay(dl0);
  end;
end;

{ Fade from black }
procedure ffb(dl0: integer);
var
  pal2: array[0..767] of Byte;
begin
  FillChar(pal2, 768, 0);
  for a := 0 to 63 do
  begin
    for b := 0 to 767 do
      if pal2[b] < pal1[b] then Inc(pal2[b]);
    for b := 0 to 255 do
      pal(b, pal2[b*3], pal2[b*3+1], pal2[b*3+2]);
    SDL_Delay(dl0);
  end;
end;

procedure RotoZoom(x, y, scale: Integer; rot: Byte; src, dst: PByteArray);
var
  ddx, ddy, d2x, d2y: Integer;
  curr_i, curr_j: LongInt; { 16.16 kiintopiste }
  line_i, line_j: LongInt;
  ix, iy, px, py: Integer;
  sintable, costable: array[0..255] of Integer;
  sin2table, cos2table: array[0..255] of Integer;
begin
  { Lasketaan askeleet x- ja y-suunnissa (skaalaus ja rotaatio) }
  ddx := (costable[rot] * scale) div 256;
  ddy := (sintable[rot] * scale) div 256;
  d2x := (cos2table[rot] * scale) div 256; { Huom: yleensä cos(rot+90) }
  d2y := (sin2table[rot] * scale) div 256;

  { Aloituspiste (vasen yläkulma) }
  line_i := LongInt(x shl 8) - (ddx * 160) - (d2x * 100);
  line_j := LongInt(y shl 8) - (ddy * 160) - (d2y * 100);

  for iy := 0 to 199 do
  begin
    curr_i := line_i;
    curr_j := line_j;
    for ix := 0 to 319 do
    begin
      { Poimitaan pikseli sourcesta (olettaen 256x256 tekstuuri) }
      px := (curr_i shr 8) and 255;
      py := (curr_j shr 8) and 255;
      
      dst^[iy * 320 + ix] := src^[py shl 8 + px];

      { Siirrytään seuraavaan pikseliin rivillä }
      inc(curr_i, ddx);
      inc(curr_j, ddy);
    end;
    { Siirrytään seuraavalle riville }
    inc(line_i, d2x);
    inc(line_j, d2y);
  end;
end;

procedure putpixeli(dst: PByteArray; x, y: integer; c: byte);
begin

    {if (x >= 0) and (x < 320) and (y >= 0) and (y < 200) then}
      dst^[(y * 320) + x] := c;
end;


procedure circle(seg: PByteArray; x_keski, y_keski, r: integer; vari: byte);
var
  x, y, d: integer;

  { Sisäinen peocedure käyttää nyt tuota 'seg' parametria }
  procedure putpixel_vs(x, y: integer; c: byte);
  begin
    if (x >= 0) and (x < 320) and (y >= 0) and (y < 200) then
      seg^[(y * 320) + x] := c;
  end;

begin
  x := 0;
  y := r;
  d := 3 - 2 * r;

  while x <= y do
  begin
    putpixel_vs(x_keski + x, y_keski + y, vari);
    putpixel_vs(x_keski - x, y_keski + y, vari);
    putpixel_vs(x_keski + x, y_keski - y, vari);
    putpixel_vs(x_keski - x, y_keski - y, vari);
    putpixel_vs(x_keski + y, y_keski + x, vari);
    putpixel_vs(x_keski - y, y_keski + x, vari);
    putpixel_vs(x_keski + y, y_keski - x, vari);
    putpixel_vs(x_keski - y, y_keski - x, vari);

    if d < 0 then
      d := d + 4 * x + 6
    else
    begin
      d := d + 4 * (x - y) + 10;
      dec(y);
    end;
    inc(x);
  end;
end;

procedure Line(x1,y1,x2,y2:word; sg: PByteArray; col:byte);
var
  dx,dy:integer;
  sx,sy:integer;
  err,e2:integer;
  {offset:longint;}
begin
  dx := abs(x2-x1);
  dy := abs(y2-y1);

  if x1 < x2 then sx := 1 else sx := -1;
  if y1 < y2 then sy := 1 else sy := -1;

  err := dx - dy;

  while true do
  begin
    sg^[y1*320 + x1] := col;

    if (x1=x2) and (y1=y2) then exit;

    e2 := err shl 1;

    if e2 > -dy then
    begin
      err := err - dy;
      x1 := x1 + sx;
    end;

    if e2 < dx then
    begin
      err := err + dx;
      y1 := y1 + sy;
    end;
  end;
end;

{ ----------------------------------------- }

{ --- SDL Flipit --- }
procedure flip_with_palette(src: PByteArray);
var
  pixels: Pointer;
  pitch, i: Integer;
  p32: PCardinal;
begin
  if SDL_LockTexture(texture, nil, @pixels, @pitch) = 0 then
  begin
    p32 := pixels;
    for i := 0 to SCREEN_SIZE - 1 do
      p32[i] := palette[src^[i]];
    SDL_UnlockTexture(texture);
  end;
  SDL_RenderCopy(renderer, texture, nil, nil);
  SDL_RenderPresent(renderer);
end;

procedure flip(src: PByteArray);
var
  pixels: Pointer;
  pitch, i: Integer;
  p32: PCardinal;
begin
  if SDL_LockTexture(texture, nil, @pixels, @pitch) = 0 then
  begin
    p32 := pixels;
    for i := 0 to SCREEN_SIZE - 1 do
      p32[i] := (src^[i] shl 16) or (src^[i] shl 8) or src^[i] or $FF000000;
    SDL_UnlockTexture(texture);
  end;
  SDL_RenderClear(renderer);
  SDL_RenderCopy(renderer, texture, nil, nil);
  SDL_RenderPresent(renderer);
end;

{ ----------------------------------------------- }

{ --- Pixelöinti / Down sampling --- }
procedure pixel_flip(src: PByteArray; block_size: integer);
var
  x, y, bx, by: integer;
  pixel: byte;
  p32: PCardinal;
  pixels: Pointer;
  pitch: integer;
begin
  if SDL_LockTexture(texture, nil, @pixels, @pitch) = 0 then
  begin
    p32 := pixels;
    { Käydään läpi ruutu block_size välein }
    for y := 0 to (199 div block_size) do
      for x := 0 to (319 div block_size) do
      begin
        { Poimitaan yksi näyte pikseli blokin alusta }
        pixel := src^[(y * block_size * 320) + (x * block_size)];
        
        { Täytetään block_size x block_size alue samalla värillä }
        for by := 0 to block_size - 1 do
          for bx := 0 to block_size - 1 do
            p32[((y * block_size + by) * 320) + (x * block_size + bx)] := palette[pixel];
      end;
    SDL_UnlockTexture(texture);
  end;
  SDL_RenderCopy(renderer, texture, nil, nil);
  SDL_RenderPresent(renderer);
end;

{ ----------------------------------------------- }

procedure retrace; 
{ Jos käytät tätä jostakin syystä niin aktivoi set_320x200 ja set_640x400 procedureissa tämä: } {renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_SOFTWARE);} 
begin
  SDL_Delay(16);
end;

{ --- 320x200c256 initialisointi --- }

procedure Set_320x200c256;
begin
  if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO) <> 0 then Halt(1);
  
   {   SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '1'); --> Alla on ohjeet. }
   { '0' tai 'nearest' = Terävät "palikkapikselit" (kuten DOSBox default) }
   { '1' tai 'linear'  = Pehmeä skaalaus (muistuttaa enemmän CRT-monitoria) }
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '0'); 

  SDL_ShowCursor(SDL_DISABLE); { Piilottaa hiiren ruudulta. }
  
  window := SDL_CreateWindow('ISO Retro Demo',
    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    SCREEN_W_320, SCREEN_H_200,
    SDL_WINDOW_SHOWN or SDL_WINDOW_FULLSCREEN_DESKTOP);
  
  {renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_SOFTWARE);}
  renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC); { Korvaa pääkoodin retrace:n }
  SDL_RenderSetLogicalSize(renderer, SCREEN_W_320, SCREEN_H_200);
  
  texture := SDL_CreateTexture(renderer,
    SDL_PIXELFORMAT_ARGB8888,
    SDL_TEXTUREACCESS_STREAMING,
    SCREEN_W_320, SCREEN_H_200);

end;

{ --- 640x400c256 initialisointi --- }

procedure Set_640x400c256;
begin
  if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO) <> 0 then Halt(1);
  
   {   SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '1'); --> Alla on ohjeet. }
   { '0' tai 'nearest' = Terävät "palikkapikselit" (kuten DOSBox default) }
   { '1' tai 'linear'  = Pehmeä skaalaus (muistuttaa enemmän CRT-monitoria) }
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '0'); 

  SDL_ShowCursor(SDL_DISABLE); { Piilottaa hiiren ruudulta. }
  
  window := SDL_CreateWindow('ISO Retro Demo',
    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    SCREEN_W_640, SCREEN_H_400,
    SDL_WINDOW_SHOWN or SDL_WINDOW_FULLSCREEN_DESKTOP);
  
 { renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_SOFTWARE);}
  renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC); { Korvaa pääkoodin retrace:n }
  SDL_RenderSetLogicalSize(renderer, SCREEN_W_640, SCREEN_H_400);
  
  texture := SDL_CreateTexture(renderer,
    SDL_PIXELFORMAT_ARGB8888,
    SDL_TEXTUREACCESS_STREAMING,
    SCREEN_W_640, SCREEN_H_400);

end;

{ --- Textmode 80x50 jutut --- }

procedure Flip_MCGA_to_ASCII(src_seg: PByteArray; dst_seg:_256k_ByteArray; valitse:boolean);
const
  { ASCII-ramppi tummasta vaaleaan, 16 astetta }
  ASCII_RAMP : array[0..15] of byte = (
    32,   { 'tyhjä' }
    46,   { '.' }
    44,   { ',' }
    45,   { '-' }
    126,  { '~' }
    58,   { ':' }
    59,   { ';' }
    61,   { '=' }
    33,   { '!' }
    63,   { '?' }
    111,  { 'o' }
    120,  { 'x' }
    37,   { '%' }
    64,   { '@' }
    77,   { 'M' }
    87    { 'W' }
  );

  KAANNETTY_ASCII_RAMP : array[0..15] of byte = (
  { ASCII-ramppi vaaleasta tummaan, 16 astetta }
    87,   { 'W' }
    77,   { 'M' }
    64,   { '@' }
    37,   { '%' }
    120,  { 'x' }
    111,  { 'o' }
    63,   { '?' }
    33,   { '!' }
    61,   { '=' }
    59,   { ';' }
    58,   { ':' }
    126,  { '~' }
    45,   { '-' }
    44,   { ',' }
    46,   { '.' }
    32   { 'tyhjä' }
);
(*
  LESBO_RAMPPI : array[0..3] of byte = (
  { Testing.... }
    176,177,178,219
);
*)
var
  x, y,i,j   : integer;
  px, py : integer;
  sum    : word;
  avg    : byte;
  idx    : byte;
  src_ofs: word;
  dst_ofs: word;
  valinta:byte;
  pixel, character: byte;
  font_line: byte;
  
begin

  for y := 0 to 49 do
  begin
    for x := 0 to 79 do
    begin
      { Lasketaan 4x4 lohkon kirkkausarvojen keskiarvo }
    sum := 0;

      for py := 0 to 3 do
        for px := 0 to 3 do
        begin
  
          src_ofs := ((y shl 2 + py) * 320) + (x shl 2 + px); { optimoitu }          

          sum := sum + (src_seg^[src_ofs] shr 4);
        end;

       idx:=sum shr 4;

      { Kirjoitetaan tekstimuistiin: merkki + attribuutti }
      {dst_ofs := (y * 80 + x) * 2;}
      { dst_ofs := (y shl 6 + y shl 4 + x) shl 1; }
      
      {dst_ofs := (y shl 7) + (y shl 5) + (x shl 1);}

 if valitse then

      valinta := ASCII_RAMP[idx] { TRUE } { Huom! Puolipiste puuttuu! }
  else

      valinta := KAANNETTY_ASCII_RAMP[idx]; { FALSE }

{ ----- }

      { Piirretään 8x8 merkki 640x400 puskuriin }

      for i := 0 to 7 do
      begin
        font_line := VGA_FONT_8x8[valinta, i];
        for j := 0 to 7 do
        begin
          if (font_line and ($80 shr j)) <> 0 then
            dst_seg^[(y * 8 + i) * 640 + (x * 8 + j)] := src_seg^[src_ofs]
          else
            dst_seg^[(y * 8 + i) * 640 + (x * 8 + j)] := 0; { Musta tausta }
        end;
      end;  
    end;
  end;
end;


procedure txt_cls(msg: PByteArray);
begin
  FillChar(msg^, 256000, 0);
end;

procedure txt_flip_80x50(src: PByteArray; dst: _256k_ByteArray; use_dither: boolean);
var
  x, y, i, j: integer;
  pixel, character: byte;
  font_line: byte;
  target_ofs: integer;
begin
  for y := 0 to 49 do
  begin
    for x := 0 to 79 do
    begin
      { Poimitaan näyte 320x200 puskurista }
      pixel := src^[(y * 4 * 320) + (x * 4)];

      character := 219; { Oletus: täysblokki }

      { Vanha kunnon dithering-logiikkasi }
      if use_dither then
      begin
        case (pixel mod 16) of
          0..3:   character := 176;
          4..7:   character := 177;
          8..11:  character := 178;
          12..15: character := 219;
        end;
      end;

      { Piirretään 8x8 merkki 640x400 puskuriin }
      for i := 0 to 7 do
      begin
        font_line := VGA_FONT_8x8[character, i];
        for j := 0 to 7 do
        begin
          if (font_line and ($80 shr j)) <> 0 then
            dst^[(y * 8 + i) * 640 + (x * 8 + j)] := pixel
          else
            dst^[(y * 8 + i) * 640 + (x * 8 + j)] := 0; { Musta tausta }
        end;
      end;
    end;
  end;
end;

{ --- tätä flippiä käytetään ainoastaa textmodessa ja tuon txt_flip_80x50 kanssa yhtäaikaa --- }
procedure flip_txt(src: _256k_ByteArray);
var
  pixels: Pointer;
  pitch, i: Integer;
  p32: PCardinal;
begin
  if SDL_LockTexture(texture, nil, @pixels, @pitch) = 0 then
  begin
    p32 := pixels;
    { Huom! Käydään läpi 640 * 400 = 256 000 pikseliä }
    for i := 0 to 255999 do
      p32[i] := palette[src^[i]];
    SDL_UnlockTexture(texture);
  end;
  SDL_RenderClear(renderer);
  SDL_RenderCopy(renderer, texture, nil, nil);
  SDL_RenderPresent(renderer);
end;

{ ----------------------------------------------- }

{ --- Yksinkertainen demo-paletti --- }
procedure Make_Luutia_Demo_Palette;
var
  i: Integer;
begin
  // Luodaan yksinkertainen gradientti-paletti
  for i := 0 to 63 do
  begin
    pal(i, 0, 0, i * 4);           // Sininen gradientti
  end;
  for i := 64 to 127 do
  begin
    pal(i, (i - 64) * 4, 0, 255);  // Sinisestä purppuraan
  end;
  for i := 128 to 191 do
  begin
    pal(i, 255, (i - 128) * 4, 255 - (i - 128) * 4); // Purppurasta punaiseen
  end;
  for i := 192 to 255 do
  begin
    pal(i, 255, (i - 192) * 4, 0); // Punaisesta keltaiseen
  end;
end;

{ --- Oujee! Biltema Motor Works = BMW --- }  
begin
  GetMem(vs1, 65536);
  FillChar(vs1^, 65536, 0);
  GetMem(vs2, 65536);
  FillChar(vs2^, 65536, 0);
  GetMem(vs3, 65536);
  FillChar(vs3^, 65536, 0);
  GetMem(vs4, 65536);
  FillChar(vs4^, 65536, 0);
  GetMem(vs5, 65536);
  FillChar(vs5^, 65536, 0);
  GetMem(vs6, 65536);
  FillChar(vs6^, 65536, 0);

{ ------------------------- }
 
  GetMem(txt_vs1, 256000);
  FillChar(txt_vs1^, 256000, 0);
  GetMem(txt_vs2, 256000);
  FillChar(txt_vs2^, 256000, 0);
  GetMem(txt_vs3, 256000);
  FillChar(txt_vs3^, 256000, 0);
  GetMem(txt_vs4, 256000);

{ ------------------------- }

 { Set_320x200c256;}  { 13h mode }
  Set_640x400c256;  {  "textmode 80x50" }
  

  {--- Esilasketut Sini/kosi taulut --- }
for a := 0 to 255 do
 begin
    sintable[a] := Round(128 + 127 * Sin(a * Pi / 128));
    costable[a] := Round(128 + 127 * Cos(a * Pi / 128));
 end;

  for urho2 := 0 to 255 do begin { vaikka textmode plasmalle }
    sini_kekkonen16[urho2] := round(7.5 + 7.5 * sin(urho2 * Pi / 128));
    { 0 - 15}
  end;

  { Esimerkki täyttämisestä }
for a := 0 to 255 do
begin
  sintable[a] := Round(sin(a * Pi / 128) * 256);
  costable[a] := Round(cos(a * Pi / 128) * 256);
  { 90 asteen vaihesiirto = pystysuuntainen vektori }
  sin2table[a] := Round(sin((a + 64) * Pi / 128) * 256);
  cos2table[a] := Round(cos((a + 64) * Pi / 128) * 256);
end;

  { --- Muza --- }
  InitMusic('amanitadream.xm');
  

{ --------------- }

  { EFEKTI 1: Oldschool tunnel efekti powered by: Filosofem82 / huano ISO 2026 }

{ --- Kummankin tunnelin data tiedostojen lataus samasta tiedostosta.  --- }
Assign(f1, 'tunneli.dat');
Reset(f1, 1);
Blockread(f1, vs3^, 64000); // Ladataan aika samalla tavalla kuten turbo pascalissa.
Blockread(f1, vs4^, 64000);
Close(f1);


(*
{ --- yksittäiaten tunnelin data tiedostojen lataus. --- }
Assign(f1, 'pallo_d.dat');
Reset(f1, 1);
Blockread(f1, vs3^, 64000); 
Close(f1);

Assign(f1, 'pallo_a.dat');
Reset(f1, 1);
Blockread(f1, vs4^, 64000);
Close(f1);
*)

  // paletti testi paskaa
  {Make_Luutia_Demo_Palette;}
(*

      for y:=0 to 255 do 
       for x:=0 to 255 do 
      begin
{       mem[vs2:(y*256+x)]:=(y xor 20) +(x xor 40);}
{       vs2^[(y*320)+x]:=(y div 20 ) + (x div 20);}
       vs2^[(y*256)+x]:=round(sin(x/3)*33)+round(cos(y/3)*33);
      end;

for paletti:=0 to 255 do begin
   pal(paletti,paletti,paletti,paletti);
end;
*)
  pcx('pallot.pcx',vs2,256,256);
  {setpal; }

for paletti:=0 to 255 do begin
   pal(paletti,paletti,paletti,paletti);
end;

repeat
     q:=16000;
     d:=round(sin(c/20)*20)+q+100;
     f:=round(sin(c/35)*128)*256;
     w:=round(sin(c/10)*23);




for y:=0 to 199 do 
  begin
  for x:=0 to 319 do 
    begin
  
  {e:=vs3^[(y*320)+x]*320+vs4^[(y*320)+x]+(c*320)+c;}
  
  tdist:=vs3^[(y*320)+x]+c;
  tangle:=vs4^[(y*320)+x]+c;
  
  e:=(tdist*256)+tangle;  
  
    vs1^[(y*320)+x]:=vs2^[e];
  end;
end;

    inc(c);
    
    { Textmode flipit. --> Molemmat pitää kutsua, että efekti toimii. }
{   txt_flip_80x50(vs1, txt_vs1, true);}  { Dithering = Boolean }
   
   Flip_MCGA_to_ASCII(vs1, txt_vs1,true);

    {txt_flip_80x50(vs1,txt_vs1,true);}
    flip_txt(txt_vs1);    
    SDL_PollEvent(@event);
    
    current_order := openmpt_module_get_current_order(openmpt_mod);
    current_row := openmpt_module_get_current_row(openmpt_mod);
 { until (current_order = 1) and (current_row >= 0);}
 until ((current_order = 1) and (current_row >= 0)) or
      (SDL_GetKeyboardState(nil)[SDL_SCANCODE_ESCAPE] <> 0);

{ --------------- }


{ --- TestiDemon paska loppui jo...höh.. --- }

  // Siivous
  CloseMusic;

{ Freemem 13h  }

  FreeMem(vs1);
  FreeMem(vs2);
  FreeMem(vs3);
  FreeMem(vs4);
  FreeMem(vs5);
  FreeMem(vs6);
  
{ Freemem txtmode 80x50 }

  FreeMem(txt_vs1);
  FreeMem(txt_vs2);
  FreeMem(txt_vs3);
  FreeMem(txt_vs4);

  { --- --- }  

  SDL_DestroyTexture(texture);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit;
  
  WriteLn('');
  WriteLn('This is real shit...');
end.


