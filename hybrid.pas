program retro;

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
{  TByteArray = array[0..SCREEN_SIZE-1] of Byte;}
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
  pal1: array[0..767] of Byte;
  f1:file;
  paletti: Byte;
  x,y,z: word;
  a,b,c,d,e,f:word;
  q,w:word; 
  tdist,tangle:byte;
  
  
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
  
  // libopenmpt renderöi suoraan int16-muodossa!
  samples_rendered := openmpt_module_read_interleaved_stereo(
    openmpt_mod, 
    AUDIO_FREQ, 
    samples_needed, 
    PSingle(buffer_int)
  );
  
  // Täytä loput nollilla jos tarvitaan --> jaa a....
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
    WriteLn('VIRHE: Musiikkitiedostoa ei löydy: ', filename);
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
      WriteLn('VIRHE: Moduulin lataus epäonnistui! Error: ', error);
      Exit;
    end;
    
    WriteLn('Moduuli ladattu: ', filename);
    WriteLn('Kesto: ', openmpt_module_get_duration_seconds(openmpt_mod):0:2, ' sek');
    
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
    WriteLn('VIRHE: SDL Audio ei aukea: ', SDL_GetError());
    Exit;
  end;
  
  // Aloita toisto
  SDL_PauseAudio(0);
  
  WriteLn('Musiikki käynnistetty!');
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
    SDL_Delay(1);  // Pieni viive ettei CPU palaa
  until ((current_order) = duppos) and ((current_row) >= durpos);
end;
{ --- Musa jutut loppuvat --- }

procedure smooth320(src, dst: PByteArray);
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


procedure delupdown(dst: PByteArray; how: word);
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
begin
  SDL_Delay(16);
end;

{ --- 320x200c256 initialisointi --- }

procedure Set_320x200c256;
begin
  if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO) <> 0 then Halt(1);
  
  window := SDL_CreateWindow('Retro Demo with Music',
    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    SCREEN_W_320, SCREEN_H_200,
    SDL_WINDOW_SHOWN or SDL_WINDOW_FULLSCREEN_DESKTOP);
  
  renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_SOFTWARE);
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
  
  window := SDL_CreateWindow('Retro Demo with Music',
    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    SCREEN_W_640, SCREEN_H_400,
    SDL_WINDOW_SHOWN or SDL_WINDOW_FULLSCREEN_DESKTOP);
  
  renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_SOFTWARE);
  SDL_RenderSetLogicalSize(renderer, SCREEN_W_640, SCREEN_H_400);
  
  texture := SDL_CreateTexture(renderer,
    SDL_PIXELFORMAT_ARGB8888,
    SDL_TEXTUREACCESS_STREAMING,
    SCREEN_W_640, SCREEN_H_400);

end;

{ --- Textmode 80x50 jutut --- }

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


  Set_320x200c256;  { 13h mode }

{  Set_640x400c256; } {  "textmode 80x50" }
  
  InitMusic('music.xm'); { ----> Darn }  
  


  { EFEKTI 1 - Oldschool tunnel efekti powered by: Filosoem82 / huano ISO 2026 }

{ tunnelin data tiedostojen lataus.}
Assign(f1, 'tunneli.dat');
Reset(f1, 1);
Blockread(f1, vs3^, 64000); // Ladataan aika samalla tavalla kuten turbo pascalissa.
Blockread(f1, vs4^, 64000);
Close(f1);


(*
{ tunnelin data tiedostojen lataus.}
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


      for y:=0 to 255 do 
       for x:=0 to 255 do 
      begin
{       mem[vs2:(y*256+x)]:=(y xor 20) +(x xor 40);}
{       vs2^[(y*320)+x]:=(y div 20 ) + (x div 20);}
       vs2^[(y*256)+x]:=round(sin(x/3)*33)+round(cos(y/3)*33);
      end;

{  pcx('riku.pcx',vs2,256,256);
  setpal; }

repeat
     q:=16000;
     d:=round(sin(c/20)*20)+q+100;
     f:=round(sin(c/35)*128)*256;
     w:=round(sin(c/10)*23);

for paletti:=0 to 255 do begin
   pal(paletti,paletti,paletti+w,paletti);
end;



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
    retrace;
    {flip(vs1);}  { 13h flip ei palettia --> harmaa }
    smooth320(vs1,vs1);
    flip_with_palette(vs1);  { 13h flip with palette }
    
    { Textmode flipit. --> Molemmat pitää kutsua, että efekti toimii. }

   { ------------------------ } 
{    txt_flip_80x50(vs1, txt_vs1, true); { Dithering = Boolean }
    flip_txt(txt_vs1);}
   { ------------------------ } 
    
    SDL_PollEvent(@event);
    
    current_order := openmpt_module_get_current_order(openmpt_mod);
    current_row := openmpt_module_get_current_row(openmpt_mod);
 { until (current_order = 1) and (current_row >= 0);}
 until ((current_order = 1) and (current_row >= 0)) or
      (SDL_GetKeyboardState(nil)[SDL_SCANCODE_ESCAPE] <> 0);

{ ------------------------------------------------------------------------- }


  { EFEKTI 2 - Ruudukko }
  repeat
    for y := 0 to 199 do
      for x := 0 to 319 do
        vs1^[(y * 320) + x] := (y div 20) + (x div 20) + c;
    
    inc(c);
    flip(vs1);
    SDL_PollEvent(@event);  // Pakollinen SDL event handling
    
    current_order := openmpt_module_get_current_order(openmpt_mod);
    current_row := openmpt_module_get_current_row(openmpt_mod);
{  until (current_order = 2) and (current_row >= 0);}
until ((current_order = 2) and (current_row >= 0)) or
      (SDL_GetKeyboardState(nil)[SDL_SCANCODE_ESCAPE] <> 0);


{ ------------------------------------------------------------------------- }

  // Luodaan demo-paletti
  MakeDemoPalette;
  
  { EFEKTI 3 - Jotain muuta }
  repeat
    for y := 0 to 199 do
      for x := 0 to 319 do
        vs1^[(y * 320) + x] := c + x + y;
    
    inc(c);
    {flip(vs1);}
    flip_with_palette(vs1);
    SDL_PollEvent(@event);
    
    current_order := openmpt_module_get_current_order(openmpt_mod);
    current_row := openmpt_module_get_current_row(openmpt_mod);
{  until (current_order = 3) and (current_row >= 0);}
until ((current_order = 3) and (current_row >= 0)) or
      (SDL_GetKeyboardState(nil)[SDL_SCANCODE_ESCAPE] <> 0);


{ ------------------------------------------------------------------------- }


{ EFEKTI 4 JNE... }


{ ------------------------------------------------------------------------- }

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


