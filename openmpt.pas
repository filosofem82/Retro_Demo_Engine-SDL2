unit openmpt;

{$mode objfpc}{$H+}

interface

type
  openmpt_module = Pointer;
  openmpt_module_initial_ctl = record
    ctl: PChar;
    value: PChar;
  end;
  Popenmpt_module_initial_ctl = ^openmpt_module_initial_ctl;

const
  OPENMPT_MODULE_RENDER_STEREOSEPARATION_PERCENT = 1;
  OPENMPT_MODULE_RENDER_INTERPOLATIONFILTER_LENGTH = 2;
  OPENMPT_MODULE_RENDER_VOLUMERAMPING_STRENGTH = 3;

// Module creation and destruction
function openmpt_module_create_from_memory2(
  filedata: Pointer;
  filesize: NativeUInt;
  logfunc: Pointer;
  loguser: Pointer;
  errfunc: Pointer;
  erruser: Pointer;
  error: PInteger;
  error_message: PPChar;
  ctls: Popenmpt_module_initial_ctl
): openmpt_module; cdecl; external 'libopenmpt.dll';

procedure openmpt_module_destroy(
  module: openmpt_module
); cdecl; external 'libopenmpt.dll';

// Playback control
function openmpt_module_set_repeat_count(
  module: openmpt_module;
  repeat_count: Integer
): Integer; cdecl; external 'libopenmpt.dll';

function openmpt_module_get_repeat_count(
  module: openmpt_module
): Integer; cdecl; external 'libopenmpt.dll';

// Position tracking - TÄRKEIMMÄT FUNKTIOT SYNKILLE!
function openmpt_module_get_current_order(
  module: openmpt_module
): Integer; cdecl; external 'libopenmpt.dll';

function openmpt_module_get_current_row(
  module: openmpt_module
): Integer; cdecl; external 'libopenmpt.dll';

function openmpt_module_get_current_pattern(
  module: openmpt_module
): Integer; cdecl; external 'libopenmpt.dll';

// Audio rendering
function openmpt_module_read_interleaved_stereo(
  module: openmpt_module;
  samplerate: Integer;
  count: NativeUInt;
  interleaved_stereo: PSingle
): NativeUInt; cdecl; external 'libopenmpt.dll';

function openmpt_module_read_stereo(
  module: openmpt_module;
  samplerate: Integer;
  count: NativeUInt;
  left: PSingle;
  right: PSingle
): NativeUInt; cdecl; external 'libopenmpt.dll';

// Module information
function openmpt_module_get_metadata(
  module: openmpt_module;
  key: PChar
): PChar; cdecl; external 'libopenmpt.dll';

function openmpt_module_get_duration_seconds(
  module: openmpt_module
): Double; cdecl; external 'libopenmpt.dll';

// Position control
function openmpt_module_set_position_order_row(
  module: openmpt_module;
  order: Integer;
  row: Integer
): Double; cdecl; external 'libopenmpt.dll';

// Memory management
procedure openmpt_free_string(
  str: PChar
); cdecl; external 'libopenmpt.dll';

function openmpt_module_get_current_channel_vu_mono(
  module: openmpt_module;
  channel: Integer
): Single; cdecl; external 'libopenmpt.dll';

implementation

end.
