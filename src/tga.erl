-module(tga).
-export([write/4]).

%% TGA (Truevision Graphics Adapter) file writer
%% Outputs uncompressed 32-bit BGRA images
%% TGA format chosen because it requires no external libraries

%% Write a TGA file with the given width, height, and pixel data
%% PixelData should be a list of {R, G, B, A} tuples (0-255 range)
write(Filename, Width, Height, PixelData) ->
    Header = build_header(Width, Height),
    ImageData = build_image_data(PixelData),
    Footer = build_footer(),

    Data = <<Header/binary, ImageData/binary, Footer/binary>>,
    file:write_file(Filename, Data).

%% Build TGA header for 32-bit uncompressed image
%% TGA header is 18 bytes
build_header(Width, Height) ->
    IDLength = 0,           % No image ID field
    ColorMapType = 0,       % No color map
    ImageType = 2,          % Uncompressed true-color image
    ColorMapSpec = <<0:40>>, % 5 bytes of zeros (no color map)
    XOrigin = 0,
    YOrigin = 0,
    BitsPerPixel = 32,      % 32-bit BGRA
    ImageDescriptor = 8,    % 8-bit alpha, origin at top-left

    <<IDLength:8,
      ColorMapType:8,
      ImageType:8,
      ColorMapSpec/binary,
      XOrigin:16/little,
      YOrigin:16/little,
      Width:16/little,
      Height:16/little,
      BitsPerPixel:8,
      ImageDescriptor:8>>.

%% Convert pixel data from {R, G, B, A} format to BGRA binary
%% TGA stores pixels in BGRA order (not RGBA)
build_image_data(PixelData) ->
    list_to_binary([<<B:8, G:8, R:8, A:8>> || {R, G, B, A} <- PixelData]).

%% Build TGA footer (optional but recommended)
%% New TGA specification includes a 26-byte footer
build_footer() ->
    ExtensionOffset = 0,
    DeveloperOffset = 0,
    Signature = <<"TRUEVISION-XFILE.">>,
    <<ExtensionOffset:32/little,
      DeveloperOffset:32/little,
      Signature/binary,
      0:8>>.  % Null terminator
