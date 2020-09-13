# Raw-UI

A barebonas SDL2-imgui bridge/wrapper.

I couldn't get autowrap/claw to work reasonably, even with cimgui (and the only prepackaged autowrap-generated bindings seem to lack many important definitions, notably for structs), so here we are.

## Features

- SWIG 3-based bindings for Dear Imgui (not based on cimgui), hand-fixed
- SDL2 packaged libraries (but fix the loader and compile your own for other platforms)
- Bindings for the ImGui_Impl{SDL|OpenGL3} "examples" from imgui
- Small helpers to get it all up and running quick

## Requirements

- All of Dear Imgui's requirements
- OpenGL 3.2-capable hardware if using the helper functions

## Usage
This is almost exclusively for personal use, so documentation is going to be lacking. See `demo.lisp` for a basic example of how to use this.

### Preparing Artifacts

`cd build/ && sh compile.sh` will generate `libimgui.so`, a shared library that contains the Dear Imgui and related impl code. Adjustment may be needed to compile on other platforms than linux. SDL2 also needs to be compiled as per its own instructions, with libraries manually moved to the lib/ directory.
The `library.lisp` file contains code to load the correct local libraries and may need to be modified to load a different library depending on the platform.
`swig -cffi -module cimgui -generate-typedef -c++ imgui-interface.i` will generate the basic swig bindings.
NOTE: This requires swig <4.1 as common lisp support is gone from swig from that point.
NOTE: The bindings were then edited by hand. Details are listed in comments at the top of `imgui.lisp` and `imgui-interface_wrap.cxx`.

### Using

- The `imgui` package contains the raw, edited, SWIG-generated bindings
- The `raw-gui` package contains some helpers to make things easier

See `demo.lisp` for a small usage demo.

The basic usage is to call `raw-gui:run` and provide `:render-fn` as a function contianing `imgui` code, and `:event-fn` with sdl2 event handling code (the rest is handled by the `run` function -- no need to worry about setting up windowing, looping and rendering).
The `event-fn` must return `nil` when it's time to quit.
The `render-fn` can contain arbitrary opengl rendering code and should work as expected, although this hasn't been tested.

## See Also
[cimgui](https://github.com/cimgui/cimgui): Autogenerated C interface for Dear Imgui
[Dear Imgui](https://github.com/ocornut/imgui)
[autowrapped-cimgui](https://github.com/cbaggers/autowrapped-cimgui): cl-autowrap-generated bindings for Dear Imgui with packaged libs (WIP quality)
