%module imgui
%feature("inter_function", "1");
%feature("flatnested", "1");

%include "imgui/imgui.h"

%include "imgui/imgui_internal.h"
%include "imgui/examples/imgui_impl_sdl.h"
%include "imgui/examples/imgui_impl_opengl3.h"

// Somehow missing from the generated wrapper...
%{
#include "imgui/imgui.h"
#include "imgui/imgui_internal.h"
#include "SDL.h"
#include "imgui/examples/imgui_impl_opengl3.h"
#include "imgui/examples/imgui_impl_sdl.h"
%}

// TODO: Figure out how to fix the flaws in the way the bindings are generated;
// TODO: Figure out a way to exclude every xxxV function -- they're broken (except xxxRGBtoHSV and xxxUV)

