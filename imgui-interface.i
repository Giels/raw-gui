%module imgui
%feature("inter_function", "1");
%feature("flatnested", "1");

%include "imgui/imgui.h"

%include "imgui/imgui_internal.h"
%include "imgui/examples/imgui_impl_sdl.h"
%include "imgui/examples/imgui_impl_opengl3.h"


