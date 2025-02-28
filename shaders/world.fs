#version 460

in vec3 fpos;
in vec3 fnorm;
in vec2 fuv;

layout(location = 0) out vec4 colour;

uniform vec4 obj_colour;
uniform int use_texture;

uniform sampler2D tex;

void main() {
  vec3 normal = normalize(fnorm);
  vec3 light_dir = normalize(vec3(-0.3, 0.3, -0.3));
  vec4 c_obj = obj_colour;
  if(use_texture == 1)
    c_obj = texture(tex, fuv);
  colour = c_obj * clamp(dot(normal, light_dir), 0.4, 1.0);  
}
