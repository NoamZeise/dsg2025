#version 330

in vec3 fpos;
in vec3 fnorm;
in vec2 fuv;

layout(location = 0) out vec4 colour;

uniform vec4 obj_colour;

void main() {
  vec3 normal = normalize(fnorm);
  vec3 light_dir = normalize(vec3(-0.3, 0.3, -0.3));
  vec4 c_obj = obj_colour;
  
  colour = c_obj * clamp(dot(normal, light_dir), 0.4, 1.0);  
}
