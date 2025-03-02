#version 330

layout (location = 0) in vec3 pos;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 uv;

uniform mat4 model;
uniform mat4 norm_mat;
uniform mat4 view;
uniform mat4 projection;

out vec3 fpos;
out vec3 cpos;
out vec3 fnorm;
out vec2 fuv;

float noise(vec2 p, float freq );

void main() {
  vec4 world = model * vec4(pos, 1);
  fpos = vec3(world);
  fuv = uv;
  fnorm = vec3(norm_mat * vec4(normal, 1));
  mat4 p = projection;
  p[1][1] = -p[1][1];
  vec4 cam = view * world;
  cpos = vec3(cam);
  gl_Position = p * cam;
}

// copied from https://gist.github.com/patriciogonzalezvivo/670c22f3966e662d2f83

#define PI 3.1415926

float rand(vec2 c){
	return fract(sin(dot(c.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

float noise(vec2 p, float freq ){
	float unit = 200/freq;
	vec2 ij = floor(p/unit);
	vec2 xy = mod(p,unit)/unit;
	//xy = 3.*xy*xy-2.*xy*xy*xy;
	xy = .5*(1.-cos(PI*xy));
	float a = rand((ij+vec2(0.,0.)));
	float b = rand((ij+vec2(1.,0.)));
	float c = rand((ij+vec2(0.,1.)));
	float d = rand((ij+vec2(1.,1.)));
	float x1 = mix(a, b, xy.x);
	float x2 = mix(c, d, xy.x);
	return mix(x1, x2, xy.y);
}

