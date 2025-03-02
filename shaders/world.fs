#version 330

in vec3 fpos;
in vec3 cpos;
in vec3 fnorm;
in vec2 fuv;

layout(location = 0) out vec4 colour;

uniform vec4 obj_colour;
uniform float time;

float noise(vec2 p, float freq );

void main() {
  vec2 offset = vec2(sin(time*0.1), cos(time*0.1));
  vec3 normal = fnorm +
    vec3(noise(fpos.xz - offset, 4000)*0.03,
	 noise(fpos.xz, 4000)*0.03,
	 noise(fpos.zx, 4000)*0.03);
  normal += vec3(noise(fpos.xz, 100)*0.05,
		 noise(fpos.xz, 100)*0.05,
		 noise(fpos.zx, 100)*0.05);

  normal = normalize(normal);
  vec3 light_dir = normalize(vec3(-0.3, 0.3, -0.3));
  vec4 c_obj = obj_colour;
  float dist = length(cpos);
  dist = clamp(dist/40, 0.0, 1.0);
  
  colour = dist * vec4(1)
    + c_obj * clamp(dot(normal, light_dir), 0.5, 0.9);
  //colour = vec4(noise(cpos.xy, 100), 0, 0, 1);
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
