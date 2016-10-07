#version 110

varying vec2 tex_st;

void main()
{
  tex_st = gl_MultiTexCoord0.st;

  gl_Position = gl_ProjectionMatrix * (gl_ModelViewMatrix * vec4(0.0, 0.0, 0.0, 1.0) + vec4(gl_Vertex.x, gl_Vertex.y, 0.0, 0.0)); 
}