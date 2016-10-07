#version 110

varying vec2 tex_st;

void main()
{
  tex_st = gl_MultiTexCoord0.st;
  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}