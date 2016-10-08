#version 110

varying vec2 tex_st;

void main()
{
  tex_st = gl_MultiTexCoord0.st;

  mat4 mv = gl_ModelViewMatrix;

  // no rotation, just scale 
  mv[0][0] = 0.1;
  mv[0][1] = 0.0;
  mv[0][2] = 0.0;

  mv[1][0] = 0.0;
  mv[1][1] = 0.1;
  mv[1][2] = 0.0;

  mv[2][0] = 0.0;
  mv[2][1] = 0.0;
  mv[2][2] = 0.1;

  gl_Position = gl_ProjectionMatrix * mv * gl_Vertex; 
}