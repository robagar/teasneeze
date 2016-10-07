#version 110

uniform sampler2D tex;
varying vec2 tex_st;

void main () {
  gl_FragColor = texture2D(tex, vec2(tex_st.x, tex_st.y));
}
