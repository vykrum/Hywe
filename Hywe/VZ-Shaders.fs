module Shaders

let vsSource = """
    attribute vec3 a_position;
    attribute vec3 a_color;
    uniform mat4 u_projection;
    uniform mat4 u_view;
    varying vec3 v_color;
    void main() {
        gl_Position = u_projection * u_view * vec4(a_position, 1.0);
        v_color = a_color;
    }"""

let fsSource = """
    precision mediump float;
    varying vec3 v_color;
    uniform vec3 u_overrideColor;
    void main() {
        float alpha = 1.0;
        if (u_overrideColor.r < 0.0)
            gl_FragColor = vec4(v_color, alpha);
        else
            gl_FragColor = vec4(u_overrideColor, 1.0);
    }"""
