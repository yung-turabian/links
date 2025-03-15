// TODO Refer to the MVU files as they give a pretty good explanation of how the relation should work

const WebGL = (function() {

		// loaded: This module should only be loaded once per page; it is an error
		// to load the page more than once. "loaded" is set to true when
		// "createContext" is called for the first time.
		let loaded = false;
		let canvas = undefined;
		var gl = undefined;

		function _createContext() { 
				canvas = document.querySelector("#gl-canvas");
				gl = canvas.getContext("webgl");

				// Only continue if WebGL is available and working
				if (gl === null) {
						alert(
						"Unable to initialize WebGL. Your browser or machine may not support it.",
						);
						return;
				}

				loaded = true;
		}

		// WebGL variables
		function _color_buffer_bit() {
				return gl.COLOR_BUFFER_BIT;
		}


		var color_buffer_bit = _$Links.kify(_color_buffer_bit);


		function _clearColor(r, g, b, a) {
				gl.clearColor(r, g, b, a);
		}

		function _clear(mask) {
				gl.clear(mask);
		}

		function _alertBox(s) {
			alert(s);
		}

		var alertBox = _$Links.kify(_alertBox);
		var createContext = _$Links.kify(_createContext);
		var clearColor = _$Links.kify(_clearColor);
		var clear = _$Links.kify(_clear);

		return {
				"color_buffer_bit": color_buffer_bit,

				"createContext": createContext,
				"clearColor": clearColor,
				"clear": clear,
				"alertBox": alertBox,
		}
})();

const color_buffer_bit = WebGL.color_buffer_bit;

const createContext = WebGL.createContext;
const clearColor = WebGL.clearColor;
const clear = WebGL.clear;

const alertBox = WebGL.alertBox;
