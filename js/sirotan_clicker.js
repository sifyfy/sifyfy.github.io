(function (console) { "use strict";
var Main = function() { };
Main.main = function() {
	Main.w = 50;
	var element = js.JQuery("#SirotanClicker img");
	element.click(function(e) {
		++Main.w;
		element.get()[0].setAttribute("width","" + Main.w + "px");
		return;
	}).mouseover(function(e1) {
		element.addClass("on");
		return;
	}).mouseleave(function(e2) {
		element.removeClass("on");
		return;
	});
};
var q = window.jQuery;
var js = js || {}
js.JQuery = q;
Main.main();
})(typeof console != "undefined" ? console : {log:function(){}});
