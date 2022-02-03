
var highlight = function (on) {
	return function () {
		var links = document.getElementsByTagName('a');
		for (var i = 0; i < links.length; i++) {
			var that = links[i];

			if (this.href != that.href) {
				continue;
			}

			if (on) {
				that.classList.add("hover-highlight");
			} else {
				that.classList.remove("hover-highlight");
			}
		}
	}
};

window.onload = function () {
	var links = document.getElementsByTagName('a');
	for (var i = 0; i < links.length; i++) {
		links[i].onmouseover = highlight(true);
		links[i].onmouseout = highlight(false);
	}
};
