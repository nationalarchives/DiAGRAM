(function() {
	'use strict';

	var page = document.body.dataset.page;

	var navLinks = document.querySelectorAll('.sidebar li a');
	for (var i = 0; i < navLinks.length; i++) {
		var node = navLinks[i];
		if (node.dataset.page === page) {
			node.setAttribute('aria-current', 'page');
			node.parentElement.classList.add('active');
			node.tabIndex = -1; // skip the current page when tabbing through
			break;
		}
	}

	// Stop the links doing mildly irritating focus flashing when clicked
	navLinks.forEach(function(link) {
		link.addEventListener('mousedown', function(evt) {
			evt.preventDefault();
		});
	});

	document.querySelector('#sidebar-toggle').addEventListener('click', function(evt) {
		evt.preventDefault();
		var content = document.querySelector('.content');
		if (this.getAttribute('aria-expanded') === 'true') {
			this.setAttribute('aria-expanded', false);
			content.classList.add('sidebar-hidden');
		}
		else {
			this.setAttribute('aria-expanded', true);
			content.classList.remove('sidebar-hidden');
		}
	});
})();