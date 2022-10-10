(function() {
	'use strict';

	var storage = window.localStorage;
	var preservationDuration = 24 * 60 * 60 * 1000; // 1 day in milliseconds

	var expiryTimestamp = storage.getItem('expiryTimestamp');

	if (expiryTimestamp && Date.now() > expiryTimestamp) {
		storage.removeItem('models');
	}

	// Update now just in case beforeunload event never fires for some reason 
	storage.setItem('expiryTimestamp', Date.now() + preservationDuration);

	window.addEventListener('beforeunload', function() {
		// Data will persist for preservationTime after they've left the website
		storage.setItem('expiryTimestamp', Date.now() + preservationDuration);
	});
})();