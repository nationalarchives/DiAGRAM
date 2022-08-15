/* global utils, api, createModelTable */
(function() {
	'use strict';

	var hideNode = utils.hideNode;
	var showNode = utils.showNode;
	var createLoadingIndicator = utils.createLoadingIndicator;

	var captionText = 'Column one gives the option to select one or multiple models, each model has a grouping header row with entries for policy changes underneath. Column three contains the policy name. The remaining columns contain the properties of each policy. There is a button to view the responses used to	generate each model and button to delete the policy entry.';

	var altText = 'Bar chart showing the scores for intellectual control and renderability for the currently selected model';


	var loadingIndicator = createLoadingIndicator(document.querySelector('#loading-indicator-container'))
		.setText('Loading image');


	var imageCache = (function() {
		var cacheSize = 10; // Limit cache to 10 items so as not to hog memory
		var cacheArray = []; // Store names of cached items so we know which to delete
		var cache = {};
	
		var addToCache = function(name, value) {
			if (cache[name] !== undefined) {
				console.warn('Item with name ' + name + ' already exists in cache, ignoring');
			}
			else {
				while (cacheArray.length >= cacheSize) {
					var deleteName = cacheArray.shift(); // Get the name of item to delete from front of cache
					delete cache[deleteName]; // Actually delete it
				}
				cache[name] = value; // Add new item to cache
				cacheArray.push(name); // Add new name to end of cacheArray
			}
		};

		var inCache = function(name) {
			return cache[name] !== undefined; 
		};

		var retrieveFromCache = function(name) {
			return cache[name];
		};

		return {
			add: addToCache,
			contains: inCache,
			retrive: retrieveFromCache
		};
	})();


	var imageContainer = document.querySelector('#image-container');
	var visualisationBox = document.querySelector('#visualisation-box');
	var requests = 0;

	var createImageFromBlob = function(simplifiedModels, blob, callback) {
		var cacheName = JSON.stringify(simplifiedModels);
		var url = URL.createObjectURL(blob);
		var image = document.createElement('img');
		image.setAttribute('src', url);
		image.setAttribute('alt', altText + (simplifiedModels.length > 1 ? 's' : ''));
		imageCache.add(cacheName, image);
		image.addEventListener('load', function() {
			// https://developer.mozilla.org/en-US/docs/Web/API/URL/revokeObjectURL
			URL.revokeObjectURL(url);
			if (callback) { callback(); }
		});
	};


	var updateVisibleImage = function() {
		var selectedModels = modelTable.getSelectedModels();

		if (!selectedModels.length) {
			imageContainer.innerText = '';
			hideNode(visualisationBox);
		}
		else {
			showNode(visualisationBox);
			var simplifiedModels = selectedModels.map(function(model) {
				return {
					model_name: model.model_name,
					scenario: model.scenario,
					renderability: model.renderability,
					intellectual_control: model.intellectual_control
				};
			});

			var modelString = JSON.stringify(simplifiedModels);
			if (imageCache.contains(modelString)) {
				imageContainer.innerText = '';
				var image = imageCache.retrive(modelString);
				imageContainer.appendChild(image);
			}
			else {
				fetchImage(simplifiedModels);
			}
		}
	};


	var fetching = {};

	var fetchImage = function(simplifiedModels) {
		var modelString = JSON.stringify(simplifiedModels);
		if (!fetching[modelString]) {
			fetching[modelString] = true;
			loadingIndicator.show();
			requests++;
	
			api.getChart(simplifiedModels, function(err, blob) {
				requests--;
				if (err) {
					if (!requests) { loadingIndicator.hide(); }
					alert(err.message);
				}
				else {
					createImageFromBlob(simplifiedModels, blob,  function() {
						if (!requests) { loadingIndicator.hide(); }
						updateVisibleImage();
						delete fetching[modelString];
					});
				}
			});
		}
	};


	var modelTable = createModelTable('#model-chooser', 'multi-select')
		.setCaptionText(captionText)
		.setSelectionChangeCallback(updateVisibleImage)
		.regenerateTable();


	updateVisibleImage();
})();