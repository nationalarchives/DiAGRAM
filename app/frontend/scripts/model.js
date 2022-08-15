/* global utils, initQuestions, createModelTable, api */
(function () {
	'use strict';

	var hideNode = utils.hideNode;
	var showNode = utils.showNode;
	var createLoadingIndicator = utils.createLoadingIndicator;
	var getModelValidationErrors  = utils.getModelValidationErrors;

	var modelBuilder = initQuestions();

	var extractSavedModels = function () {
		return JSON.parse(window.localStorage.getItem('models')) || [];
	};
	var models = extractSavedModels();

	var captionText = 'Each model has a grouping header row with entries for scenario changes underneath. Column two contains the scenario name. The remaining columns contain the properties of each scenario. There is a button to view the responses used to - generate each model and button to delete the scenario entry.';

	var modelList = createModelTable('#model-list', 'no-select')
		.setCaptionText(captionText)
		.regenerateTable();

	if (models.length) {
		showNode('#models-section-container');
	}

	var loadingIndicator = createLoadingIndicator(document.querySelector('#loading-indicator-container'))
		.setText('Calculating scores');

	var unstart = function () {
		showNode('#prequestions');
	};

	var end = function () {
		var model = extractModel();
		showNode('#models-section-container');
		hideNode('#models-section');
		loadingIndicator.show();
		api.addScores(model, function(err) {
			loadingIndicator.hide();
			if (err) {
				alert(err.message);
			}
			else {
				saveModel(model);
			}
			showNode('#postquestions');
			showNode('#models-section');
			if (models.length) { showNode('#models-section'); }
		});		
	};

	modelBuilder
		.onUnstart(unstart)
		.onEnd(end);

	document.querySelector('#model-start').addEventListener('click', function () {
		hideNode('#start-container');
		showNode('#name-container');
		hideNode('#models-section-container');
		document.querySelector('#model-name').value = '';
		document.querySelector('#questions-comments').value = '';
	});

	document.querySelector('#model-name').addEventListener('input', function () {
		document.querySelector('#questions-name').innerText = this.value;
		var validationErrors = getModelValidationErrors(this.value, models);
		var innerText = validationErrors && this.value.length ? validationErrors : '';
		document.querySelector('#valid-text-alert').innerText = innerText;
		var nextButton = document.querySelector('#name-container .btn-next');
		this.value.length && !validationErrors ? showNode(nextButton) : hideNode(nextButton);
	});

	document.querySelector('#name-container .btn-next').addEventListener('click', function () {
		hideNode('#prequestions');
		modelBuilder.start(document.querySelector('#model-name').value);
	});


	var extractModel = function () {
		/* eslint-disable camelcase */
		var model = {
			model_name: modelBuilder.name,
			scenario: 'Base Model',
			notes: modelBuilder.comments,
			is_advanced: false,
			intellectual_control: null,
			renderability: null,
			response: modelBuilder.getResponses(),
			advanced: null
		};
		/* eslint-enable camelcase */
		return model;
	};



	var saveModel = function (model) {
		var storage = window.localStorage;
		var models;

		if (storage.getItem('models')) {
			models = JSON.parse(storage.getItem('models'));
		}
		else {
			models = [];
		}

		models.push(model);
		storage.setItem('models', JSON.stringify(models));
		modelList.regenerateTable();
	};
})();