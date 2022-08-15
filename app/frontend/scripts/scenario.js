/* global utils, initQuestions, api, createModelTable */
(function () {
	'use strict';

	var hideNode = utils.hideNode;
	var showNode = utils.showNode;
	var createLoadingIndicator = utils.createLoadingIndicator;
	var getScenarioValidationErrors  = utils.getScenarioValidationErrors;

	var storage = window.localStorage;
	var model, models;

	var extractSavedModels = function () {
		return JSON.parse(window.localStorage.getItem('models')) || [];
	};

	models = extractSavedModels();

	// If there are no saved models then there's nothing to do
	if (!models.length) { return; }

	var deepCopyObject = function(obj) {
		return JSON.parse(JSON.stringify(obj));
	};

	hideNode('#no-models');
	showNode('#prequestions');

	var setButtonStatus = function() {
		var nextButton = document.querySelector('#choose-model-container .btn-next');
		if (modelChooser.getSelectedModels().length) {
			nextButton.removeAttribute('disabled');
		}
		else {
			nextButton.setAttribute('disabled', true);
		}
	};

	var scenarioBuilder = initQuestions();

	var captionText= 'Choose a model first before you create a new scenario. Scenarios cannot be made from advanced models in this section.';

	var modelChooser = createModelTable('#model-chooser', 'single-select')
		.setCaptionText(captionText)
		.setUnselectableRadioFunction(function(model) { return model.is_advanced; })
		.setSelectionChangeCallback(setButtonStatus)
		.setDeleteCallback(setButtonStatus)
		.regenerateTable();

	var loadingIndicator = createLoadingIndicator(document.querySelector('#loading-indicator-container'))
		.setText('Calculating scores');

	var unstart = function () {
		showNode('#prequestions');
	};

	var end = function () {
		var scenario = deepCopyObject(model);
		scenario.scenario = scenarioBuilder.name;
		scenario.response = scenarioBuilder.getResponses();
		loadingIndicator.show();
		api.addScores(scenario, function(err) {
			loadingIndicator.hide();
			if (err) { alert(err.message); }
			else {
				models.push(scenario);
				storage.setItem('models', JSON.stringify(models));
			}
			showNode('#postquestions');
		});
	};

	var spreadEntity = function (entity, output) {
		output = output || [];
		if (typeof entity === 'string' || typeof entity === 'number') {
			output.push(entity);
		}
		else {
			Object.keys(entity).forEach(function (key) {
				spreadEntity(entity[key], output);
			});
		}
		return output;
	};

	scenarioBuilder
		.onUnstart(unstart)
		.onEnd(end);
	
	document.querySelector('#choose-model-container .btn-next').addEventListener('click', function () {
		model = modelChooser.getSelectedModels()[0];
		hideNode('#choose-model-container');
		showNode('#response-changes');
		var response = model.response;
		scenarioBuilder.setAnswers(response);

		Object.keys(response).forEach(function (key) {
			var input = document.querySelector('#' + key);
			var answer = input.parentElement.parentElement.querySelector('.answers');
			answer.innerText = spreadEntity(model.response[key]).join(', ');
		});
	});

	document.querySelector('#response-changes .btn-previous').addEventListener('click', function () {
		showNode('#choose-model-container');
		hideNode('#response-changes');
	});


	var checkboxes = document.querySelectorAll('#response-changes tbody input');
	var checkboxToggle = document.querySelector('#response-changes thead input');
	var scenarioButton = document.querySelector('#response-changes .btn-finish');

	checkboxes.forEach(function (checkbox) {
		checkbox.addEventListener('change', function () {
			var boxesChecked = 0;
			for (var i = 0; i < checkboxes.length; i++) {
				if (checkboxes[i].checked) { boxesChecked++; }
			}

			if (boxesChecked) {
				showNode(scenarioButton);
			}
			else {
				hideNode(scenarioButton);
			}
			if (boxesChecked === checkboxes.length) {
				checkboxToggle.checked = true;
			}
			else {
				checkboxToggle.checked = false;
			}
		});
	});


	checkboxToggle.addEventListener('change', function () {
		var value = checkboxToggle.checked;
		checkboxes.forEach(function (checkbox) {
			checkbox.checked = value;
		});
		if (value) {
			showNode(scenarioButton);
		}
		else {
			hideNode(scenarioButton);
		}
	});

	scenarioButton.addEventListener('click', function () {
		hideNode('#response-changes');
		showNode('#name-container');
	});

	document.querySelector('#name-container .btn-previous').addEventListener('click', function () {
		showNode('#response-changes');
		hideNode('#name-container');
	});

	document.querySelector('#name-container .btn-next').addEventListener('click', function () {
		hideNode('#prequestions');
		var skip = [];
		checkboxes.forEach(function (checkbox) {
			if (!checkbox.checked) {
				skip.push(checkbox.getAttribute('id'));
			}
		});
		scenarioBuilder.start(document.querySelector('#scenario-name').value, skip);
	});

	document.querySelector('#scenario-name').addEventListener('input', function () {
		var validationErrors = getScenarioValidationErrors(model, this.value, models);
		var innerText = validationErrors && this.value.length ? validationErrors : '';
		document.querySelector('#valid-text-alert').innerText = innerText;
		var nextButton = document.querySelector('#name-container .btn-next');
		this.value.length && !validationErrors ? showNode(nextButton) : hideNode(nextButton);
	});

	setButtonStatus();
})();