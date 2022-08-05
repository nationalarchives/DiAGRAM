(function() {
	'use strict';


	/* ----- Polyfils ----- */
	if (window.NodeList && !NodeList.prototype.forEach) {
		NodeList.prototype.forEach = Array.prototype.forEach;
	}
	
	if (!Array.prototype.includes) {
		Array.prototype.includes = function(value) {
			return this.indexOf(value) !== -1;
		};
	}


	/* ----- Constants ----- */
	var modelNameKey = 'model_name';
	var scenarioNameKey = 'scenario';
	var exampleNames = ['Example - Commercial Backup', 'Example - Established National Archive'];


	/* ----- Utility funcrions ----- */
	var showNode = function(entity) {
		var node = typeof entity === 'string' ? document.querySelector(entity) : entity;
		node.classList.remove('hidden');
	};
	
	var hideNode = function(entity) {
		var node = typeof entity === 'string' ? document.querySelector(entity) : entity;
		node.classList.add('hidden');
	};


	var getModelValidationErrors = function(modelName, models) {
		var modelNameExists = function(m) {
			return m[modelNameKey] === modelName;
		};

		var err = '';

		if (modelName === '') {
			err = 'Model name empty.';
		}
		else if (models.some(modelNameExists)) {
			err = 'The name "' + modelName + '" already exists in the stored data, please choose another name.';
		}
		else if (exampleNames.includes(modelName)) {
			err = 'The name "' + modelName + '" is a reserved name, please choose another name.';
		}

		return err;
	};


	var getScenarioValidationErrors = function(model, scenarioName, models) {
		var scenarioNameExists = function(m) {
			return m[modelNameKey] === model[modelNameKey] && m[scenarioNameKey] === scenarioName;
		};

		var err = '';

		if (scenarioName === '') {
			err = 'Scenario name empty.';
		}
		else if (models.some(scenarioNameExists)) {
			err = 'The scenario name "' + scenarioName + '" already exists in the stored data, please choose another name.';
		}

		return err;
	};


	var createLoadingIndicator = function(parent) {
		var indicatorText = 'Loading';

		var indicator = document.createElement('div');
		indicator.classList.add('loading-indicator');
		var indicatorPara = document.createElement('p');
		indicatorPara.classList.add('no-margin');
		indicator.appendChild(indicatorPara);

		var messageSpan = document.createElement('span');
		messageSpan.setAttribute('role', 'status');
		messageSpan.setAttribute('aria-live', 'assertive');
		messageSpan.setAttribute('aria-atomic', true);
		indicatorPara.appendChild(messageSpan);

		var ellipsisSpan = document.createElement('span');
		ellipsisSpan.setAttribute('aria-hidden', true);
		indicatorPara.appendChild(ellipsisSpan);

		parent.appendChild(indicator);

		var out = {};

		var intervalId;
		var counter = 0;
		var ellipsisify = function() {
			counter = (counter + 1) % 4;
			ellipsisSpan.innerText = '.'.repeat ? '.'.repeat(counter) : '...';
		};

		var stopEllipsisifying = function() {
			window.clearInterval(intervalId);
			counter = 0;
		};

		out.setText = function(text) {
			indicatorText = text;
			return out;
		};

		out.show = function() {
			stopEllipsisifying();
			messageSpan.innerText = indicatorText;
			indicatorPara.classList.remove('no-margin');
			intervalId = window.setInterval(ellipsisify, 500);
			return out;
		};

		out.hide = function() {
			stopEllipsisifying();
			messageSpan.innerText = '';
			ellipsisSpan.innerText = '';
			indicatorPara.classList.add('no-margin');
		};

		return out;
	};


	/* ----- Exported utility object ----- */
	window.utils = {
		showNode: showNode,
		hideNode: hideNode,
		getModelValidationErrors: getModelValidationErrors,
		getScenarioValidationErrors: getScenarioValidationErrors,
		createLoadingIndicator: createLoadingIndicator
	};
})();
