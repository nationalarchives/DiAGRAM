/* global utils, createModelTable, api */
(function() {
	'use strict';

	var hideNode = utils.hideNode;
	var showNode = utils.showNode;
	var createLoadingIndicator = utils.createLoadingIndicator;

	var storage = window.localStorage;

	// If there are no saved models then there's nothing to do
	if (!storage.getItem('models')) { return; }

	hideNode('#no-models');
	showNode('#have-models');

	var captionText = 'Column one gives the option to select one or multiple models, each model has a grouping header row with entries for policy changes underneath. Column three contains the policy name. The remaining columns contain the properties of each policy. There is a button to view the responses used to	generate each model and button to delete the policy entry.';

	var pdfButton = document.querySelector('#download-pdf');
	var noBase = document.querySelector('#no-base');
	var csvButton = document.querySelector('#download-csv');
	var noResponse = document.querySelector('#no-response');


	var missingBaseModel = function(selectedModels) {
		var models = selectedModels.filter(function(model) {
			return model.scenario === 'Base Model';
		});

		var scenarios = selectedModels.filter(function(model) {
			return model.scenario !== 'Base Model';
		});

		if (!scenarios.length) { return false; }
		if (!models.length) { return true; }

		return scenarios.some(function(scenario) {
			var modelName = scenario.model_name;
			return !models.some(function(model) {
				return model.model_name === modelName;
			});
		});
	};


	var setButtonStatuses = function() {
		var buttons = document.querySelectorAll('#download-buttons button');
		var selectedModels = modelChooser.getSelectedModels();
		var noModels = !selectedModels.length;
		var haveMissingBaseModel = missingBaseModel(selectedModels);
		var hasAdvanced = selectedModels.some(function(model) { return model.is_advanced; });

		buttons.forEach(function(button) {
			var isPdfButton = button === pdfButton;
			var isCsvButton = button === csvButton;
			var disable = noModels || (isPdfButton && haveMissingBaseModel) || (isCsvButton && hasAdvanced);

			if (disable) {
				button.setAttribute('disabled', true);
				if (isPdfButton) {
					haveMissingBaseModel ? showNode(noBase) : hideNode(noBase);
				}
				else if (isCsvButton) {
					hasAdvanced ?	showNode(noResponse) : hideNode(noResponse);
				}
			}
			else {
				button.removeAttribute('disabled');
				if (isPdfButton) { hideNode(noBase); }
				if (isCsvButton) { hideNode(noResponse); }
			}
		});
	};

	var modelChooser = createModelTable('#model-chooser', 'multi-select')
		.setCaptionText(captionText)
		.setSelectionChangeCallback(setButtonStatuses)
		.regenerateTable();

	var loadingIndicator = createLoadingIndicator(document.querySelector('#loading-indicator-container'));

	var downloadLink = document.querySelector('#download-link');

	var getTodaysDate = function() {
		return new Date().toISOString().split('T')[0];
	};

	var downloadBlob = function(blob, filename) {
		var href = URL.createObjectURL(blob);
		downloadLink.setAttribute('download', filename);
		downloadLink.setAttribute('href', href);
		downloadLink.click();
	};

	pdfButton.addEventListener('click', function() {
		var filename = 'models-' + getTodaysDate() + '.pdf';
		var callback = function(err, blob) {
			err ? alert(err.message) : downloadBlob(blob, filename);
			loadingIndicator.hide();
		};
		loadingIndicator.setText('Generating PDF file').show();
		api.getPdf(modelChooser.getSelectedModels(), callback);
	});

	document.querySelector('#download-csv').addEventListener('click', function() {
		var filename = 'models-' + getTodaysDate() + '.csv';
		var callback = function(err, blob) {
			err ? alert(err.message) : downloadBlob(blob, filename);
			loadingIndicator.hide();
		};
		loadingIndicator.setText('Generating CSV file').show();
		api.getCsv(modelChooser.getSelectedModels(), callback);
	});

	document.querySelector('#download-json').addEventListener('click', function() {
		var json = JSON.stringify(modelChooser.getSelectedModels());
		var blob = new Blob([json], {type: 'application/json'});
		var filename = 'models-' + getTodaysDate() + '.json';
		downloadBlob(blob, filename);
	});

	setButtonStatuses();
})();