/* global utils, api */
(function() {
	'use strict';

	var createLoadingIndicator = utils.createLoadingIndicator;
	var getModelValidationErrors  = utils.getModelValidationErrors;
	var getScenarioValidationErrors  = utils.getScenarioValidationErrors;
	
	var storage = window.localStorage;

	var uploadButton = document.querySelector('#upload-button');
	var uploadInput = document.querySelector('#upload-input');
	
	uploadButton.addEventListener('click', function() {
		uploadInput.click();
	});

	var loadingIndicator = createLoadingIndicator(document.querySelector('#loading-indicator-container'))
		.setText('Uploading file');

	// File name is user-supplied so we should treat it as untrusted
	var getFileName = function(fileObj) {
		var dummyDiv = document.createElement('div');
		dummyDiv.innerText = fileObj.name;
		return dummyDiv.innerText;
	};


	uploadInput.addEventListener('change', function() {
		var fileObj = this.files[0];
		var reader = new FileReader();
		loadingIndicator.show();

		reader.addEventListener('load', function(evt) {
			var payload = evt.target.result;
			uploadInput.value = null;
			api.validateModels(payload, function(err, isValid) {
				loadingIndicator.hide();
				if (err) {
					alert(err.message);
				}
				else if (!isValid) {
					alert('Could not extract models from file');
				}
				else {
					var models = JSON.parse(storage.getItem('models')) || [];
					var newModels;
					try {
						// This shouldn't ever throw if API validation works
						newModels = JSON.parse(payload);
					}
					catch (err) {
						alert('Could not extract models from file');
						return;
					}
					var localValidationErrors = [];
					var errors;
					for(var model in newModels){
						if (newModels[model]['scenario'] === 'Base Model'){
							errors = getModelValidationErrors(newModels[model].model_name,models);
							if(errors){
								localValidationErrors.push(errors);
							}
						}
						else {
							errors = getScenarioValidationErrors(model ,newModels[model].scenario, models);
							if (errors) {
								localValidationErrors.push(errors);
							}
						}
					}
					if (localValidationErrors.length){
						localValidationErrors.unshift('The file contains models with the following validation violations:');
						localValidationErrors.push('No new models were added.');
						alert(localValidationErrors.join('\n\n'));
						return;
					}
					newModels.forEach(function(model) { models.push(model); });
					storage.setItem('models', JSON.stringify(models));
					var numberOfNewModels = newModels.length;
					var message;
					if (numberOfNewModels === 1) {
						message = 'Succssfully extracted and saved model from ';
					}
					else {
						message = 'Succssfully extracted and saved ' + numberOfNewModels + ' models from ';
					}
					message += getFileName(fileObj);
					alert(message);
				}
			});
		});

		reader.readAsText(fileObj);
	});
})();