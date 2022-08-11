(function() {
	'use strict';

	var BASEURL = 'https://nata-dia2.jmpr.io/api/staging/';
	var SCOREURL = BASEURL + 'model/score';
	var VALIDATEURL = BASEURL + 'validation/validate_json';
	var PDFURL = BASEURL + 'report/pdf';
	var CSVURL = BASEURL + 'report/csv';
	var CHARTURL = BASEURL + 'chart/plot';


	var wasSuccessful = function(evt) {
		var statusCode = evt.currentTarget.status;
		return statusCode >= 200 && statusCode < 300;
	};


	var addScores = function (model, callback) {
		var request = new XMLHttpRequest();
		var handleLoad = function(evt) {
			if (wasSuccessful(evt)) {
				try {
					var result = JSON.parse(request.responseText);
					model.renderability =  Math.round(result.renderability);
					model.intellectual_control = Math.round(result.intellectual_control);
					model.advanced = result.nodes;
					if (callback) { callback(); }
				}
				catch(err) {
					handleError();
				}
			}
			else {
				handleError();
			}
		};
	
		var handleError = function() {
			console.error('Error: ' + request.status);
			if (callback) {
				var err = new Error('An error occurred trying to process model scores');
				callback(err);
			}	
		};
	
		request.open('POST', SCOREURL);
		request.setRequestHeader('Content-Type', 'application/json');
		request.addEventListener('load', handleLoad);
		request.addEventListener('error', handleError);
		request.send(JSON.stringify(model));
	};


	var validateModels = function (json, callback) {
		var request = new XMLHttpRequest();

		var handleLoad = function(evt) {
			if (wasSuccessful(evt)) {
				try {
					var result = JSON.parse(request.responseText).status;
					if (callback) { callback(null, result); }
				}
				catch(err) {
					handleError();
				}
			}
			else {
				handleError();
			}
		};
	
		var handleError = function() {
			console.error('Error: ' + request.status);
			if (callback) {
				var err = new Error('An error occurred trying to process model scores');
				callback(err);
			}
		};

		request.open('POST', VALIDATEURL);
		request.setRequestHeader('Content-Type', 'application/json');
		request.addEventListener('load', handleLoad);
		request.addEventListener('error', handleError);
		request.send(json);
	};


	var getPdf = function(models, callback) {
		var request = new XMLHttpRequest();

		var handleLoad = function(evt) {
			if (wasSuccessful(evt)) {
				var blob = new Blob([request.response], {type: 'application/pdf'});
				if (callback) { callback(null, blob); }
			}
			else {
				handleError();
			}
		};
	
		var handleError = function() {
			console.error('Error: ' + request.status);
			if (callback) {
				var err = new Error('An error occurred trying to download pdf');
				callback(err);
			}
		};
	
		request.responseType = 'blob';
		request.open('POST', PDFURL);
		request.setRequestHeader('Content-Type', 'application/json');
		request.addEventListener('load', handleLoad);
		request.addEventListener('error', handleError);
		request.send(JSON.stringify(models));
	};


	var getCsv = function(models, callback) {
		var request = new XMLHttpRequest();

		var handleLoad = function(evt) {
			if (wasSuccessful(evt)) {
				var blob = new Blob([request.response], {type: 'text/csv'});
				if (callback) { callback(null, blob); }
			}
			else {
				handleError();
			}
		};
	
		var handleError = function() {
			console.error('Error: ' + request.status);
			if (callback) {
				var err = new Error('An error occurred trying to download CSV');
				callback(err);
			}
		};
	
		request.open('POST', CSVURL);
		request.setRequestHeader('Content-Type', 'application/json');
		request.addEventListener('load', handleLoad);
		request.addEventListener('error', handleError);
		request.send(JSON.stringify(models));
	};


	var getChart = function(models, callback) {
		var request = new XMLHttpRequest();

		var handleLoad = function(evt) {
			if (wasSuccessful(evt)) {
				var blob = new Blob([request.response], {type: 'image/png'});
				if (callback) { callback(null, blob); }
			}
			else {
				handleError();
			}
		};
	
		var handleError = function() {
			console.error('Error: ' + request.status);
			if (callback) {
				var err = new Error('An error occurred trying to download chart');
				callback(err);
			}
		};
	
		request.responseType = 'blob';
		request.open('POST', CHARTURL);
		request.setRequestHeader('Content-Type', 'application/json');
		request.addEventListener('load', handleLoad);
		request.addEventListener('error', handleError);
		request.send(JSON.stringify(models));
	};


	window.api = {
		addScores: addScores,
		validateModels: validateModels,
		getPdf: getPdf,
		getCsv: getCsv,
		getChart: getChart
	};
})();