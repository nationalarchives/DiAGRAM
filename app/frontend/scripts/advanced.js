/* global utils, getSubnetwork, defaultAdvanced, createModelTable, api */
(function() {
	'use strict';

	var hideNode = utils.hideNode;
	var showNode = utils.showNode;
	var createLoadingIndicator = utils.createLoadingIndicator;
	var getModelValidationErrors  = utils.getModelValidationErrors;
	var getScenarioValidationErrors  = utils.getScenarioValidationErrors;

	var model, models;
	var extractSavedModels = function () {
		return JSON.parse(window.localStorage.getItem('models')) || [];
	};

	var decimalPlaces = 2;
	models = extractSavedModels();


	var textToProperty = {
		'Operating Environment': 'Op_Environment',
		'Replication and Refreshment': 'Rep_and_Refresh',
		'Physical Disaster': 'Physical_Disaster',
		'Storage Medium': 'Storage_Medium',
		'Technical Skills': 'Technical_Skills',
		'Digital Object': 'Digital_Object',
		'Information Management': 'Info_Management',
		'System Security': 'System_Security',
		'Checksum': 'Checksum',
		'File Format': 'File_Format',
		'Storage Life': 'Storage_Life',
		'Obsolescence': 'Obsolescence',
		'Tools to Render': 'Tools_to_Render',
		'Technical Metadata': 'Tech_Metadata',
		'Conditions of Use': 'Conditions_of_Use',
		'Content Metadata': 'Content_Metadata',
		'Integrity': 'Integrity',
		'Bit Preservation': 'Bit_Preservation',
		'Identity': 'Identity'
	};

	var propertyToText = Object.keys(textToProperty).reduce(function (obj, text) {
		var prop = textToProperty[text];
		obj[prop] = text;
		return obj;
	}, {});


	var transpose = function (input) {
		var output = [];
		if (input.length) {
			for (var i = 0; i < input.length; i++) {
				for (var j = 0; j < input[0].length; j++) {
					if (i === 0) { output.push([]); }
					output[j][i] = input[i][j];
				}
			}
		}
		return output;
	};

	var deepCopyObject = function(obj, transform) {
		return JSON.parse(JSON.stringify(obj, transform));
	};

	var clamp = function (initial, min, max) {
		var output = initial;
		if (initial < min) { output = min; }
		else if (initial > max) { output = max; }
		return output;
	};

	var objectsAreDifferent = function (a, b) {
		return JSON.stringify(a) !== JSON.stringify(b);
	};

	var roundTo2DP = function(num) {
		return Math.round((num + Number.EPSILON) * 100) / 100;
	};

	var decimalToPercentage = function(_key, value) {
		var output = value;
		if (typeof value === 'number') {
			output *= 100; // convert from decimal to percentage
			output = roundTo2DP(output); // tidy-up floating-point junk
		}
		return output;
	};

	var percentageToDecimal = function(_key, value) {
		var output = value;
		if (typeof value === 'number') { output /= 100; }
		return output;
	};

	// This function both converts from number to string and fixes decimal places shown
	var fixed = function(value) {
		return value.toFixed(decimalPlaces);
	};


	var mode, referenceModel, referenceAdvanced, newAdvanced;
	var changedNodes = [];

	var modelChooser = createModelTable('#choose-model-table', 'single-select');

	var loadingIndicator = createLoadingIndicator(document.querySelector('#loading-indicator-container'))
		.setText('Calculating scores');

	document.querySelector('#start').addEventListener('click', function() {
		hideNode('#start-container');
		showNode('#configure');
		if (document.activeElement === this) {
			document.querySelector('#configure h3').focus();
		}
	});

	document.querySelector('#from-scratch').addEventListener('click', function () {
		hideNode('#scratch-modify');
		showNode('#adv-options');
		hideNode('#choose-existing-container');
		showNode('#choose-new-container');
		if (document.activeElement === this) {
			document.querySelector('#adv-name-new').focus();
		}
	});

	document.querySelector('#modify').addEventListener('click', function () {
		hideNode('#scratch-modify');
		showNode('#adv-options');
		showNode('#choose-model-table');
		hideNode('#adv-name-controls');
		hideNode('#choose-new-container');
		showNode('#choose-existing-container');
		if (document.activeElement === this) {
			document.querySelector('#adv-name-existing').focus();
		}
	});


	document.querySelector('#adv-name-new').addEventListener('input', function () {
		var startButton = document.querySelector('#choose-new-container .btn-start');
		var textAlert = document.querySelector('#valid-newmodel-text-alert');
		var validationErrors = getModelValidationErrors(this.value, models);
		
		if (!validationErrors) {
			startButton.classList.remove('hidden');
		}
		else {
			startButton.classList.add('hidden');
		}

		textAlert.innerText = validationErrors && this.value.length ? validationErrors : '';
	});


	var processChangeToExistingForm = function() {
		model = modelChooser.getSelectedModels()[0];
		var startButton = document.querySelector('#choose-existing-container .btn-start');
		var nameField = document.querySelector('#adv-name-existing');
		var textAlert = document.querySelector('#valid-existing-text-alert');
		var value = nameField.value;
		var validationErrors;

		if (document.querySelector('#radio-model').checked) {
			validationErrors = getModelValidationErrors(value, models);
		}
		else {
			validationErrors = getScenarioValidationErrors(model, value, models);
		}

		if (!validationErrors) {
			startButton.classList.remove('hidden');
		}
		else {
			startButton.classList.add('hidden');
		}

		textAlert.innerText = '';
		if (validationErrors && value.length) {
			var span = document.createElement('span');
			span.innerText =  validationErrors;
			textAlert.appendChild(span);
			var close = document.createElement('button');
			close.innerText = close.innerText = '×';
			close.setAttribute('id', 'alert-close');
			close.setAttribute('aria-label', 'Dismiss warning');
			close.classList.add('btn-finish-invert');
			textAlert.appendChild(close);

			close.addEventListener('click', function() {
				textAlert.innerText = '';
				// Don't refocus on nameField if current focus is on some other element
				// This can happen if pressing the escape key
				var refocusList = [close, nameField, document.body, null];
				if (refocusList.some(function(d) { return d === document.activeElement; })) {
					nameField.focus();
				}
			});
		}

		document.querySelector('#no-name').classList[value.length ? 'add' : 'remove']('hidden');
	};

	document.addEventListener('keydown', function(evt) {
		if (evt.key === 'Escape') {
			var close = document.querySelector('#alert-close');
			if (close) { close.click(); }
		}
	});

	document.querySelector('#adv-name-existing').addEventListener('input', function () {
		processChangeToExistingForm();
	});

	document.querySelector('#new-adv-radio-group').addEventListener('change', function () {
		processChangeToExistingForm();
	});

	modelChooser.setSelectionChangeCallback(processChangeToExistingForm);


	document.querySelectorAll('#adv-options .btn-previous').forEach(function (node) {
		node.addEventListener('click', function () {
			hideNode('#adv-options');
			showNode('#scratch-modify');
			if (document.activeElement === this) {
				document.querySelector('#configure h3').focus();
			}
		});
	});

	document.querySelectorAll('#adv-options .btn-next').forEach(function (node) {
		node.addEventListener('click', function () {
			hideNode('#choose-model-table');
			showNode('#adv-name-controls');
			document.querySelector('#adv-name-existing').value = '';
		});
	});

	document.querySelectorAll('#adv-options .btn-start').forEach(function (node) {
		node.addEventListener('click', function () {
			hideNode('#configure');
			showNode('#edit');
			showNode('#changed-nodes');
			showNode('#store');
			if (document.activeElement === this) {
				document.querySelector('#edit h3').focus();
			}

			mode = node.dataset.mode;

			if (mode === 'existing') {
				referenceModel = modelChooser.getSelectedModels()[0];
			}
			else {
				referenceModel = {
					model_name: 'Default advanced',
					scenario: 'Base Model',
					notes: '',
					is_advanced: true,
					intellectual_control: null,
					renderability: null,
					response: null,
					advanced: defaultAdvanced
				};
			}
			referenceAdvanced = deepCopyObject(referenceModel.advanced, decimalToPercentage);
			newAdvanced = deepCopyObject(referenceAdvanced);
		});
	});

	var svg = document.querySelector('.svg-container svg');
	var selectEdit = document.querySelector('#select-edit');

	var getRow = function (node) {
		var current = node;
		while (current.nodeName !== 'TR') {
			current = current.parentElement;
		}
		return current;
	};


	var getNewValues = (function () {
		var getWeights = function (values) {
			var a = values[0];
			var b = values[1];
			var weights;
			if (a === 0 && b === 0) {
				weights = [0.5, 0.5];
			}
			else if (b === 0) {
				weights = [1, 0];
			}
			else {
				var ratio = a / b;
				var denominator = 1 + ratio;
				weights = [ratio / denominator, 1 / denominator];
			}
			return weights;
		};


		return function (oldValues, amountAvailable) {
			var weights = getWeights(oldValues);
			return weights.map(function (w) { return w * amountAvailable; });
		};
	})();

	var processInputPair = function (inputs, input) {
		var otherInput = inputs[0] === input ? inputs[1] : inputs[0];
		otherInput.value = fixed(100 - input.value);
	};

	// This is same logic as used for slider triplets in questions.js
	var processInputTrio = function (inputs, input) {
		var values, index;
		var value = parseFloat(input.value);
		for (var i = 0; i < inputs.length; i++) {
			if (inputs[i] === input) {
				index = i;
				break;
			}
		}

		if (index === 0) {
			var oldValue1 = parseFloat(inputs[1].value);
			var oldValue2 = parseFloat(inputs[2].value);
			var oldValues = [oldValue1, oldValue2];
			values = [value].concat(getNewValues(oldValues, 100 - value));
		}
		else {
			var oldValue0 = parseFloat(inputs[0].value);
			var amountAvailable = 100 - oldValue0;
			var diff = value - amountAvailable;
			if (index === 1) {
				if (diff > 0) {
					values = [oldValue0 - diff, value, 0];
				}
				else {
					values = [oldValue0, value, 100 - (value + oldValue0)];
				}
			}
			else { // index === 2
				if (diff > 0) {
					values = [oldValue0 - diff, 0, value];
				}
				else {
					values = [oldValue0, 100 - (value + oldValue0), value];
				}
			}
		}
		values.forEach(function(v, i) { inputs[i].value = fixed(v); });
	};


	var updateInputs = function (input) {
		var raw = parseFloat(input.value) || 0;
		input.value = fixed(clamp(raw, 0, 100));
		var row = getRow(input);
		var inputs = row.querySelectorAll('input');
		if (inputs.length === 2) { processInputPair(inputs, input); }
		else { processInputTrio(inputs, input); }
	};

	var table = document.querySelector('#editing-table');
	var tableCache = {};

	var getCurrentKey = function () {
		var tbody = document.querySelector('#editing-table tbody');
		return tbody ? textToProperty[tbody.dataset.name] : null;
	};

	var getCurrentValues = function () {
		var tbody = table.querySelector('tbody');
		var name = tbody.dataset.name;
		var prop = textToProperty[name];
		var obj = deepCopyObject(newAdvanced[prop]);
		var keys = Object.keys(obj);
		var rows = tbody.querySelectorAll('tr');
		for (var i = 0; i < rows.length; i++) {
			var row = rows[i];
			var cells = row.querySelectorAll('.cell');
			for (var j = 0; j < cells.length; j++) {
				var cell = cells[j];
				var input = cell.querySelector('input');
				if (input) {
					var key = keys[j];
					if (Array.isArray(obj[key])) {
						obj[key][i] = parseFloat(input.value);
					}
					else {
						obj[key] = parseFloat(input.value);
					}
				}
			}
		}
		return obj;
	};

	var createRowNumberCell = function () {
		var th = document.createElement('th');
		var span = document.createElement('span');
		span.classList.add('screen-reader-only');
		span.innerText = 'Row number';
		th.appendChild(span);
		return th;
	};


	var makeTable = function (name, headers, rows) {
		var th;

		var thead = document.createElement('thead');
		var tr = document.createElement('tr');
		tr.appendChild(createRowNumberCell());

		for (var i = 0; i < headers.length; i++) {
			th = document.createElement('th');
			var headerText = headers[i];
			if (typeof rows[0][i] === 'number') { headerText += ' (%)'; }
			th.innerText = headerText;
			tr.appendChild(th);
		}
		thead.appendChild(tr);
		table.appendChild(thead);

		var tbody;
		if (tableCache[name]) {
			tbody = tableCache[name];
		}
		else {
			tbody = document.createElement('tbody');
			tbody.setAttribute('data-name', name);
			for (i = 0; i < rows.length; i++) {
				tr = document.createElement('tr');
				th = document.createElement('th');
				th.innerText = i + 1;
				tr.appendChild(th);
				var row = rows[i];
				for (var j = 0; j < row.length; j++) {
					var value = row[j];
					var td = document.createElement('td');
					td.classList.add('cell');
					var text = value;
					if (typeof value === 'number') {
						var input = document.createElement('input');
						input.setAttribute('type', 'number');
						input.setAttribute('min', 0);
						input.setAttribute('step', fixed(Math.pow(10, -decimalPlaces)));
						input.setAttribute('max', 100);
						input.setAttribute('value', fixed(value));
						td.appendChild(input);
					}
					else {
						td.innerText = text;
					}
					tr.appendChild(td);
				}
				tbody.appendChild(tr);
			}
		}
		table.appendChild(tbody);

		setAddButtonStatus();
	};

	var setAddButtonStatus = function () {
		var addChange = document.querySelector('#add-change');
		if (objectsAreDifferent(newAdvanced[getCurrentKey()], getCurrentValues())) {
			addChange.removeAttribute('disabled');
		}
		else {
			addChange.setAttribute('disabled', true);
		}
	};

	table.addEventListener('change', function (evt) {
		var input = evt.target;
		if (input.nodeName !== 'INPUT') { return; }
		updateInputs(input);
		setAddButtonStatus();
	});

	var clearTable = function () {
		var tbody = table.querySelector('tbody');
		if (tbody) {
			var name = tbody.dataset.name;
			tableCache[name] = tbody;
		}
		table.innerText = '';
	};

	var getTableHeaders = function (obj) {
		return Object.keys(obj).map(function (h) { return propertyToText[h] || h; });
	};

	var getTableContent = function (obj) {
		var content;
		var multipleRows = Array.isArray(obj[Object.keys(obj)[0]]);
		if (multipleRows) {
			content = transpose(Object.keys(obj).map(function (k) { return obj[k]; }));
		}
		else {
			content = [Object.keys(obj).map(function (k) { return obj[k]; })];
		}
		return content;
	};


	var setEditing = function (name) {
		var editingContainer = document.querySelector('#editing-container');
		clearSubnetworkClasses();
		clearTable();
		if (!name) {
			svg.classList.remove('is-selection');
			hideNode(editingContainer);
			selectEdit.selectedIndex = 0;
		}
		else {
			var vertex;
			for (i = 0; i < vertexes.length; i++) {
				var ng = vertexes[i];
				if (ng.name === name) {
					vertex = ng;
					break;
				}
			}
			svg.classList.add('is-selection');
			vertex.classList.add('selected');
			var subnetwork = getSubnetwork(vertex);
			subnetwork.forEach(function (node) {
				node.classList.add('subnetwork');
				node.parentNode.appendChild(node);
			});

			var options = selectEdit.options;
			for (var i = 1; i < options.length; i++) {
				var option = options[i];
				if (option.innerText === name) {
					selectEdit.selectedIndex = i;
					break;
				}
			}

			showNode(editingContainer);
			var prop = textToProperty[name];
			editingContainer.querySelector('#editing-name').innerText = name;
			editingContainer.querySelector('#glossary-link').setAttribute('href', 'glossary.html#' + prop);

			var data = newAdvanced[prop];
			var headers = getTableHeaders(data);
			var rows = getTableContent(data);
			makeTable(name, headers, rows);
		}
	};

	var vertexes = svg.querySelectorAll('.vertex');
	var edges = svg.querySelectorAll('.edge');

	var clearSubnetworkClasses = function () {
		vertexes.forEach(function (vertex) {
			vertex.classList.remove('selected', 'subnetwork');
		});
		edges.forEach(function (edge) {
			edge.classList.remove('subnetwork');
		});
	};

	vertexes.forEach(function (vertex) {
		var name = vertex.querySelector('desc').innerHTML;
		vertex.name = name;
		var editable = name !== 'Renderability' && name !== 'Intellectual Control';

		if (editable) {
			vertex.classList.add('editable');

			vertex.addEventListener('mouseover', function () {
				vertex.classList.add('hovered');
			});

			vertex.addEventListener('mouseout', function () {
				vertex.classList.remove('hovered');
			});

			vertex.addEventListener('click', function (evt) {
				evt.stopPropagation();
				var alreadySelected = vertex.classList.contains('selected');
				setEditing(alreadySelected ? null : name);
			});
		}
		else {
			vertex.addEventListener('click', function (evt) {
				evt.stopPropagation();
			});
		}
	});

	selectEdit.addEventListener('change', function () {
		var index = selectEdit.selectedIndex;
		if (index === 0) { setEditing(null); }
		else { setEditing(selectEdit.options[index].innerText); }
	});

	svg.addEventListener('click', function () {
		setEditing(null);
	});

	document.querySelector('#add-change').addEventListener('click', function (evt) {
		var key = getCurrentKey();
		newAdvanced[key] = getCurrentValues();
		if (evt.pointerType === '') { selectEdit.focus(); }
		setAddButtonStatus();
		// If there is already an entry for the node with the given key we remove it
		// as we want to add it to the front of the changedNodes array
		if (changedNodes.includes(key)) {
			changedNodes.splice(changedNodes.indexOf(key), 1);
		}
		// We only add our key if one or more object values are different to their original values
		// That will occasionally not be the case if values are changed, saved, and then changed back
		if (objectsAreDifferent(referenceAdvanced[key], newAdvanced[key])) {
			changedNodes.unshift(key);
		}
		updateChangesList();
	});


	var updateChangesList = function () {
		var noNodesChanged = document.querySelector('#no-nodes-changed');
		var changedNodesList = document.querySelector('#changed-nodes-list');
		changedNodesList.innerText = '';
		var storeButton = document.querySelector('#store-model');

		if (!changedNodes.length) {
			showNode(noNodesChanged);
			hideNode(changedNodesList);
			storeButton.setAttribute('disabled', true);
		}
		else {
			hideNode(noNodesChanged);
			showNode(changedNodesList);
			storeButton.removeAttribute('disabled');
		}

		changedNodes.forEach(function (key) {
			var details = document.createElement('details');
			var summary = document.createElement('summary');
			summary.innerText = propertyToText[key];

			var oldValues = referenceAdvanced[key];
			var newValues = newAdvanced[key];

			var th, tr;
			var wrapper = document.createElement('div');
			wrapper.classList.add('table-wrapper');
			var table = document.createElement('table');
			table.classList.add('zebra');
			wrapper.appendChild(table);

			var headers = getTableHeaders(newValues);
			var newContent = getTableContent(newValues);
			var oldContent = getTableContent(oldValues);

			var thead = document.createElement('thead');
			tr = document.createElement('tr');
			tr.appendChild(createRowNumberCell());
			for (var i = 0; i < headers.length; i++) {
				th = document.createElement('th');
				var headerText = headers[i];
				if (typeof newContent[0][i] === 'number') { headerText += ' (%)'; }
				th.innerText = headerText;
				tr.appendChild(th);
			}
			thead.appendChild(tr);
			table.appendChild(thead);

			var tbody = document.createElement('tbody');
			for (i = 0; i < newContent.length; i++) {
				tr = document.createElement('tr');
				th = document.createElement('th');
				th.innerText = i + 1;
				tr.appendChild(th);
				var row = newContent[i];
				for (var j = 0; j < row.length; j++) {
					var value = row[j];
					var td = document.createElement('td');
					if (typeof value === 'number') {
						var text = fixed(value);
						var previousValue = oldContent[i][j];
						var diff = value - previousValue;
						if (diff > 0) {
							text += ' (⬆' + fixed(diff) + ')';
						}
						else if (diff < 0) {
							text += ' (⬇' + fixed(Math.abs(diff)) + ')';
						}
						td.innerText = text;
					}
					else {
						td.innerText = value;
					}
					tr.appendChild(td);
				}
				tbody.appendChild(tr);
			}
			table.appendChild(tbody);

			details.appendChild(summary);
			details.appendChild(wrapper);
			changedNodesList.appendChild(details);
		});
	};

	document.querySelector('#store-model').addEventListener('click', function() {
		var focused = document.activeElement === this;
		hideNode('#edit');
		hideNode('#changed-nodes');
		hideNode('#store');
		showNode('#post-editing');
		var newModel = deepCopyObject(referenceModel);
		newModel.advanced = deepCopyObject(newAdvanced, percentageToDecimal);
		newModel.is_advanced = true;

		if (mode === 'new') {
			newModel.model_name = document.querySelector('#adv-name-new').value;
		}
		else {
			var newName = document.querySelector('#adv-name-existing').value;
			if (document.querySelector('input[name="type"]:checked').value === 'model') {
				newModel.model_name = newName;
				newModel.scenario = 'Base Model';
			}
			else {
				newModel.scenario = newName;
			}
		}
		newModel.notes = document.querySelector('#advanced-comments').value;

		loadingIndicator.show();
		api.addScores(newModel, function(err) {
			loadingIndicator.hide();
			showNode('#what-next');
			if (err) {
				alert(err.message);
			}
			else {
				saveModel(newModel);
				if (focused) {
					document.querySelector('#post-editing h2').focus();
				}
			}
		});
	});


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
	};
})();