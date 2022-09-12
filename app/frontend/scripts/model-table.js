/* global utils, questions */
(function () {
	'use strict';

	var getModelValidationErrors = utils.getModelValidationErrors;
	var getScenarioValidationErrors = utils.getScenarioValidationErrors;

	var baseScenarioName = 'Base Model';
	var modelNameKey = 'model_name';
	var scenarioNameKey = 'scenario';
	var modelNotesKey = 'notes';
	var rowHeaderField = scenarioNameKey;

	var modelTableProps = {
		postFields: ['delete'],
		preFields: ['edit'],
		selectColumn: false,
		singleSelect: false
	};

	var modelTableSelectProps = {
		postFields: ['delete'],
		preFields: ['select', 'edit'],
		selectColumn: true,
		singleSelect: false
	};

	var modelTableSingleSelectProps = {
		postFields: ['delete'],
		preFields: ['select', 'edit'],
		selectColumn: true,
		singleSelect: true
	};

	var sharedFields = [
		'scenario',
		'notes',
		'is_advanced',
		'intellectual_control',
		'renderability',
		'response'
	];


	var fixedModels = (window.exampleModels || []);
	var fixedModelNames = fixedModels.map(function(model) { return model.model_name; });


	function getTableProps(tableType) {
		var tableProps;
		if (tableType === 'multi-select') { tableProps = modelTableSelectProps; }
		else if (tableType === 'single-select') { tableProps = modelTableSingleSelectProps; }
		else { tableProps = modelTableProps; }
		return tableProps;
	}


	var extractSavedModels = function () {
		return (JSON.parse(window.localStorage.getItem('models')) || []).concat(fixedModels);
	};

	var saveModels = function (models) {
		var filteredModels = models.filter(function(m) { return !fixedModelNames.includes(m[modelNameKey]); });
		window.localStorage.setItem('models', JSON.stringify(filteredModels));
	};


	var createModelTable = function(containerEntity, tableType) {
		var container = typeof containerEntity === 'string' ? document.querySelector(containerEntity): containerEntity;
		if (tableType === undefined) { tableType = 'no-select'; }
		var tableProps = getTableProps(tableType);
		var focusTrap = null;
		var selectionChangeCallback = null;
		var deleteCallback = null;
		var unselectableRadioFunction = function() { return false; };

		var table = document.createElement('table');
		table.classList.add('model-table');
		container.appendChild(table);

		var models = extractSavedModels();
		var captionText = null;
	
		function regenerateTable() {
			models = extractSavedModels();
			models.sort(function (a, b) {
				if (a.model_name > b.model_name) { return 1; }
				if (a.model_name === b.model_name) {
					if (a.scenario === baseScenarioName) { return -1; }
					if (b.scenario === baseScenarioName) { return 1; }
					return (a.scenario > b.scenario) ? 1 : -1;
				}
				return -1;
			});
			generateTable();
		}

		function getCheckedCheckboxes() {
			return table.querySelectorAll('tr input.cb:checked');
		}

		function getCheckedRadioButton() {
			return table.querySelector('tr input.radio:checked');
		}

		/**
		 * @param {string} inputString The input string
		 * @return {string} inputString with underscores replaced with spaces and capitalizing 
		 * the first letter of each word.
		 */
		function capitalizeAndReplaceUnderscores(inputString) {
			var arr = inputString.split('_');
			for (var i = 0; i < arr.length; i++) {
				arr[i] = arr[i].charAt(0).toUpperCase() + arr[i].slice(1);
			}
			return arr.join(' ');
		}
	
		/**
		 * generate a table head in the detected table
		 * @param {object[]} data an object array where the first object contains keys corresponding to the table fields
		 */
		function generateTableHead() {
			var thead = table.createTHead();
			var row = thead.insertRow();
			for (var key of tableProps.preFields.concat(sharedFields, tableProps.postFields)) {
				var th = document.createElement('th');
				var text = document.createTextNode(capitalizeAndReplaceUnderscores(key));
				th.setAttribute('id', 'column_header_' + key);
				if (key === 'notes') {
					th.setAttribute('class', 'wideCol');
				}
				th.setAttribute('scope', 'col');
				th.appendChild(text);
				row.appendChild(th);
			}
		}
	

		function generateTableBody() {
			table.createTBody();
			var counter = 0;
			var groupHeaders = {};
			models.forEach(function(rowData, index) {
				var groupHeaderName;
				groupHeaderName = rowData[modelNameKey];
				if (groupHeaders[groupHeaderName] === undefined) {
					createGroupHeaderRow(groupHeaderName, counter);
					groupHeaders[groupHeaderName] = counter++;
				}
				createRow(rowData, index, groupHeaders[groupHeaderName]);
			});
			if (tableType === 'single-select') {
				var firstRadio = document.querySelector('[name="model-choice"]:not([disabled="true"])');
				if (firstRadio) {
					firstRadio.checked = true;
				}
			}
		}
	
		function deleteClick(clicked) {
			var index = parseInt(clicked.dataset.index);
			var selectedModel = models[index];
			var model = selectedModel.model_name;
			var scenario = selectedModel.scenario;
			var isBaseScenario = scenario === baseScenarioName;
			
			var confirmMessage = 'Delete scenario: ' + model + ' ' + scenario + '?';
			if (isBaseScenario) {
				confirmMessage = 'Delete model: ' + model + ' and ALL related scenarios?';
			}

			if (confirm(confirmMessage)) {
				var remainingModels;
				if (isBaseScenario) {
					remainingModels = models.filter(function (obj) {
						return obj.model_name !== model;
					});
				}
				else {
					remainingModels = models.filter(function (obj, i) {
						return i !== index;
					});
				}
				models = remainingModels;
				saveModels(models);
				regenerateTable();
				if (deleteCallback) { deleteCallback(); }
			}
		}
	
		function editClick(clicked) {
			var editModal = document.createElement('div');
			document.body.appendChild(editModal);
			editModal.setAttribute('role', 'dialog');
			editModal.setAttribute('aria-modal', 'true');
			editModal.setAttribute('aria-labelledby', 'edit-modal-title');
			editModal.setAttribute('aria-describedby', 'edit-modal-description');

			var modalContent = document.createElement('div');
			modalContent.classList.add('modal-content');
			editModal.appendChild(modalContent);

			var close = document.createElement('button');
			close.setAttribute('id', 'close-edit');
			close.setAttribute('aria-label', 'Close');
			close.classList.add('close', 'btn-finish');
			close.innerText = '×';
			modalContent.appendChild(close);

			var title = document.createElement('h1');
			title.setAttribute('id', 'edit-modal-title');
			modalContent.appendChild(title);

			var description = document.createElement('p');
			description.setAttribute('id', 'edit-modal-description');
			modalContent.appendChild(description);

			focusTrap = [close];

			var selectedModel = models[clicked.dataset.index];
			var isBaseScenario = selectedModel.scenario === baseScenarioName;

			var inputList = [isBaseScenario ? 'Model Name' : 'Scenario Name', 'Notes'];

			inputList.forEach(function(labelText) {
				var kebab = labelText.toLowerCase().replace(/\s/g, '-');
				var id = 'edit-modal-' + kebab;
				var wrapper = document.createElement('div');
				wrapper.setAttribute('id', id + '-wrapper');
				var label = document.createElement('label');
				label.setAttribute('for', id);
				label.innerText = labelText;
				wrapper.appendChild(label);
				var formElement = document.createElement(labelText === 'Notes' ? 'textarea' : 'input');
				formElement.setAttribute('id', id);
				formElement.setAttribute('placeholder', labelText);
				formElement.classList.add('modalTextInput', 'form-control');
				wrapper.appendChild(formElement);
				modalContent.appendChild(wrapper);
				focusTrap.push(formElement);
			});

			var errorAlert = document.createElement('p');
			errorAlert.setAttribute('id', 'modal-alert');
			errorAlert.classList.add('warning');
			errorAlert.setAttribute('role', 'status');
			errorAlert.setAttribute('aria-atomic', true);
			modalContent.appendChild(errorAlert);

			var saveButton = document.createElement('input');
			saveButton.setAttribute('id', 'save-edit');
			saveButton.setAttribute('type', 'button');
			saveButton.setAttribute('value', 'Save');
			modalContent.appendChild(saveButton);

			focusTrap.push(saveButton);
				
			showDialog(editModal);
			addCloseModalFunctionality(close, clicked, editModal);

			initEditFormValues(editModal, selectedModel);
			initEditFormFunctionality(editModal, selectedModel);
		}
	
		function initEditFormValues(modal, selectedModel) {
			var editModalName = modal.querySelector('#edit-modal-model-name');
	
			if (editModalName) {
				editModalName.value = selectedModel[modelNameKey];
			}
			else {
				modal.querySelector('#edit-modal-scenario-name').value = selectedModel[scenarioNameKey];
			}

			modal.querySelector('#edit-modal-notes').value = selectedModel[modelNotesKey];
		}
	
		function initEditFormFunctionality(modal, selectedModel) {
			var saveButton = modal.querySelector('#save-edit');
			var isBaseScenario = !!(modal.querySelector('#edit-modal-model-name'));

			var editModalTitle = modal.querySelector('#edit-modal-title');
			var editModalDescription = modal.querySelector('#edit-modal-description');
			if (isBaseScenario) {
				editModalTitle.innerText = 'Edit Model: ' + selectedModel.model_name;
				editModalDescription.innerText = 'Currently editing the model: ' + selectedModel.model_name + '. This will affect associated scenarios';
			}
			else {
				editModalTitle.innerText = 'Edit Scenario: ' + selectedModel.scenario;
				editModalDescription.innerText = 'Currently editing the scenario ' + selectedModel.scenario + ' derived from the model: ' + selectedModel.model_name;
			}

			var saveFunc = isBaseScenario ? trySaveModelDetails : trySaveScenarioDetails;
			saveButton.addEventListener('click', function() { saveFunc(modal, selectedModel); });
		}
	
	
		function trySaveModelDetails(modal, selectedModel) {
			var modalAlert = modal.querySelector('#modal-alert');
			var oldName = selectedModel.model_name;
			var newName = modal.querySelector('#edit-modal-model-name').value;
			var newNotes = modal.querySelector('#edit-modal-notes').value;
			var modelValidationErrors = newName !== oldName ? getModelValidationErrors(newName, models) : '';
			if (modelValidationErrors) {
				modalAlert.innerText = 'Unable to save: ' + modelValidationErrors;
			}
			else {
				updateAllModelsWithName(selectedModel, newName, newNotes);
				modal.querySelector('#close-edit').click();
			}
		}
	
		function trySaveScenarioDetails(modal, selectedModel) {
			var modalAlert = modal.querySelector('#modal-alert');
			var oldName = selectedModel.scenario;
			var newName = modal.querySelector('#edit-modal-scenario-name').value;
			var newNotes = modal.querySelector('#edit-modal-notes').value;
			var scenarioValidationErrors = newName !== oldName ? getScenarioValidationErrors(selectedModel, newName, models) : '';
			if (scenarioValidationErrors) {
				modalAlert.innerText = 'Unable to save: ' + scenarioValidationErrors;
			}
			else {
				updateScenario(selectedModel, newName, newNotes);
				modal.querySelector('#close-edit').click();
			}
		}
	
		function updateScenario(selectedModel, newScenario, newModalNotes) {
			selectedModel.notes = newModalNotes;
			selectedModel.scenario = newScenario;
			saveModels(models);
			regenerateTable();
		}
	
		function updateAllModelsWithName(selectedModel, newModalName, newModalNotes) {
			selectedModel.notes = newModalNotes;
			models.forEach(function(model) {
				if (model.model_name === selectedModel.model_name) {
					model.model_name = newModalName;
				}
			});
	
			saveModels(models);
			regenerateTable();
		}

	
		function showDialog(dialog) {
			dialog.setAttribute('data-open', '');
		}
	
		function preventTabDefault(close) {
			close.addEventListener('keydown', function (e) {
				if (e.key === 'Tab') {
					e.preventDefault();
				}
			});
		}

	
		function responsesClick(clicked) {
			var responsesModal = document.createElement('div');
			document.body.appendChild(responsesModal);
			responsesModal.setAttribute('role', 'dialog');
			responsesModal.setAttribute('aria-modal', 'true');
			responsesModal.setAttribute('aria-labelledby', 'responses-modal-title');
			responsesModal.setAttribute('aria-describedby', 'responses-modal-description');

			var modalContent = document.createElement('div');
			modalContent.classList.add('modal-content');
			responsesModal.appendChild(modalContent);

			var close = document.createElement('button');
			close.setAttribute('id', 'close-responses');
			close.setAttribute('aria-label', 'Close');
			close.classList.add('close', 'btn-finish');
			close.innerText = '×';
			modalContent.appendChild(close);

			var title = document.createElement('h1');
			title.setAttribute('id', 'responses-modal-title');
			modalContent.appendChild(title);

			var description = document.createElement('p');
			description.setAttribute('id', 'responses-modal-description');
			modalContent.appendChild(description);

			var responsesTable = document.createElement('table');
			responsesTable.classList.add('zebra');
			modalContent.appendChild(responsesTable);

			var selectedModel = models[clicked.dataset.index];
	
			showDialog(responsesModal);
			preventTabDefault(close);
	
			title.innerText = 'Responses for ' + selectedModel.model_name + ', ' + selectedModel.scenario;
			description.innerText = 'A table of the responses to the questions for ' + selectedModel.model_name + ' ' + selectedModel.scenario;
			var answers = selectedModel.response;
			var questionData = generateQuestionData(questions, answers);

			var thead = responsesTable.createTHead();
			var tr = document.createElement('tr');
			thead.appendChild(tr);

			var properties = ['text', 'detail', 'answer'];
			
			properties.forEach(function(property) {
				var th = document.createElement('th');
				th.innerText = capitalizeAndReplaceUnderscores(property);
				tr.appendChild(th);
			});
			
			var tbody = responsesTable.createTBody();
			questionData.forEach(function(q) {
				var row = document.createElement('tr');
				properties.forEach(function(property) {
					var td = document.createElement('td');
					td.innerText = q[property];
					row.appendChild(td);
				});
				tbody.appendChild(row);
			});

			focusTrap = [close];
			addCloseModalFunctionality(close, clicked, responsesModal);
		}
	
		function generateQuestionData(questions, answers) {
			var outputArray = [];
			Object.keys(questions).forEach(
				function (q) {
					var question = questions[q];
					var nodeAnswers = answers[question.node];
					var answer;
					if (question.part) {
						var part = question.part;
						answer = nodeAnswers[part];
					}
					else {
						answer = nodeAnswers;
					}
					if (question.detail) {
						if (question.detail.length > 1) {
							question.detail.forEach(function (detail, i) {
								outputArray.push({
									text: question.text,
									detail: detail,
									answer: answer[i]
								});
							});
						}
						else {
							outputArray.push({
								text: question.text,
								detail: question.detail,
								answer: answer
							});
						}
					}
				}
			);
			return outputArray;
		}
	
		function addCloseModalFunctionality(closeElement, ModalTriggeringElement, dialogElement) {
			closeElement.focus();
			closeElement.addEventListener('click', function () {
				closeDialog(ModalTriggeringElement);
			});
	
			var addESC = function (e) {
				if (e.key === 'Escape') {
					closeDialog(ModalTriggeringElement);
				}
			};
	
			function closeDialog(ModalTriggeringElement) {
				dialogElement.remove();
				ModalTriggeringElement.focus();
				document.removeEventListener('keydown', addESC);
				focusTrap = null;
			}
	
			document.addEventListener('keydown', addESC);
			if (focusTrap) { focusTrap[0].focus(); }
		}
	
		function createGroupHeaderRow(text, index) {
			var tbody = table.querySelector('tbody');
			var groupHeaderRow = tbody.insertRow();
			groupHeaderRow.setAttribute('class', 'groupheader');
			var groupHeaderCell = document.createElement('th');
			groupHeaderCell.innerText = text;
			groupHeaderCell.setAttribute('id', 'group' + index);
			groupHeaderCell.setAttribute('colspan', 9);
			groupHeaderCell.setAttribute('scope', 'colgroup');
			groupHeaderCell.setAttribute('class', 'groupheader');
			groupHeaderRow.appendChild(groupHeaderCell);
		}
	

		function createSelectCell(props) {
			var id = 'checkbox-row-' + props.index;
			var containerDiv = document.createElement('div');
			containerDiv.setAttribute('class', 'cb_wrapper');
			var label = document.createElement('label');
			label.setAttribute('for', id);
			label.setAttribute('class', 'cb_label');
			label.textContent = 'Select ' + props.data[rowHeaderField];
			var checkBoxCell = props.row.insertCell();
			var cBox = document.createElement('input');

			cBox.setAttribute('class', 'cb');
			cBox.setAttribute('type', 'checkbox');
			cBox.setAttribute('value', 'no');
			cBox.setAttribute('id', id);
			cBox.setAttribute('data-index', props.index);

			containerDiv.appendChild(cBox);
			containerDiv.appendChild(label);
			checkBoxCell.setAttribute('headers', 'column_header_select ' + props.headers);
			checkBoxCell.appendChild(containerDiv);

			cBox.addEventListener('change', function() {
				if (selectionChangeCallback) { selectionChangeCallback(); }
			});

			if (props.index === 0) { cBox.checked = true; }
		}

		function createRadioCell(props) {
			var id = 'radio-row-' + props.index;
			var containerDiv = document.createElement('div');
			containerDiv.setAttribute('class', 'radio_wrapper');
			var label = document.createElement('label');
			label.setAttribute('for', id);
			label.setAttribute('class', 'radio_label');
			label.textContent = 'Select ' + props.data[rowHeaderField];
			var radioCell = props.row.insertCell();
			var radio = document.createElement('input');

			radio.setAttribute('class', 'radio');
			radio.setAttribute('type', 'radio');
			radio.setAttribute('value', props.index);
			radio.setAttribute('name', 'model-choice');
			radio.setAttribute('id', id);
			radio.setAttribute('data-index', props.index);
			if (props.unselectable) { radio.setAttribute('disabled', true); }

			containerDiv.appendChild(radio);
			containerDiv.appendChild(label);
			radioCell.setAttribute('headers', 'column_header_select ' + props.headers);
			radioCell.appendChild(containerDiv);

			radio.addEventListener('change', function() {
				if (selectionChangeCallback) { selectionChangeCallback(); }
			});
		}		
	
		function createEditCell(props) {
			var editCell = props.row.insertCell();
			editCell.setAttribute('headers', 'column_header_edit ' + props.headers);
			var editBtn = document.createElement('input');
			editBtn.setAttribute('type', 'button');
			editBtn.setAttribute('value', 'Edit');
			editBtn.setAttribute('id', 'editBtn' + props.index);
			editBtn.setAttribute('data-index', props.index);
			if (fixedModelNames.includes(props.data[modelNameKey])) { editBtn.setAttribute('disabled', true); }
			editBtn.addEventListener('click', function () { editClick(this); });
			editCell.appendChild(editBtn);
		}
	
		function createDeleteCell(props) {
			var deleteCell = props.row.insertCell();
			deleteCell.setAttribute('headers', 'column_header_delete ' + props.headers);
			var delBtn = document.createElement('input');
			delBtn.setAttribute('type', 'button');
			delBtn.setAttribute('value', 'Delete');
			delBtn.setAttribute('id', 'delBtn' + props.index);
			delBtn.setAttribute('data-index', props.index);
			if (fixedModelNames.includes(props.data[modelNameKey])) { delBtn.setAttribute('disabled', true); }
			delBtn.addEventListener('click', function () { deleteClick(this); });
			deleteCell.appendChild(delBtn);
		}
	
		function createRowHeaderCell(props) {
			var rowHeaderCell = document.createElement('th');
			rowHeaderCell.innerText = props.data[rowHeaderField];
			rowHeaderCell.setAttribute('scope', 'row');
			rowHeaderCell.setAttribute('id', 'row' + props.index);
			props.row.appendChild(rowHeaderCell);
		}
	
		function createResponsesCell(props) {
			var responseCell = props.row.insertCell();
			responseCell.setAttribute('headers', 'column_header_delete ' + props.headers);
			if (props.data['response'] !== null && !props.data.is_advanced) {
				var resBtn = document.createElement('input');
				resBtn.setAttribute('type', 'button');
				resBtn.setAttribute('value', 'Responses');
				resBtn.setAttribute('id', 'resBtn' + props.index);
				resBtn.setAttribute('data-index', props.index);
				resBtn.addEventListener('click', function() { responsesClick(this); });
				responseCell.appendChild(resBtn);
			}
		}

		function createIsAdvancedCell(props) {
			var cell = props.row.insertCell();
			cell.style.textAlign = 'center';
			cell.style.fontSize = '30px';
			cell.innerText = props.data.is_advanced ? '✓' : '';
			cell.setAttribute('headers', 'column_header_is_advanced' + ' ' + props.headers);
		}
	
		function createNotesCell(props) {
			var cell = props.row.insertCell();
			var div = document.createElement('div');
			div.setAttribute('class', 'notes-content');
			div.innerText = props.data.notes;
			cell.appendChild(div);
			cell.setAttribute('headers', 'column_header_notes' + ' ' + props.headers);
		}

		function createDataCell(key, props) {
			var cell = props.row.insertCell();
			cell.innerText = props.data[key];
			cell.setAttribute('headers', 'column_header_' + key + ' ' + props.headers);
			if (['intellectual_control', 'renderability'].includes(key)) {
				cell.style.textAlign = 'right';
			}
		}
	
		function createRow(rowData, index, headerIndex) {
			var tbody = table.querySelector('tbody');
			var row = tbody.insertRow();
			row.classList.add(rowData[scenarioNameKey] === baseScenarioName ? 'model-row' : 'scenario-row');

			var props = {
				row: row,
				data: rowData,
				index: index,
				headers: 'row' + index + ' group' + headerIndex,
				unselectable: unselectableRadioFunction(rowData)
			};

			if (tableProps.selectColumn) {
				if (tableProps.singleSelect) { createRadioCell(props); }
				else { createSelectCell(props); }
			}
			createEditCell(props);
			sharedFields.forEach(function(key) {
				if (key === rowHeaderField) {
					createRowHeaderCell(props);
				}
				else if (key === 'response') {
					createResponsesCell(props);
				}
				else if (key === 'is_advanced') {
					createIsAdvancedCell(props);
				}
				else if (key === 'notes') {
					createNotesCell(props);
				}
				else {
					createDataCell(key, props);
				}
			});

			createDeleteCell(props);
		}
	
		function generateTable() {
			table.innerText = '';
			if (captionText) {
				var caption = document.createElement('caption');
				caption.innerText = captionText;
				table.appendChild(caption);
			}
			if (models.length) {
				generateTableHead();
				generateTableBody(models);
			}
			else {
				var td = document.createElement('td');
				td.innerText = 'No models found. Create a model';
				table.appendChild(td);
			}
		}
	
		regenerateTable();

		document.addEventListener('keydown', function(evt) {
			if (evt.key === 'Tab' && focusTrap) {
				evt.preventDefault();
				var newIndex;
				var currentIndex = focusTrap.indexOf(document.activeElement);
				var lastIndex = focusTrap.length - 1;
				if (evt.shiftKey) {
					newIndex = currentIndex - 1;
					if (newIndex < 0) { newIndex = lastIndex; }
				}
				else {
					newIndex = currentIndex + 1;
					if (newIndex > lastIndex) { newIndex = 0; }
				}
				focusTrap[newIndex].focus();
			}
		});


		var modelTable = {};
		
		modelTable.regenerateTable = function() {
			regenerateTable();
			return modelTable;
		};

		modelTable.getSelectedModels = function() {
			var selectedModels = [];

			if (tableProps.selectColumn) {
				if (tableProps.singleSelect) {
					var radio = getCheckedRadioButton();
					if (radio) {
						selectedModels.push(models[radio.dataset.index]);
					}
				}
				else {
					getCheckedCheckboxes().forEach(function(checkbox) {
						selectedModels.push(models[checkbox.dataset.index]);
					});
				}
			}
			
			return selectedModels;
		};

		modelTable.setCaptionText = function(text) {
			captionText = text;
			return modelTable;
		};

		modelTable.setSelectionChangeCallback = function(callback) {
			selectionChangeCallback = callback;
			return modelTable;
		};

		modelTable.setDeleteCallback = function(callback) {
			deleteCallback = callback;
			return modelTable;
		};

		modelTable.setUnselectableRadioFunction = function(func) {
			unselectableRadioFunction = func;
			return modelTable;
		};

		return modelTable;
	};


	window.createModelTable = createModelTable;
})();