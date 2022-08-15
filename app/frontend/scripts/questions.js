/* global utils */
(function() {
	'use strict';

	var hideNode = utils.hideNode;
	var showNode = utils.showNode;

	var getSliderInput = function(container) {
		return container.querySelector('input[type="range"]');
	};

	var getNumberInput = function(container) {
		return container.querySelector('input[type="number"]');
	};

	var setSliderValue = function(container, value) {
		if (value < 0 || value > 100) {
			throw new RangeError('The value of a slider must be between 0 and 100');
		}
		getSliderInput(container).value = value;
		getNumberInput(container).value = value;
	};

	var selectRadioButton = function(selected) {
		var siblings = selected.parentElement.querySelectorAll('button[role=radio]');
		siblings.forEach(function(current) {
			if (current === selected) {
				current.classList.add('selected');
				current.setAttribute('aria-checked', true);
			}
			else {
				current.classList.remove('selected');
				current.setAttribute('aria-checked', false);
			}
		});
	};

	var extractSingleButtonGroupAnswer = function(question) {
		var radiogroup = question.querySelector('.answer[role="radiogroup"]');
		return radiogroup.querySelector('.selected').innerText;
	};

	var extractMultipleButtonGroupsAnswer = function(question) {
		var values = [];
		question.querySelectorAll('.answer[role="radiogroup"]').forEach(function(radiogroup) {
			values.push(radiogroup.querySelector('.selected').innerText);
		});
		return values;
	};

	var extractSingleSliderAnswer = function(question) {
		var sliderContainer = question.querySelector('.slider-single');
		return parseInt(getNumberInput(sliderContainer).value);
	};

	var extractTripleSliderAnswer = function(question) {
		var values = [];
		question.querySelectorAll('.slider-container').forEach(function(sliderContainer) {
			values.push(parseInt(getNumberInput(sliderContainer).value));
		});
		return values;
	};

	var extractAnswer = function(question) {
		var answer;
		switch(question.dataset.interface) {
			case 'single-button-group':
				answer = extractSingleButtonGroupAnswer(question);
				break;
			case 'single-slider':
				answer = extractSingleSliderAnswer(question);
				break;
			case 'multiple-button-groups':
				answer = extractMultipleButtonGroupsAnswer(question);
				break;
			case 'triple-slider':
				answer = extractTripleSliderAnswer(question);
				break;
			default:
				throw new Error('Unrecognised interface ' + question.dataset.interface);
		}
		return answer;
	};

	var setButtonGroupFromText = function(radiogroup, value) {
		var buttons = radiogroup.querySelectorAll('button[role="radio"]');
		var foundButton = false;
		for (var i = 0; i < buttons.length; i++) {
			var button = buttons[i];
			if (button.innerText === value) {
				selectRadioButton(button);
				foundButton = true;
				break;
			}
		}
		if (!foundButton) {
			throw new Error('Radiogroup ' + radiogroup.getAttribute('id') + ' has no button with text "' + value + '"');
		}
	};

	var setSingleButtonGroupAnswer = function(question, answer) {
		if (typeof answer !== 'string') {
			throw new TypeError('The answer for a button group question should be a string');
		}
		var radiogroup = question.querySelector('.answer[role="radiogroup"]');
		setButtonGroupFromText(radiogroup, answer);
	};

	var setMultipleButtonGroupsAnswer = function(question, answer) {
		if (!Array.isArray(answer)) {
			throw new TypeError('The answer for a multiple button-group question must be an array');
		}
		if (answer.some(function(value) { return typeof value !== 'string'; })) {
			throw new TypeError('The value for a button group must be a string');
		}
		var radiogroups = question.querySelectorAll('.answer[role="radiogroup"]');
		answer.forEach(function(value, index) {
			setButtonGroupFromText(radiogroups[index], value);
		});
	};

	var setSingleSliderAnswer = function(question, answer) {
		if (typeof answer !== 'number') {
			throw new TypeError('The answer for a single slider must be a number');
		}
		setSliderValue(question.querySelector('.slider-container'), answer);
	};

	var setTripleSliderAnswer = function(question, answer) {
		if (!Array.isArray(answer)) {
			throw new TypeError('The answer for a triple slider should be an array');
		}
		if (answer.length !== 3) {
			throw new RangeError('The answer for a triple slider should be an array of length 3');
		}
		if (answer.some(function(v) { return typeof v !== 'number'; })) {
			throw new TypeError('The value for a slider must be a number');
		}
		if (answer.reduce(function(sum, v) { sum += v; return sum; }, 0) !== 100) {
			throw new RangeError('The sum of answers for a triple slider must be 100');
		}
		var sliderContainers = question.querySelectorAll('.slider-container');
		answer.forEach(function(v, i) {
			setSliderValue(sliderContainers[i], v);
		});
	};

	var setAnswer = function(question, answer) {
		try {
			switch(question.dataset.interface) {
				case 'single-button-group':
					setSingleButtonGroupAnswer(question, answer);
					break;
				case 'single-slider':
					setSingleSliderAnswer(question, answer);
					break;
				case 'multiple-button-groups':
					setMultipleButtonGroupsAnswer(question, answer);
					break;
				case 'triple-slider':
					setTripleSliderAnswer(question, answer);
					break;
				default:
					throw new Error('Unrecognised interface ' + question.dataset.interface);
			}
		}
		catch (e) {
			console.error(e);
		}
	};


	var initQuestions = function() {
		var onUnstart, onEnd;
		var quest = document.querySelector('#questions-section');
		var allQuestions = quest.querySelectorAll('#questions .question');
		var previousButton = quest.querySelector('#questions-buttons .btn-previous');
		var nextButton = quest.querySelector('#questions-buttons .btn-next');
		var finishButton = quest.querySelector('#questions-buttons .btn-finish');
		var questions, firstQuestion, lastQuestion, questionCounter;

		var startQuestioning = function(name, skipSections) {
			quest.querySelector('#questions-name').innerText = name;

			skipSections = skipSections || [];
			allQuestions.forEach(function(question) {
				var section = question.dataset.section;
				if (skipSections.indexOf(section) !== -1) {
					question.classList.add('skip');
				}
			});
	
			questions = quest.querySelectorAll('#questions .question:not(.skip)');
			firstQuestion = questions[0];
			lastQuestion = questions[questions.length - 1];
			showNode(quest);
			showNode(firstQuestion);

			// If there's only one question then we need to hide the next button
			// and show the finish button instead
			if (firstQuestion === lastQuestion) {
				hideNode(nextButton);
				showNode(finishButton);
			}

			questionCounter = 0;
			updateProgress();
		};

		var unstartQuestioning = function() {
			hideNode(quest);
			hideNode(firstQuestion);
			allQuestions.forEach(function(question) {
				question.classList.remove('skip');
			});
			if (onUnstart) { onUnstart(); }
		};

		var endQuestioning = function() {
			hideNode(quest);
			if (onEnd) { onEnd(); }
		};

		var updateProgress = function() {
			var progressAmount = (questionCounter / questions.length) * 100;
			var progressNode = quest.querySelector('#questions-progress progress');
			progressNode.value = progressAmount;
			quest.querySelector('#questions-progress #questions-progress-text').innerHTML = Math.round(progressAmount);
		};

		previousButton.addEventListener('click', function() {
			var currentQuestion = questions[questionCounter];
			var previousQuestion = questions[questionCounter - 1];

			if (currentQuestion === firstQuestion) {
				unstartQuestioning();
			}
			else {
				hideNode(currentQuestion);
				showNode(previousQuestion);
				if (document.activeElement === this) {
					previousQuestion.querySelector('h3').focus();
				}
			}

			if (currentQuestion === lastQuestion) {
				hideNode(finishButton);
				showNode(nextButton);
			}

			questionCounter--;
			updateProgress();
		});


		nextButton.addEventListener('click', function() {
			var currentQuestion = questions[questionCounter];
			var nextQuestion = questions[questionCounter + 1];

			hideNode(currentQuestion);
			showNode(nextQuestion);

			if (nextQuestion === lastQuestion) {
				hideNode(nextButton);
				showNode(finishButton);
			}
			
			if (document.activeElement === this) {
				nextQuestion.querySelector('h3').focus();
			}
			
			questionCounter++;
			updateProgress();
		});


		finishButton.addEventListener('click', function() {
			endQuestioning();
		});


		quest.querySelectorAll('button[role="radio"]').forEach(function(button) {
			button.addEventListener('mousedown', function(evt) {
				evt.preventDefault();
			});
			button.addEventListener('click', function() {
				selectRadioButton(button);
			});
			button.addEventListener('keydown', function(evt) {
				if (evt.key === 'Return' || evt.key === ' ' || evt.key === 'Spacebar') {
					evt.preventDefault();
					selectRadioButton(button);
				}	
			});
		});

		// Make sure number input is always set to integer value between 0 and 100
		quest.querySelectorAll('.slider-container input[type=number]').forEach(function(numberInput) {
			numberInput.addEventListener('change', function() {
				var correctedValue = Math.round(parseFloat(numberInput.value));
				if (correctedValue < 0) { correctedValue = 0; }
				else if (correctedValue > 100) { correctedValue = 100; }
				numberInput.value = correctedValue;
			});
		});

		quest.querySelectorAll('.slider-single .slider-container').forEach(function(container) {
			var sliderInput = getSliderInput(container);
			var numberInput = getNumberInput(container);
			sliderInput.addEventListener('input', function() {	
				numberInput.value = sliderInput.value;
			});
			numberInput.addEventListener('change', function() {
				sliderInput.value = numberInput.value;
			});
		});


		var getNewValues = (function() {
			var getWeights = function(values) {
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
		
			var fixValues = function(values) {
				var a = values[0];
				var b = values[1];
				if (a === b && ((a*2) % 2) && ((b*2) % 2)) {
					a = Math.floor(a);
					b = Math.ceil(b);
				}
				else {
					a = Math.round(a);
					b = Math.round(b);
				}
				return [a,b];
			};

			return function(oldValues, amountAvailable) {
				var weights = getWeights(oldValues);
				var newValues = weights.map(function(w) { return w * amountAvailable; });
				return fixValues(newValues);
			};
		})();

		var setSliderGroupValues = function(containers, value, index) {
			var values;
			if (index === 0) {
				var oldValue1 = parseInt(getNumberInput(containers[1]).value);
				var oldValue2 = parseInt(getNumberInput(containers[2]).value);
				var oldValues = [oldValue1, oldValue2];
				values = [value].concat(getNewValues(oldValues, 100 - value));
			}
			else {
				var oldValue0 = parseInt(getNumberInput(containers[0]).value);
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
			containers.forEach(function(container, i) {
				setSliderValue(container, values[i]);
			});
		};

		quest.querySelectorAll('.slider-group').forEach(function(sliderSet) {
			var sliderContainers = sliderSet.querySelectorAll('.slider-container');
			sliderContainers.forEach(function(container, index) {
				var sliderInput = getSliderInput(container);
				var numberInput = getNumberInput(container);
				sliderInput.addEventListener('input', function() {
					setSliderGroupValues(sliderContainers, parseInt(sliderInput.value), index);
				});
				numberInput.addEventListener('change', function() {
					setSliderGroupValues(sliderContainers, parseInt(numberInput.value), index);
				});
			});
		});


		var getResponses = function() {
			/* eslint-disable camelcase */
			// Extract the section names from the data-section properties
			var sections = [];
			quest.querySelectorAll('.question').forEach(function(question) {
				var section = question.dataset.section;
				if (sections.indexOf(section) === -1) {
					sections.push(section);
				}
			});

			// Loop over the sections and add the answers to the response object of the model
			return sections.reduce(function(response, section) {
				var selector = '.question[data-section="' + section + '"]';
				var questions = quest.querySelectorAll(selector);
				if (questions.length === 1) {
					response[section] = extractAnswer(questions[0]);
				}
				else {
					var answers = {};
					for (var i = 0; i < questions.length; i++) {
						answers[i + 1] = extractAnswer(questions[i]);
					}
					response[section] = answers;
				}
				return response;
			}, {});
		};

		var setAnswers = function(valuesObject) {
			var sections = Object.keys(valuesObject);

			sections.forEach(function(section) {
				var selector = '.question' + '[data-section="' + section + '"]';
				var questions = quest.querySelectorAll(selector);
				try {
					if (questions.length === 0) {
						console.warn('Section ' + section + ' not recognised. Ignoring.');
					}
					else if (questions.length === 1) {
						var value = valuesObject[section];
						setAnswer(questions[0], value);
					}
					else {
						var values = valuesObject[section];
						for (var i = 0; i < questions.length; i++) {
							setAnswer(questions[i], values[i+1]);
						}
					}
				}
				catch (e) {
					console.error(e);
				}
			});
		};

		return {
			get name() { return quest.querySelector('#questions-name').innerText; },
			get comments() { return quest.querySelector('#questions-comments').value; },
			start: startQuestioning,
			onEnd: function(callback) { onEnd = callback; return this; },
			onUnstart: function(callback) { onUnstart = callback; return this; },
			getResponses: getResponses,
			setAnswers: function(values) { setAnswers(values); return this; }
		};
	};

	
	window.initQuestions = initQuestions;
})();