(function () {
	'use strict';

	/* eslint-disable camelcase */
	window.exampleModels = [
		{
			model_name: 'Example - Commercial Backup',
			scenario: 'Base Model',
			notes: 'The archive has no knowledge of digital preservation but outsources their data storage of their born-digital records. They have an inventory of content but nothing more and have not thought about preserving or rendering content, only storing the bit-stream.',
			is_advanced: false,
			intellectual_control: 6,
			renderability: 38,
			response: {
				Digital_Object: [
					100,
					0,
					0
				],
				Storage_Medium: [
					0,
					0,
					100
				],
				Rep_and_Refresh: {
					1: 100,
					2: 100
				},
				Op_Environment: {
					1: 100,
					2: 'Not Applicable - we have copies offsite'
				},
				Physical_Disaster: 'Very Low',
				Checksum: [
					0,
					0,
					100
				],
				System_Security: {
					1: 'ISO 27001',
					2: 'No test',
					3: 'Level 2',
					4: 'No'
				},
				Info_Management: {
					1: 'Level 1',
					2: 'Not achieved',
					3: [
						'Minimal awareness',
						'Awareness'
					]
				},
				Technical_Skills: [
					'None',
					'None',
					'None',
					'None',
					'None',
					'None',
					'None',
					'None',
					'None',
					'None'
				]
			},
			advanced: null
		},
		{
			model_name: 'Example - Established National Archive',
			scenario: 'Base Model',
			notes: 'There is a dedicated digital archive infrastructure to record and manage information about material. Multiple copies of the bit-stream are maintained on and off site on stable media and new copies are automatically made whenever an error is detected. Staff are experts in preservation practices and very active in digital preservation communities. The archive has reasonable control over metadata requirements from depositors.',
			is_advanced: false,
			intellectual_control: 60,
			renderability: 61,
			response: {
				Digital_Object: [
					20,
					0,
					80
				],
				Storage_Medium: [
					0,
					100,
					0
				],
				Rep_and_Refresh: {
					1: 100,
					2: 100
				},
				Op_Environment: {
					1: 100,
					2: 'Not Applicable - we have copies offsite'
				},
				Physical_Disaster: 'Medium',
				Checksum: [
					99,
					1,
					0
				],
				System_Security: {
					1: 'ISO 27001',
					2: 'None, or only minor issues outstanding',
					3: 'Level 3',
					4: 'Yes'
				},
				Info_Management: {
					1: 'Level 3',
					2: 'Level 4',
					3: [
						'Optimized',
						'Optimized'
					]
				},
				Technical_Skills: [
					'Advanced',
					'Advanced',
					'Advanced',
					'Advanced',
					'Advanced',
					'Advanced',
					'Advanced',
					'Advanced',
					'Advanced',
					'Advanced'
			    ]
			},
			advanced: null
		},
	];
})();
