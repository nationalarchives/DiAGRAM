(function() {
	'use strict';
		
	window.defaultAdvanced = {
		Integrity: {
			System_Security: [
				'Good',
				'Good',
				'Good',
				'Good',
				'Good',
				'Good',
				'Poor',
				'Poor',
				'Poor',
				'Poor',
				'Poor',
				'Poor'
			],
			Info_Management: [
				'Sufficient',
				'Sufficient',
				'Sufficient',
				'Insufficient',
				'Insufficient',
				'Insufficient',
				'Sufficient',
				'Sufficient',
				'Sufficient',
				'Insufficient',
				'Insufficient',
				'Insufficient'
			],
			Checksum: [
				'Yes',
				'Self Generated',
				'No',
				'Yes',
				'Self Generated',
				'No',
				'Yes',
				'Self Generated',
				'No',
				'Yes',
				'Self Generated',
				'No'
			],
			Yes: [
				1,
				0.9099,
				0,
				0,
				0,
				0,
				0.5326,
				0.4424,
				0,
				0,
				0,
				0
			],
			No: [
				0,
				0.0901,
				1,
				1,
				1,
				1,
				0.4674,
				0.5576,
				1,
				1,
				1,
				1
			]
		},
		Info_Management: {
			Sufficient: 0.55,
			Insufficient: 0.45
		},
		Storage_Medium: {
			A: 0.5,
			B: 0.45,
			C: 0.05
		},
		Digital_Object: {
			Born_digital: 0.34,
			Digitised: 0.1,
			Surrogate: 0.56
		},
		File_Format: {
			Digital_Object: [
				'Born digital',
				'Digitised',
				'Surrogate'
			],
			Yes: [
				0.8589,
				0.9999,
				0.9999
			],
			No: [
				0.1411,
				0.0001,
				0.0001
			]
		},
		Op_Environment: {
			Yes: 0.54,
			No: 0.46
		},
		Renderability: {
			Tech_Metadata: [
				'Sufficient',
				'Sufficient',
				'Sufficient',
				'Sufficient',
				'Insufficient',
				'Insufficient',
				'Insufficient',
				'Insufficient'
			],
			Tools_to_Render: [
				'Yes',
				'Yes',
				'No',
				'No',
				'Yes',
				'Yes',
				'No',
				'No'
			],
			Bit_Preservation: [
				'Yes',
				'No',
				'Yes',
				'No',
				'Yes',
				'No',
				'Yes',
				'No'
			],
			Yes: [
				1,
				0,
				0,
				0,
				0.5993,
				0,
				0,
				0
			],
			No: [
				0,
				1,
				1,
				1,
				0.4007,
				1,
				1,
				1
			]
		},
		Conditions_of_Use: {
			Digital_Object: [
				'Born digital',
				'Digitised',
				'Surrogate'
			],
			Yes: [
				0.611,
				0.745,
				0.7896
			],
			No: [
				0.389,
				0.255,
				0.2104
			]
		},
		Checksum: {
			Yes: 0.05,
			Self_Generated: 0.5,
			No: 0.45
		},
		System_Security: {
			Good: 0.17,
			Poor: 0.83
		},
		Physical_Disaster: {
			Yes: 0.02,
			No: 0.98
		},
		Rep_and_Refresh: {
			Good: 0.65,
			Poor: 0.35
		},
		Tech_Metadata: {
			Technical_Skills: [
				'Good',
				'Poor'
			],
			Sufficient: [
				0.7729,
				0.427
			],
			Insufficient: [
				0.2271,
				0.573
			]
		},
		Identity: {
			Info_Management: [
				'Sufficient',
				'Sufficient',
				'Insufficient',
				'Insufficient'
			],
			Content_Metadata: [
				'Yes',
				'No',
				'Yes',
				'No'
			],
			Yes: [
				1,
				0.5348,
				0,
				0
			],
			No: [
				0,
				0.4652,
				1,
				1
			]
		},
		Technical_Skills: {
			Good: 0.15,
			Poor: 0.85
		},
		Content_Metadata: {
			Digital_Object: [
				'Born digital',
				'Digitised',
				'Surrogate'
			],
			Yes: [
				0.2,
				0.7492,
				0.7261
			],
			No: [
				0.8,
				0.2508,
				0.2739
			]
		},
		Obsolescence: {
			Storage_Medium: [
				'A',
				'A',
				'B',
				'B',
				'C',
				'C'
			],
			Technical_Skills: [
				'Good',
				'Poor',
				'Good',
				'Poor',
				'Good',
				'Poor'
			],
			Yes: [
				0.2985,
				0.6422,
				0.1405,
				0.419,
				0.001,
				0.001
			],
			No: [
				0.7015,
				0.3578,
				0.8595,
				0.581,
				0.999,
				0.999
			]
		},
		Intellectual_Control: {
			Conditions_of_Use: [
				'Yes',
				'Yes',
				'No',
				'No'
			],
			Identity: [
				'Yes',
				'No',
				'Yes',
				'No'
			],
			Yes: [
				1,
				0,
				0,
				0
			],
			No: [
				0,
				1,
				1,
				1
			]
		},
		Bit_Preservation: {
			Integrity: [
				'Yes',
				'Yes',
				'Yes',
				'Yes',
				'No',
				'No',
				'No',
				'No'
			],
			Obsolescence: [
				'Yes',
				'Yes',
				'No',
				'No',
				'Yes',
				'Yes',
				'No',
				'No'
			],
			Storage_Life: [
				'Yes',
				'No',
				'Yes',
				'No',
				'Yes',
				'No',
				'Yes',
				'No'
			],
			Yes: [
				0,
				0,
				1,
				0,
				0,
				0,
				0.7158,
				0
			],
			No: [
				1,
				1,
				0,
				1,
				1,
				1,
				0.2842,
				1
			]
		},
		Tools_to_Render: {
			File_Format: [
				'Yes',
				'Yes',
				'No',
				'No'
			],
			Technical_Skills: [
				'Good',
				'Poor',
				'Good',
				'Poor'
			],
			Yes: [
				0.8111,
				0.8111,
				0.4343,
				0
			],
			No: [
				0.1889,
				0.1889,
				0.5657,
				1
			]
		},
		Storage_Life: {
			Op_Environment: [
				'Yes',
				'Yes',
				'Yes',
				'Yes',
				'Yes',
				'Yes',
				'Yes',
				'Yes',
				'Yes',
				'Yes',
				'Yes',
				'Yes',
				'No',
				'No',
				'No',
				'No',
				'No',
				'No',
				'No',
				'No',
				'No',
				'No',
				'No',
				'No'
			],
			Storage_Medium: [
				'A',
				'A',
				'A',
				'A',
				'B',
				'B',
				'B',
				'B',
				'C',
				'C',
				'C',
				'C',
				'A',
				'A',
				'A',
				'A',
				'B',
				'B',
				'B',
				'B',
				'C',
				'C',
				'C',
				'C'
			],
			Rep_and_Refresh: [
				'Good',
				'Good',
				'Poor',
				'Poor',
				'Good',
				'Good',
				'Poor',
				'Poor',
				'Good',
				'Good',
				'Poor',
				'Poor',
				'Good',
				'Good',
				'Poor',
				'Poor',
				'Good',
				'Good',
				'Poor',
				'Poor',
				'Good',
				'Good',
				'Poor',
				'Poor'
			],
			Physical_Disaster: [
				'Yes',
				'No',
				'Yes',
				'No',
				'Yes',
				'No',
				'Yes',
				'No',
				'Yes',
				'No',
				'Yes',
				'No',
				'Yes',
				'No',
				'Yes',
				'No',
				'Yes',
				'No',
				'Yes',
				'No',
				'Yes',
				'No',
				'Yes',
				'No'
			],
			Yes: [
				1,
				1,
				0.7588,
				0.7588,
				1,
				1,
				0.945,
				0.945,
				1,
				1,
				1,
				1,
				0.5393,
				1,
				0.3212,
				0.7588,
				0.8747,
				1,
				0.646,
				0.945,
				1,
				1,
				1,
				1
			],
			No: [
				0,
				0,
				0.2412,
				0.2412,
				0,
				0,
				0.055,
				0.055,
				0,
				0,
				0,
				0,
				0.4607,
				0,
				0.6788,
				0.2412,
				0.1253,
				0,
				0.354,
				0.055,
				0,
				0,
				0,
				0
			]
		}
	};
})();