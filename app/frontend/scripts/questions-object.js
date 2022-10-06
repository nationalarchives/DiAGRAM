(function() {
	'use strict';

	/* eslint-disable camelcase */
	window.questions = {
		question1: {
			node: 'Digital_Object',
			text: 'What proportion of your digital archive are the following?',
			detail: [
				'Records were created in a digital format.',
				'Records have been created as a result of converting analogue originals, but you do not hold those originals.',
				'Digital images have been created as a result of converting analogue originals, and you also hold the originals.'
			]
		},
		question2: {
			node: 'Storage_Medium',
			text: 'What proportion of your records are stored on the following media types?',
			detail: [
				'Expected lifespan below 10 years or unknown, highly susceptible to physical damage, requires specific environmental conditions and very sensitive to changes, does not support error-detection methods, supporting technology is novel, proprietary and limited. Examples include USB flash drives (memory sticks), floppy disks, SD drives and CD-R discs.',
				'A proven lifespan of at least 10 years, low susceptibility to physical damage, tolerant of a wide range of environmental conditions without data loss, supports robust error-detection methods, supporting technology is well established and widely available. Examples include LTO tapes, Blu-ray discs, enterprise/corporate managed hard drives and CD-ROM discs.',
				'An external company is responsible for our digital storage. Examples include Amazon Simple Storage Service, Microsoft Azure Archive Storage and Google Cloud Storage.'
			]
		},
		question3_1: {
			node: 'Rep_and_Refresh',
			part: 1,
			text: 'What percentage of your material do you have at least one additional copy of?'
		},
		question3_2: {
			node: 'Rep_and_Refresh',
			part: 2,
			text: 'For those files with an additional copy, do you ensure you always have at least 2 independent copies?'
		},
		question4_1: {
			node: 'Op_Environment',
			part: 1,
			text: 'What percentage of your digital material has a copy kept offsite?',
		},
		question4_2: {
			node: 'Op_Environment',
			part: 2,
			text: 'If all of your digital material is in one location, is there adequate protection against damage from a flood?',
		},
		question5: {
			node: 'Physical_Disaster',
			text: 'Based on the Government\'s long term flood risk assessment, how likely is it that your safest digital storage location will experience a flood?',
		},
		question6: {
			node: 'Checksum',
			text: 'For what proportion of files do you have a checksum from following sources?',
			detail: [
				'The depositor',
				'The archivist, generated on receipt of the record but prior to accessioning',
				'You don\'t have checksums at all, or they were generated sometime after initial receipt'
			]
		},
		question7_1: {
			node: 'System_Security',
			part: 1,
			text: 'Does your organisation hold a recognised security accreditation such as Cyber Essentials or ISO 27001 (or has it carried out equivalent assessments)?',
		},
		question7_2: {
			node: 'System_Security',
			part: 2,
			text: 'Have your archival systems had a penetration test? If yes, are any issues outstanding?',
		},
		question7_3: {
			node: 'System_Security',
			part: 3,
			definition: 'A secure system can protect data from deletion or modification from any unauthorized party, and it ensures that when an authorized person makes a change that should not have been made, the damage can be reversed. Definition from Forcepoint.',
			text: 'Referring to the NDSA Levels of Preservation, what level is your archive for the Control functional area?',
		},
		question7_4: {
			node: 'System_Security',
			part: 4,
			definition: 'A secure system can protect data from deletion or modification from any unauthorized party, and it ensures that when an authorized person makes a change that should not have been made, the damage can be reversed. Definition from Forcepoint.',
			text: 'Is all of your digital material virus checked and the result recorded?',
		},
		question8_1: {
			node: 'Info_Management',
			part: 1,
			text: 'Referring to the NDSA Levels of Preservation, what level is your archive for the Metadata functional area',
		},
		question8_2: {
			node: 'Info_Management',
			part: 2,
			text: 'Referring to the NDSA Levels of Preservation, what level is your archive for the Content functional area?',
		},
		question8_3: {
			node: 'Info_Management',
			part: 3,
			definition: 'Internal systems and support for coherent information management and documentation of preservation actions. This is needed to ensure integrity and provenance of the digital object.',
			text: [
				'Referring to DPC RAM, what level is your archive for Service capability, I - Content preservation',
				'Referring to DPC RAM, what level is your archive for Service capability, J - Metadata management'
			],
		},
		question9: {
			node: 'Technical_Skills',
			text: 'The following statements on digital preservation skills are from the DigCurV curriculum framework for digital curation. For each, rate the level of skill in your organisation.',
			detail: [
				'KIA 1.9 Apply appropriate technological solutions',
				'KIA 1.12 Digital preservation standards',
				'KIA 1.15 Information technology definitions and skills',
				'KIA 1.16 Select and apply digital curation and preservation techniques',
				'KIA 3.4 Continuously monitor and evaluate digital curation technologies',
				'KIA 5.1 Data structures and types',
				'KIA 5.2 File types, applications and systems',
				'KIA 5.3 Database types and structures',
				'KIA 5.4 Execute analysis of and forensic procedures in digital curation',
				'PQ 3.9 Translate current digital curation knowledge into new services and tools'
			]
		}
	};
	/* eslint-enable camelcase */
})();