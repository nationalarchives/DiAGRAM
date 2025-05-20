describe('DiAGRAM model creation spec', () => {

    beforeEach(() => {
        cy.visit('/model.html')
    })

    it('should have all sections needed for model creation', () => {
        cy.get('.box-body #questions-section #questions .question .question-header h3').then($sectionHeaders => {
            expect($sectionHeaders[0]).to.contain.text('Digital Object');
            expect($sectionHeaders[1]).to.contain.text('Storage Medium');
            expect($sectionHeaders[2]).to.contain.text('Replication and Refreshment');
            expect($sectionHeaders[3]).to.contain.text('Replication and Refreshment');
            expect($sectionHeaders[4]).to.contain.text('Operating Environment');
            expect($sectionHeaders[5]).to.contain.text('Operating Environment');
            expect($sectionHeaders[6]).to.contain.text('Physical Disaster');
            expect($sectionHeaders[7]).to.contain.text('Checksum');
            expect($sectionHeaders[8]).to.contain.text('System Security');
            expect($sectionHeaders[9]).to.contain.text('System Security');
            expect($sectionHeaders[10]).to.contain.text('System Security');
            expect($sectionHeaders[11]).to.contain.text('System Security');
            expect($sectionHeaders[12]).to.contain.text('Information Management');
            expect($sectionHeaders[13]).to.contain.text('Information Management');
            expect($sectionHeaders[14]).to.contain.text('Information Management');
            expect($sectionHeaders[15]).to.contain.text('Technical Skills');
        });
    })

    it('should click through the model creation pages with default values and produce a model at the end', () => {
        cy.get('#models-section-container').should('be.not.visible')
        cy.get('.box-body #model-start').click({force: true});

        // Name container, give name to the model
        cy.get('.box-body #name-container #model-name').should('be.visible')
        cy.get('.box-body #name-container #model-name').type('Test Model')
        cy.get('.box-body #prequestions #name-container .button-row .btn-next').should('be.visible')
        cy.get('.box-body #prequestions #name-container .button-row .btn-next').click({force: true})

        // Name captured in the questions-ancillary section
        cy.get('.box-body #questions-section #questions-ancillary .questions-ancillary-top-row .questions-name-container p span').then($modelName => {
            expect($modelName[0]).to.contain.text('Test Model');
        });

        // Model type container
        assertHiddenAndVisibleToggled('sm-q1', 'do-q1');
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-next').click({force: true})

        // Hide Model type and make Storage Medium visible
        assertHiddenAndVisibleToggled('do-q1', 'sm-q1');
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-next').click({force: true})

        // Hide Storage Medium and make Replication and refreshment 1 visible
        assertHiddenAndVisibleToggled('sm-q1', 'rr-q1');
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-next').click({force: true})

        // Hide Replication and refreshment 1 and make Replication and refreshment 2 visible
        assertHiddenAndVisibleToggled('rr-q1', 'rr-q2');
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-next').click({force: true})

        // Hide Replication and refreshment 2 and make Operating Environment 1 visible
        assertHiddenAndVisibleToggled('rr-q2', 'oe-q1');
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-next').click({force: true})

        // Hide Operating Environment 1 and make Operating Environment 2 visible
        assertHiddenAndVisibleToggled('oe-q1', 'oe-q2');
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-next').click({force: true})

        // Hide Operating Environment 2 and make Physical Disaster visible
        assertHiddenAndVisibleToggled('oe-q2', 'pd-q1');
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-next').click({force: true})

        // Hide Physical Disaster and make Checksum visible
        assertHiddenAndVisibleToggled('pd-q1', 'c-q1');
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-next').click({force: true})

        // Hide Checksum and make System Security 1 visible
        assertHiddenAndVisibleToggled('c-q1', 'ss-q1');
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-next').click({force: true})

        // Hide System Security 1 and make System Security 2 visible
        assertHiddenAndVisibleToggled('ss-q1', 'ss-q2');
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-next').click({force: true})

        // Hide System Security 2 and make System Security 3 visible
        assertHiddenAndVisibleToggled('ss-q2', 'ss-q3');
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-next').click({force: true})

        // Hide System Security 3 and make System Security 4 visible
        assertHiddenAndVisibleToggled('ss-q3', 'ss-q4');
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-next').click({force: true})

        // Hide System Security 4 and make Information Management 1 visible
        assertHiddenAndVisibleToggled('ss-q4', 'im-q1');
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-next').click({force: true})

        // Hide Information Management 1 and make Information Management 2 visible
        assertHiddenAndVisibleToggled('im-q1', 'im-q2');
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-next').click({force: true})

        // Hide Information Management 2 and make Information Management 3 visible
        cy.get(`.box-body #questions-section #questions .question .question-content .question-text #im-q2`)
            .parent().parent().parent().should('have.class', 'question hidden')
        cy.get(`.box-body #questions-section #questions .question .question-content .question-text #im-q3a`).should('be.visible')
        cy.get(`.box-body #questions-section #questions .question .question-content .question-text #im-q3a`)
            .parent().parent().parent().parent().should('have.class', 'question')
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-next').click({force: true})

        // Hide Information Management 3 and make Technical Skills visible
        cy.get(`.box-body #questions-section #questions .question .question-content .question-text #im-q3a`)
            .parent().parent().parent().parent().should('have.class', 'question hidden')
        cy.get(`.box-body #questions-section #questions .question .question-content .question-text #ts-q1a`).should('be.visible')
        cy.get(`.box-body #questions-section #questions .question .question-content .question-text #ts-q1a`)
            .parent().parent().parent().parent().should('have.class', 'question')

        //Finish button visible, click i
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-finish').should('be.visible')
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-finish').click({force: true})
        cy.get('#models-section-container').should('be.visible')

    })

    function assertHiddenAndVisibleToggled(toHide, toShow) {
        cy.get(`.box-body #questions-section #questions .question .question-content .question-text #${toHide}`)
            .parent().parent().parent().should('have.class', 'question hidden')
        cy.get(`.box-body #questions-section #questions .question .question-content .question-text #${toShow}`).should('be.visible')
        cy.get(`.box-body #questions-section #questions .question .question-content .question-text #${toShow}`)
            .parent().parent().parent().should('have.class', 'question')
    }
})
