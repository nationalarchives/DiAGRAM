describe('DiAGRAM scenario creation spec', () => {
    beforeEach(() => {
        cy.visit('/scenario.html')
    })

    it('should tell user to create a model first when model does not exist', () => {
        cy.get('.box-body #no-models').should('be.visible')
    })

    it('should click through the scenario creation pages with default model and produce a scenario', () => {
        createDefaultModel('Model 1')
        cy.get('#models-section #model-list', {timeout: 5000}).should('be.visible')
        cy.get('.content #sidebar-menu ul li a[data-page="scenario"]').click({force: true})
        cy.get('#choose-model-container', {timeout: 5000}).should('be.visible')
        cy.get('.box-body #prequestions #choose-model-container .button-row .btn-next').should('be.visible')
        cy.get('.box-body #prequestions #choose-model-container .button-row .btn-next').click({force: true})

        cy.get('.box-body #prequestions #response-changes .button-row').contains('Create Scenario').should('not.be.visible')
        cy.get('.box-body #prequestions #response-changes table tbody tr').first().find('input[type="checkbox"]').check({force: true})
        cy.get('.box-body #prequestions #response-changes .button-row').contains('Create Scenario').click({force: true})

        cy.get('.box-body #prequestions #name-container .button-row .btn-next').should('not.be.visible')
        cy.get('.box-body #prequestions #name-container #scenario-name').type('Scenario 1')
        cy.get('.box-body #prequestions #name-container .button-row .btn-next').should('be.visible')
        cy.get('.box-body #prequestions #name-container .button-row .btn-next').click({force: true})

        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-finish').should('be.visible')
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-finish').click({force: true})

        cy.get('#postquestions', {timeout: 5000}).should('be.visible')
        cy.get('.box-body #postquestions').contains('Results').click({force: true})
        cy.get('#visualisation-box #image-container').should('be.visible')
    })

    function createDefaultModel(model1) {
        cy.visit('/model.html')
        cy.get('.box-body #model-start').click({force: true});
        cy.get('.box-body #name-container #model-name').type(model1)
        cy.get('.box-body #prequestions #name-container .button-row .btn-next').click({force: true})
        clickNextUntilFinish()
    }

    function clickNextUntilFinish() {
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-next').click({force: true})
        cy.get('.box-body #questions-section #questions-buttons .button-row .btn-finish').then ($btnFinish => {
            if ($btnFinish.is(':visible')) {
                cy.get('.box-body #questions-section #questions-buttons .button-row .btn-finish').click({force: true})
            } else {
                clickNextUntilFinish()
            }
        })
    }
})