describe('DiAGRAM home page spec', () => {
  beforeEach(() => {
    cy.visit('/')
  })

  it('Should collapse and expand left hand navigation panel when clicked on the toggle navigation chevron', () => {
    //Collapse
    cy.get('.outer-wrapper .content .sidebar').parent().should('have.class', 'content')
    cy.get('.outer-wrapper header #sidebar-toggle').click({force: true})
    cy.get('.outer-wrapper .content .sidebar').parent().should('have.class', 'content sidebar-hidden')

    //Expand
    cy.get('.outer-wrapper header #sidebar-toggle').click({force: true})
    cy.get('.outer-wrapper .content .sidebar').parent().should('have.class', 'content')
  })

  it('Index page loads and has the relevant components', () => {
    cy.get('h3').contains('Version 1.0.0')
    cy.get('li').then($navLinks => {
      expect($navLinks[0]).to.contain.text('Home page');
      expect($navLinks[1]).to.contain.text('How to use the tool');
      expect($navLinks[2]).to.contain.text('Create a model');
      expect($navLinks[3]).to.contain.text('Create a scenario');
      expect($navLinks[4]).to.contain.text('View results');
      expect($navLinks[5]).to.contain.text('Download a report');
      expect($navLinks[6]).to.contain.text('Upload previous models');
      expect($navLinks[7]).to.contain.text('Using the reference models');
      expect($navLinks[8]).to.contain.text('Learn about DiAGRAM');
      expect($navLinks[9]).to.contain.text('Advanced customisation');
      expect($navLinks[10]).to.contain.text('Glossary');
    });
    cy.get('footer p').then($paragraphs => {
      expect($paragraphs[1]).to.contain.text('DiAGRAM is free to use for non-commercial purposes. For re-use of the code see the licence statement on GitHub')
    });
  })

  it('Home page should display the correct details', () => {
    cy.contains('Home page').click({force: true})
    cy.get('.box-body p').then($paragraphs => {
      expect($paragraphs[8]).to.contain.text('DiAGRAM was built by The National Archives and the University of Warwick with support from the National Lottery Heritage Fund and the Engineering and Physical Sciences Research Council.')
    });
  })

  it('How to use the tool should display the correct details', () => {
    cy.contains('How to use the tool').click({force: true})
    cy.get('.box-body h2').then($headers => {
      expect($headers[0]).to.contain.text('How to use the tool');
      expect($headers[1]).to.contain.text('Before you start');
    });
  })

  it('Create a model page should display the correct details', () => {
    cy.contains('Create a model').click({force: true})
    cy.get('.box-body section p').then($preamble => {
      expect($preamble[0]).to.contain.text('By creating a model, you will be able to see ');
    });
  })

  it('Create a scenario page should display the correct details', () => {
    cy.contains('Create a scenario').click({force: true})
    cy.get('.box-body section p').then($preamble => {
      expect($preamble[0]).to.contain.text('By creating a scenario you will be able to change the answers you used to create your model ');
    });
  })

  it('View results page should display the correct details', () => {
    cy.contains('View results').click({force: true})
    cy.get('.box-body h2').then($headers => {
      expect($headers[0]).to.contain.text('View results');
    });
    cy.get('.box-body h3').then($headers => {
      expect($headers[0]).to.contain.text('Select the models and scenarios to visualise');
    });
  })

  it('Download a report page should display the correct details', () => {
    cy.contains('Download a report').click({force: true})
    cy.get('.box-body h2').then($headers => {
      expect($headers[0]).to.contain.text('Download a report');
    });
  })

  it('Upload previous model page should display the correct details', () => {
    cy.contains('Upload previous models').click({force: true})
    cy.get('.box-body h2').then($headers => {
      expect($headers[0]).to.contain.text('Upload previous models');
    });
  })

  it('Using the reference models page should display the correct details', () => {
    cy.contains('Using the reference models').click({force: true})
    cy.get('.box-body h2').then($headers => {
      expect($headers[0]).to.contain.text('Using the reference models');
    });
    cy.get('.box-body h3').then($headers => {
      expect($headers[0]).to.contain.text('How can I use these?');
    });
  })

  it('Learn about DiAGRAM page should display the correct details', () => {
    cy.contains('Learn about DiAGRAM').click({force: true})
    cy.get('.box-body h2').then($headers => {
      expect($headers[0]).to.contain.text('Learn about DiAGRAM');
    });
  })

  it('Advanced customisation page should display the correct details', () => {
    cy.contains('Advanced customisation').click({force: true})
    cy.get('.box-body h2').then($headers => {
      expect($headers[0]).to.contain.text('Advanced customisation');
    });
  })

  it('Glossary page should display the correct details', () => {
    cy.contains('Glossary').click({force: true})
    cy.get('.box-body h1').then($headers => {
      expect($headers[0]).to.contain.text('General terms');
    });
  })
})