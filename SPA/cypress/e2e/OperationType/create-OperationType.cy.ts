describe('Create OperationType', () => {

    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/operation-types/create'); // Adjust the URL as needed
    });

    it('should display the create OperationType form', () => {
        cy.get('.create-operation-type-container').should('be.visible');
        cy.get('h2').contains('Create New Operation Type');
    });

    it('should display required fields', () => {
        cy.get('input[name="name"]').should('have.attr', 'required');
        cy.get('select[name="status"]').should('have.attr', 'required');
        cy.get('input[name="specialization"]').should('have.attr', 'required');
        cy.get('input[name="preparationPhaseDuration"]').should('have.attr', 'required');
        cy.get('input[name="surgeryPhaseDuration"]').should('have.attr', 'required');
        cy.get('input[name="cleaningPhaseDuration"]').should('have.attr', 'required');
    });

    it('should fill out and submit the form', () => {
        cy.intercept('POST', '/api/OperationType/Create', {
            statusCode: 201,
            body: { message: 'OperationType successfully created!' }
        }).as('createOperationType');

        cy.get('input[name="name"]').type('New Operation Type');
        cy.get('select[name="status"]').select('active');
        cy.get('input[name="specialization"]').type('General Surgery');
        cy.get('input[name="preparationPhaseDuration"]').type('30');
        cy.get('input[name="surgeryPhaseDuration"]').type('60');
        cy.get('input[name="cleaningPhaseDuration"]').type('15');

        // Add required staff for each phase
        cy.get('#addPreparationStaff').click();
        cy.get('input[name="preparationStaff0Num"]').type('2');
        cy.get('input[name="preparationStaff0Specialization"]').type('Nurse');

        cy.get('#addSurgeryStaff').click();
        cy.get('input[name="surgeryStaff0Num"]').type('1');
        cy.get('input[name="surgeryStaff0Specialization"]').type('Surgeon');

        cy.get('#addCleaningStaff').click();
        cy.get('input[name="cleaningStaff0Num"]').type('1');
        cy.get('input[name="cleaningStaff0Specialization"]').type('Cleaner');

        cy.get('.btn-submit').click();

        cy.wait('@createOperationType');
        cy.get('.success-message').should('contain', 'OperationType successfully created!');
    });

    it('should display an error message if form submission fails', () => {
        cy.intercept('POST', '/api/OperationType/Create', {
            statusCode: 500,
            body: { message: 'Failed to create OperationType' }
        }).as('createOperationTypeError');

        cy.get('input[name="name"]').type('New Operation Type');
        cy.get('select[name="status"]').select('active');
        cy.get('input[name="specialization"]').type('General Surgery');
        cy.get('input[name="preparationPhaseDuration"]').type('30');
        cy.get('input[name="surgeryPhaseDuration"]').type('60');
        cy.get('input[name="cleaningPhaseDuration"]').type('15');

        // Add required staff for each phase
        cy.get('#addPreparationStaff').click();
        cy.get('input[name="preparationStaff0Num"]').type('2');
        cy.get('input[name="preparationStaff0Specialization"]').type('Nurse');

        cy.get('#addSurgeryStaff').click();
        cy.get('input[name="surgeryStaff0Num"]').type('1');
        cy.get('input[name="surgeryStaff0Specialization"]').type('Surgeon');

        cy.get('#addCleaningStaff').click();
        cy.get('input[name="cleaningStaff0Num"]').type('1');
        cy.get('input[name="cleaningStaff0Specialization"]').type('Cleaner');

        cy.get('.btn-submit').click();

        cy.wait('@createOperationTypeError');
        cy.get('.error-message').should('contain', 'Failed to create OperationType');
    });

    it('should navigate to /operation-types on cancel', () => {
        cy.get('.btn-cancel').click();
        cy.url().should('include', '/operation-types');
    });
});