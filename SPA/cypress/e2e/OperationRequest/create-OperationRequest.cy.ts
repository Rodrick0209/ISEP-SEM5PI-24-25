describe('Create Operation Request', () => {
    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/operation-requests'); // Adjust the URL as needed
    });

    it('should display the create operation request form', () => {
        cy.get('button').contains('Create Operation Request').click();
        cy.get('form').should('be.visible');
    });

    it('should create a new operation request successfully', () => {
        cy.intercept('POST', '/api/OperationRequest', { statusCode: 201 }).as('createOperationRequest');
        
        cy.get('button').contains('Create Operation Request').click();
        
        // Fill out the form
        cy.get('input[name="operationName"]').type('Appendectomy');
        cy.get('select[name="operationType"]').select('Emergency');
        cy.get('input[name="patientName"]').type('John Doe');
        cy.get('input[name="scheduledDate"]').type('2023-12-01');
        cy.get('textarea[name="notes"]').type('Patient requires immediate attention.');

        // Submit the form
        cy.get('button').contains('Submit').click();
        
        // Wait for the request and check the response
        cy.wait('@createOperationRequest');
        cy.get('.success-message').should('contain', 'Operation request created successfully!');
    });

    it('should display an error message if creation fails', () => {
        cy.intercept('POST', '/api/OperationRequest', { statusCode: 500, body: { message: 'Internal Server Error' } }).as('createOperationRequestError');
        
        cy.get('button').contains('Create Operation Request').click();
        
        // Fill out the form
        cy.get('input[name="operationName"]').type('Appendectomy');
        cy.get('select[name="operationType"]').select('Emergency');
        cy.get('input[name="patientName"]').type('John Doe');
        cy.get('input[name="scheduledDate"]').type('2023-12-01');
        cy.get('textarea[name="notes"]').type('Patient requires immediate attention.');

        // Submit the form
        cy.get('button').contains('Submit').click();
        
        // Wait for the request and check the response
        cy.wait('@createOperationRequestError');
        cy.get('.error-message').should('contain', 'Failed to create operation request: Internal Server Error');
    });

    it('should cancel the creation process', () => {
        cy.get('button').contains('Create Operation Request').click();
        cy.get('button').contains('Cancel').click();
        cy.get('form').should('not.exist');
    });
});