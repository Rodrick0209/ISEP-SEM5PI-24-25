describe('Edit Operation Request', () => {
    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/operation-requests'); // Adjust the URL as needed
    });

    it('should display the edit operation request form', () => {
        cy.get('tbody tr').first().find('button').contains('Edit').click();
        cy.get('form').should('be.visible');
    });

    it('should edit the operation request successfully', () => {
        cy.intercept('PUT', '/api/OperationRequest/*', { statusCode: 200 }).as('editOperationRequest');
        
        cy.get('tbody tr').first().find('button').contains('Edit').click();
        
        // Fill out the form with new data
        cy.get('input[name="operationName"]').clear().type('Appendectomy Updated');
        cy.get('select[name="operationType"]').select('Scheduled');
        cy.get('input[name="patientName"]').clear().type('Jane Doe');
        cy.get('input[name="scheduledDate"]').clear().type('2023-12-02');
        cy.get('textarea[name="notes"]').clear().type('Updated notes for the operation.');

        // Submit the form
        cy.get('button').contains('Submit').click();
        
        // Wait for the request and check the response
        cy.wait('@editOperationRequest');
        cy.get('.success-message').should('contain', 'Operation request updated successfully!');
    });

    it('should display an error message if editing fails', () => {
        cy.intercept('PUT', '/api/OperationRequest/*', { statusCode: 500, body: { message: 'Internal Server Error' } }).as('editOperationRequestError');
        
        cy.get('tbody tr').first().find('button').contains('Edit').click();
        
        // Fill out the form with new data
        cy.get('input[name="operationName"]').clear().type('Appendectomy Updated');
        cy.get('select[name="operationType"]').select('Scheduled');
        cy.get('input[name="patientName"]').clear().type('Jane Doe');
        cy.get('input[name="scheduledDate"]').clear().type('2023-12-02');
        cy.get('textarea[name="notes"]').clear().type('Updated notes for the operation.');

        // Submit the form
        cy.get('button').contains('Submit').click();
        
        // Wait for the request and check the response
        cy.wait('@editOperationRequestError');
        cy.get('.error-message').should('contain', 'Failed to update operation request: Internal Server Error');
    });

    it('should cancel the editing process', () => {
        cy.get('tbody tr').first().find('button').contains('Edit').click();
        cy.get('button').contains('Cancel').click();
        cy.get('form').should('not.exist');
    });
});