describe('Deactivate OperationType', () => {
    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/operation-types'); // Adjust the URL as needed
    });

    it('should display the deactivate confirmation message', () => {
        cy.get('tbody tr').first().find('button').contains('Deactivate').click();
        cy.get('.modal-content h3').should('contain', 'Confirm Deactivation');
    });

    it('should deactivate the OperationType when the deactivate button is clicked', () => {
        cy.intercept('POST', '/api/OperationType/*', { statusCode: 200 }).as('deactivateOperationType');
        cy.get('tbody tr').first().find('button').contains('Deactivate').click();
        cy.get('.modal-actions .btn-confirm').click();
        cy.wait('@deactivateOperationType');
        cy.get('.success-message').should('contain', 'OperationType successfully deactivated!');
    });

    it('should display an error message if deactivation fails', () => {
        cy.intercept('POST', '/api/OperationType/*', { statusCode: 500, body: { message: 'Internal Server Error' } }).as('deactivateOperationTypeError');
        cy.get('tbody tr').first().find('button').contains('Deactivate').click();
        cy.get('.modal-actions .btn-confirm').click();
        cy.wait('@deactivateOperationTypeError');
        cy.get('.error-message').should('contain', 'Failed to deactivate OperationType: Internal Server Error');
    });

    it('should cancel the deactivation process', () => {
        cy.get('tbody tr').first().find('button').contains('Deactivate').click();
        cy.get('.modal-actions .btn-cancel').click();
        cy.get('.modal').should('not.exist');
    });
});