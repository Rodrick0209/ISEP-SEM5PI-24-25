describe('Delete Operation Request', () => {
    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/operation-requests'); // Adjust the URL as needed
    });

    it('should display the delete confirmation message', () => {
        cy.get('tbody tr').first().find('button').contains('Delete').click();
        cy.get('.modal-content h3').should('contain', 'Confirm Deletion');
    });

    it('should delete the Operation Request when the delete button is clicked', () => {
        cy.intercept('DELETE', '/api/OperationRequest/*', { statusCode: 200 }).as('deleteOperationRequest');
        cy.get('tbody tr').first().find('button').contains('Delete').click();
        cy.get('.modal-actions .btn-confirm').click();
        cy.wait('@deleteOperationRequest');
        cy.get('.success-message').should('contain', 'Operation request successfully deleted!');
    });

    it('should display an error message if deletion fails', () => {
        cy.intercept('DELETE', '/api/OperationRequest/*', { statusCode: 500, body: { message: 'Internal Server Error' } }).as('deleteOperationRequestError');
        cy.get('tbody tr').first().find('button').contains('Delete').click();
        cy.get('.modal-actions .btn-confirm').click();
        cy.wait('@deleteOperationRequestError');
        cy.get('.error-message').should('contain', 'Failed to delete operation request: Internal Server Error');
    });

    it('should cancel the deletion process', () => {
        cy.get('tbody tr').first().find('button').contains('Delete').click();
        cy.get('.modal-actions .btn-cancel').click();
        cy.get('.modal').should('not.exist');
    });
});