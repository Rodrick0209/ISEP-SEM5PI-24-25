describe('Delete Patient', () => {
    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/patient/delete/202410000001'); // Adjust the URL as needed
    });

    it('should display the delete confirmation message', () => {
        cy.get('.delete-patient h3').should('contain', 'Are you sure you want to delete this patient profile?');
    });

    it('should disable the delete button initially', () => {
        cy.get('.delete-button').should('be.disabled');
    });

    it('should enable the delete button when confirmed', () => {
        cy.get('app-mark-x').click();
        cy.get('.delete-button').should('not.be.disabled');
    });

    it('should show an error message if delete is attempted without confirmation', () => {
        cy.get('.delete-button').click();
        cy.get('.error-message').should('contain', 'Please confirm the deletion');
    });

    it('should delete the patient and navigate to patients list on successful deletion', () => {
        cy.intercept('DELETE', '/api/patients/*', { statusCode: 204 }).as('deletePatient');
        cy.get('app-mark-x').click();
        cy.get('.delete-button').click();
        cy.wait('@deletePatient');
        cy.url().should('include', '/patients');
        cy.get('.message').should('contain', 'Patient nº');
    });

    it('should navigate to patients list on cancel', () => {
        cy.get('.cancel-button').click();
        cy.url().should('include', '/patients');
    });
});