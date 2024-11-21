describe('Delete Patient', () => {
    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/patient/delete/202410000001'); // Adjust the URL as needed
    });

    it('should display the delete confirmation message', () => {
        cy.get('.delete-patient h3').should('contain', 'Are you sure you want to delete this patient profile?');
    });

    it('should display the note about data retention', () => {
        cy.get('.delete-patient p').should('contain', 'Note: This profile will be permanently deleted in 30 days, and some data will be retained for analysis purposes.');
    });

    it('should delete the patient when the delete button is clicked', () => {
        cy.intercept('DELETE', '/api/patients/202410000001', { statusCode: 200 }).as('deletePatient');
        cy.get('.delete-button').click();
        cy.wait('@deletePatient');
        cy.url().should('include', '/patients');
        cy.get('.success-message').should('contain', 'Patient nº 202410000001 successfully deleted!');
    });

    it('should navigate to patients list when cancel button is clicked', () => {
        cy.get('.cancel-button').click();
        cy.url().should('include', '/patients');
    });

    it('should display an error message if deletion fails', () => {
        cy.intercept('DELETE', '/api/patients/202410000001', { statusCode: 500, body: { message: 'Internal Server Error' } }).as('deletePatientError');
        cy.get('.delete-button').click();
        cy.wait('@deletePatientError');
        cy.get('.error-message').should('contain', 'Failed to delete patient: Internal Server Error');
    });
});