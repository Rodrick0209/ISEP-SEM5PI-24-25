describe('Delete Staff', () => {
    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/staff/delete/12345'); // Ajuste o ID do staff e a rota conforme necessário
    });

    it('should display the delete confirmation message', () => {
        cy.get('.delete-staff h3').should('contain', 'Are you sure you want to deactivate this staff profile?');
    });

    it('should display the note about deactivation', () => {
        cy.get('.delete-staff p').should('contain', 'Note: This profile will be deactivated but retained for audit purposes.');
    });

    it('should deactivate the staff when the delete button is clicked', () => {
        cy.intercept('DELETE', '/api/staff/12345', { statusCode: 200 }).as('deleteStaff');
        cy.get('.delete-button').click();
        cy.wait('@deleteStaff');
        cy.url().should('include', '/staffs');
        cy.get('.success-message').should('contain', 'Staff 12345 successfully deactivated!');
    });

    it('should navigate to staff list when cancel button is clicked', () => {
        cy.get('.cancel-button').click();
        cy.url().should('include', '/staffs');
    });

    it('should display an error message if deactivation fails', () => {
        cy.intercept('DELETE', '/api/staff/12345', { statusCode: 500, body: { message: 'Internal Server Error' } }).as('deleteStaffError');
        cy.get('.delete-button').click();
        cy.wait('@deleteStaffError');
        cy.get('.error-message').should('contain', 'Failed to deactivate staff');
    });

    it('should display a confirmation error message if mark is not clicked', () => {
        cy.get('.delete-button').click();
        cy.get('.error-message').should('contain', 'Please confirm the deactivation');
    });

    it('should allow toggling the confirmation mark', () => {
        cy.get('.mark-x').click(); // Supondo que o botão ou elemento tem a classe `mark-x`
        cy.get('.mark-x').should('have.class', 'confirmed'); // Verifica o estado confirmado (ajuste conforme a implementação)
        cy.get('.mark-x').click();
        cy.get('.mark-x').should('not.have.class', 'confirmed');
    });
});
