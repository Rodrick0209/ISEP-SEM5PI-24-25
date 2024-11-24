describe('Edit Staff', () => {
    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/staff/edit/12345'); // Substitua pelo ID real do staff e pela rota conforme necessário
    });

    it('should display the edit staff form', () => {
        cy.get('.edit-staff-container').should('be.visible');
        cy.get('h2').contains('Edit Staff');
    });

    it('should fill out and submit the form', () => {
        cy.intercept('PATCH', '/api/staff/12345', {
            statusCode: 200,
        });

        cy.get('#fullName').clear().type('Jane Smith');
        cy.get('#licenseNumber').clear().type('LIC123456');
        cy.get('#phoneNumber').clear().type('987654321');
        cy.get('#email').clear().type('jane.smith@example.com');
        cy.get('#specializationId').clear().type('1');

        cy.get('.btn-submit').click();
        cy.get('.modal').should('be.visible');
        cy.get('.btn-confirm').click();

        cy.url().should('include', '/staffs');
        cy.get('.success-message').should('contain', 'Staff 12345 successfully edited!');
    });

    it('should display an error message if form submission fails', () => {
        cy.intercept('PATCH', '/api/staff/12345', {
            statusCode: 500,
            body: { message: 'Failed to save changes' }
        });

        cy.get('#fullName').clear().type('Jane Smith');
        cy.get('#licenseNumber').clear().type('LIC123456');
        cy.get('#phoneNumber').clear().type('987654321');
        cy.get('#email').clear().type('jane.smith@example.com');
        cy.get('#specializationId').clear().type('1');

        cy.get('.btn-submit').click();
        cy.get('.modal').should('be.visible');
        cy.get('.btn-confirm').click();

        cy.get('.error-message').should('contain', 'Failed to save changes');
    });

    it('should cancel the form submission', () => {
        cy.get('.btn-cancel').click();
        cy.url().should('not.include', '/staff/edit/12345');
    });

    it('should display a confirmation modal on form submission', () => {
        cy.get('#fullName').clear().type('Jane Smith');
        cy.get('#licenseNumber').clear().type('LIC123456');
        cy.get('#phoneNumber').clear().type('987654321');
        cy.get('#email').clear().type('jane.smith@example.com');
        cy.get('#specializationId').clear().type('1');

        cy.get('.btn-submit').click();
        cy.get('.modal').should('be.visible');
    });

    it('should close the confirmation modal when cancel button is clicked', () => {
        cy.get('.btn-submit').click();
        cy.get('.modal').should('be.visible');
        cy.get('.btn-cancel-modal').click(); // Substitua pelo seletor do botão de cancelar na modal
        cy.get('.modal').should('not.be.visible');
    });
});
