describe('Edit patient', () => {
    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/patient/edit/202410000001');
    });

    it('should display the edit patient form', () => {
        cy.get('.edit-patient-container').should('be.visible');
        cy.get('h2').contains('Edit Patient');
    });

    it('should fill out and submit the form', () => {
        cy.get('#name').clear().type('John Doe');
        cy.get('#email').clear().type('john.doe@example.com');
        cy.get('#phoneNumber').clear().type('1234567890');
        cy.get('#street').clear().type('123 Main St');
        cy.get('#postalCode').clear().type('12345');
        cy.get('#city').clear().type('Anytown');
        cy.get('#country').clear().type('USA');
        cy.get('#medicalConditions').clear().type('None');

        cy.get('.btn-submit').click();
        cy.get('.modal').should('be.visible');
        cy.get('.btn-confirm').click();

        cy.get('.error-message').should('not.exist');
    });

    it('should display an error message if form submission fails', () => {
        cy.intercept('POST', '/api/patients/edit', {
            statusCode: 500,
            body: { message: 'Failed to save changes' }
        });

        cy.get('#name').clear().type('John Doe');
        cy.get('#email').clear().type('john.doe@example.com');
        cy.get('#phoneNumber').clear().type('1234567890');
        cy.get('#street').clear().type('123 Main St');
        cy.get('#postalCode').clear().type('12345');
        cy.get('#city').clear().type('Anytown');
        cy.get('#country').clear().type('USA');
        cy.get('#medicalConditions').clear().type('None');

        cy.get('.btn-submit').click();
        cy.get('.modal').should('be.visible');
        cy.get('.btn-confirm').click();

        cy.get('.error-message').should('contain', 'Failed to edit patient');
    });

    it('should cancel the form submission', () => {
        cy.get('.btn-cancel').click();
        cy.url().should('not.include', '/patient/edit/202410000001');
    });
});