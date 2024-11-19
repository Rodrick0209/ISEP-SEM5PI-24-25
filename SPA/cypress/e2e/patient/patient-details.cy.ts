describe('Patient Details', () => {
    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/patient-details');
    });

    it('should display patient details', () => {
        cy.get('.patient-details').should('exist');
        cy.get('.patient-header h2').should('contain', 'Patient Details');
        cy.get('.patient-info h3').should('contain', 'Patient Information');
        cy.get('.patient-field').should('contain', 'Medical Record Number:');
        cy.get('.patient-field').should('contain', 'Name:');
        cy.get('.patient-field').should('contain', 'Date of Birth:');
        cy.get('.patient-field').should('contain', 'Gender:');
        cy.get('.patient-field').should('contain', 'Email:');
        cy.get('.patient-field').should('contain', 'Phone Number:');
        cy.get('.address h4').should('contain', 'Address');
        cy.get('.patient-field').should('contain', 'Street:');
        cy.get('.patient-field').should('contain', 'Postal Code:');
        cy.get('.patient-field').should('contain', 'City:');
        cy.get('.patient-field').should('contain', 'Country:');
        cy.get('.emergency-contact h4').should('contain', 'Emergency Contact');
        cy.get('.patient-field').should('contain', 'Name:');
        cy.get('.patient-field').should('contain', 'Email:');
        cy.get('.patient-field').should('contain', 'Phone Number:');
        cy.get('.medical-history h3').should('contain', 'Medical History');
        cy.get('.patient-field').should('contain', 'Medical Conditions:');
    });

    it('should mask email and phone number', () => {
        cy.get('.patient-field').contains('Email:').should('contain', '***@***.com');
        cy.get('.patient-field').contains('Phone Number:').should('contain', '***');
    });

    it('should navigate back when back button is clicked', () => {
        cy.get('.back-button button').click();
        cy.url().should('not.include', '/patient-details');
    });
});