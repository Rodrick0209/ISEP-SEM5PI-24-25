describe('Patient List Page', () => {
    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/patients');
    });

    it('should display the patient list page', () => {
        cy.get('.list-patients-container').should('be.visible');
    });

    it('should display the create patient button', () => {
        cy.get('.create-patient-button').should('be.visible').and('contain', 'Create Patient Profile');
    });

    it('should navigate to create patient page on button click', () => {
        cy.get('.create-patient-button').click();
        cy.url().should('include', '/patient/create');
    });

    it('should display success message if present', () => {
        cy.get('.success-message').should('be.visible');
    });

    it('should display error message if present', () => {
        cy.get('.error-message').should('be.visible');
    });

    it('should display patients in the table', () => {
        cy.get('table').should('be.visible');
        cy.get('tbody tr').should('have.length.greaterThan', 0);
    });

    it('should navigate to patient details on details button click', () => {
        cy.get('tbody tr').first().find('button').contains('Details').click();
        cy.url().should('include', '/patient/details');
    });

    it('should navigate to edit patient page on edit button click', () => {
        cy.get('tbody tr').first().find('button').contains('Edit').click();
        cy.url().should('include', '/patient/edit');
    });

    it('should navigate to delete patient page on delete button click', () => {
        cy.get('tbody tr').first().find('button').contains('Delete').click();
        cy.url().should('include', '/patient/delete');
    });

    it('should paginate through the patient list', () => {
        cy.get('.pagination-controls').should('be.visible');
        cy.get('.pagination-controls button').contains('Next').click();
        cy.get('.pagination-controls span').should('contain', 'Page 2');
        cy.get('.pagination-controls button').contains('Previous').click();
        cy.get('.pagination-controls span').should('contain', 'Page 1');
    });
});