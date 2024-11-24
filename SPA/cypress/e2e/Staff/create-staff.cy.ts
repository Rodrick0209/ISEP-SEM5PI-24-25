describe('Create Staff', () => {

    beforeEach(() => {
        cy.loginAsAdmin(); // Assumindo que existe uma função Cypress para login
        cy.visit('/staff/create'); // Altere a rota conforme necessário
    });

    it('should display the create staff form', () => {
        cy.get('.create-staff-container').should('be.visible');
        cy.get('h2').contains('Create New Staff');
    });

    it('should display required fields', () => {
        cy.get('input[name="fullName"]').should('have.attr', 'required');
        cy.get('input[name="licenseNumber"]').should('have.attr', 'required');
        cy.get('input[name="specialization"]').should('have.attr', 'required');
        cy.get('input[name="email"]').should('have.attr', 'required');
        cy.get('input[name="phoneNumber"]').should('have.attr', 'required');
        cy.get('select[name="category"]').should('have.attr', 'required');
    });

    it('should create a new staff member', () => {
        cy.get('input[name="fullName"]').type('Alice Smith');
        cy.get('input[name="licenseNumber"]').type('12345ABC');
        cy.get('input[name="specialization"]').type('Cardiology');
        cy.get('input[name="email"]').type('alice.smith@example.com');
        cy.get('input[name="phoneNumber"]').type('987654321');
        cy.get('select[name="category"]').select('Doctor');

        cy.get('button[type="submit"]').click();

        cy.get('.modal').should('be.visible');
        cy.get('.modal-content h3').contains('Confirm Submission');
        cy.get('.modal-actions .btn-confirm').click();

        cy.url().should('include', '/staffs');
        cy.get('.success-message').should('contain', 'Staff Alice Smith successfully created!');
    });

    it('should cancel staff creation', () => {
        cy.get('button.btn-cancel').click();
        cy.url().should('not.include', '/staff/create');
    });
});
