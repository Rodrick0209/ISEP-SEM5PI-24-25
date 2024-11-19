describe('Create Patient', () => {

    beforeEach(() => {
        cy.loginAsAdmin()
        cy.visit('/patient/create');
    });

    it('should display the create patient form', () => {
        cy.get('.create-patient-container').should('be.visible');
        cy.get('h2').contains('Create New Patient');
    });

    it('should display required fields', () => {
        cy.get('input[name="firstName"]').should('have.attr', 'required');
        cy.get('input[name="lastName"]').should('have.attr', 'required');
        cy.get('input[name="fullName"]').should('have.attr', 'required');
        cy.get('input[name="dateOfBirth"]').should('have.attr', 'required');
        cy.get('select[name="gender"]').should('have.attr', 'required');
        cy.get('input[name="email"]').should('have.attr', 'required');
        cy.get('input[name="phoneNumber"]').should('have.attr', 'required');
        cy.get('input[name="street"]').should('have.attr', 'required');
        cy.get('input[name="postalCode"]').should('have.attr', 'required');
        cy.get('input[name="city"]').should('have.attr', 'required');
        cy.get('select[name="country"]').should('have.attr', 'required');
        cy.get('input[name="emergencyContactName"]').should('have.attr', 'required');
        cy.get('input[name="emergencyContactEmail"]').should('have.attr', 'required');
        cy.get('input[name="emergencyContactPhone"]').should('have.attr', 'required');
    });
    
   it('should create a new patient', () => {
        cy.get('input[name="firstName"]').type('John');
        cy.get('input[name="lastName"]').type('Doe');
        cy.get('input[name="fullName"]').type('John Doe');
        cy.get('input[name="dateOfBirth"]').type('1990-01-01');
        cy.get('select[name="gender"]').select('Male');
        cy.get('input[name="email"]').type('john.doe@example.com');
        cy.get('input[name="phoneNumber"]').type('123454512');
        cy.get('input[name="street"]').type('123 Main St');
        cy.get('input[name="postalCode"]').type('12345');
        cy.get('input[name="city"]').type('Anytown');
        cy.get('select[name="country"]').select('United States');
        cy.get('input[name="emergencyContactName"]').type('Jane Doe');
        cy.get('input[name="emergencyContactEmail"]').type('jane.doe@example.com');
        cy.get('input[name="emergencyContactPhone"]').type('0987654321');

        cy.get('button[type="submit"]').click();

        cy.get('.modal').should('be.visible');
        cy.get('.modal-content h3').contains('Confirm Submission');
        cy.get('.modal-actions .btn-confirm').click();

        cy.url().should('include', '/patients');
        cy.get('.success-message').should('contain', 'Patient John Doe successfully created!');
    });

    it('should cancel patient creation', () => {
        cy.get('button.btn-cancel').click();
        cy.url().should('not.include', '/patient/create');
    });
});