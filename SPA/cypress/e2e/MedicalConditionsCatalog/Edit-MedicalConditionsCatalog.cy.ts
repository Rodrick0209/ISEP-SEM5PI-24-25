describe('Edit Medical Condition', () => {
    /*
    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/medicalConditions/edit/20241812');
    });

    it('should load the medical condition details', () => {
        cy.get('#designation').should('have.value', 'Teste');
        cy.get('#description').should('have.value', 'TesteABC1234\n');
    });

    it('should show confirmation modal on form submit', () => {
        cy.get('#designation').clear().type('Teste');
        cy.get('#description').clear().type('TesteABC1234\n');
        cy.get('.btn-submit').click();
        cy.get('.modal').should('be.visible');
    });

    it('should submit the form and navigate to medical conditions list on confirmation', () => {
        cy.intercept('PATCH', '/api2/medicalConditions/update/20241812', {
            statusCode: 200,
            body: {
                code: '20241812',
                designation: 'Teste',
                description: 'TesteABC1234'
            }
        }).as('updateMedicalCondition');
        cy.get('#designation').clear().type('Teste');
        cy.get('#description').clear().type('TesteABC1234\n');
        cy.get('.btn-submit').click();
        cy.get('.btn-confirm').click();
        cy.url().should('include', '/medicalConditions');
        cy.contains('Medical Condition 20241812 successfully edited!');
    });

    it('should navigate to medical conditions list on cancel', () => {
        cy.get('.btn-cancel').click();
        cy.url().should('include', '/medicalConditions');
    });
    */
});