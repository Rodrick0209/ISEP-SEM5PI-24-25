describe('Operation Type List Page', () => {
    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/operationType');
    });

    it('should display the OperationType list page', () => {
        cy.get('.operation-types-container').should('be.visible');
    });


    it('should display operationType in the table', () => {
        cy.get('table').should('be.visible');
        cy.get('tbody tr').should('have.length.greaterThan', 0);
    });


});