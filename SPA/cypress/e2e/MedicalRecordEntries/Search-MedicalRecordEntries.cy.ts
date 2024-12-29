describe('Search Medical Record Entries', () => {
    beforeEach(() => {
        cy.loginAsDoctor();
        cy.visit('/patient/medical-record/202410000001');
    });

    it('should navigate to update medical record page on button click', () => {
        cy.get('button.btn-primary').click();
        cy.url().should('include', '/edit-medical-record');
    });

    it('should display medical record entries if available', () => {
        cy.get('table.table').should('be.visible');
        cy.get('tbody tr').should('have.length.greaterThan', 0);
    });

    it('should display no record message if no medical record is available', () => {
        cy.get('.no-record-message').should('be.visible');
    });

    it('should filter medical record entries based on filter criteria', () => {
        cy.get('app-filter-medical-record-entries input').type('aaa');
        cy.get('app-filter-medical-record-entries button').click();
        cy.get('tbody tr').each(($row) => {
            cy.wrap($row).find('td').eq(1).should('contain.text', 'aaaa');
        });
    });
});