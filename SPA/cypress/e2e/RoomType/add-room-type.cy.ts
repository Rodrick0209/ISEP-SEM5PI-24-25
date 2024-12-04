describe('Add Room Type', () => {
    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/room-types/add');
    });

    it('should display the add room type form', () => {
        cy.get('h2').contains('Add New Room Type');
        cy.get('form').should('exist');
    });

    it('should successfully add a new room type', () => {
        cy.get('input[name="internalCode"]').type('ICQ-0001');
        cy.get('input[name="designation"]').type('Deluxe Room');
        cy.get('textarea[name="description"]').type('A deluxe room with all amenities.');
        cy.get('select[name="suitableForSurgeries"]').select('yes');
        cy.get('button[type="submit"]').click();

        cy.get('.modal').should('be.visible');
        cy.get('.btn-confirm').click();
    });

    it('should navigate back on cancel button click', () => {
        cy.get('.btn-cancel').click();
        cy.url().should('not.include', '/room-types/add');
    });
});