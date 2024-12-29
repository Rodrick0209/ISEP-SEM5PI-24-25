describe('Edit Allergy Catalog', () => {
    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/allergiesCatalog/edit/20241812');
    });


    it('should show confirmation modal on form submit', () => {
        cy.get('input[name="designation"]').clear();
        cy.get('textarea[name="description"]').clear();
        cy.get('input[name="designation"]').type('aaaa3');
        cy.get('textarea[name="description"]').type('aaaabc1');
        cy.get('button[type="submit"]').click();
        cy.get('.modal').should('be.visible');
    });

    it('should close confirmation modal on cancel', () => {
        cy.get('input[name="designation"]').clear();
        cy.get('textarea[name="description"]').clear();
        cy.get('input[name="designation"]').type('aaaa3');
        cy.get('textarea[name="description"]').type('aaaabc1');
        cy.get('button[type="submit"]').click();
        cy.get('.modal').should('be.visible');
        cy.get('.modal .btn-cancel').click();
        cy.get('.modal').should('not.exist');
    });

    it('should submit the form and navigate to allergies catalog', () => {
        cy.intercept('PUT', '/api2/allergiesCatalog/update/20241812', {
            statusCode: 200,
            body: {
                code: '20241812',
                designation: 'aaaaa',
                description: 'TesteABC123'
            }
        }).as('updateAllergy');

        cy.get('input[name="designation"]').clear();
        cy.get('textarea[name="description"]').clear();
        cy.get('input[name="designation"]').type('aaaaa');
        cy.get('textarea[name="description"]').type('TesteABC123');
        cy.get('button[type="submit"]').click();
        cy.get('.modal .btn-confirm').click();
        cy.wait('@updateAllergy');
        cy.url().should('include', '/allergiesCatalog');
    });

    it('should navigate to allergies catalog on cancel', () => {
        cy.get('button.btn-cancel').click();
        cy.url().should('include', '/allergiesCatalog');
    });
});