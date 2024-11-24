describe('List Operation Requests', () => {
    beforeEach(() => {
        cy.loginAsAdmin();
        cy.visit('/operation-requests'); // Adjust the URL as needed
    });

    it('should display the list of operation requests', () => {
        cy.intercept('GET', '/api/OperationRequest', { fixture: 'operationRequests.json' }).as('getOperationRequests');
        cy.visit('/operation-requests');
        cy.wait('@getOperationRequests');
        cy.get('tbody tr').should('have.length.greaterThan', 0);
    });

    it('should display a message if no operation requests are found', () => {
        cy.intercept('GET', '/api/OperationRequest', { statusCode: 200, body: [] }).as('getNoOperationRequests');
        cy.visit('/operation-requests');
        cy.wait('@getNoOperationRequests');
        cy.get('.no-requests-message').should('contain', 'No operation requests found.');
    });

    it('should display an error message if the request fails', () => {
        cy.intercept('GET', '/api/OperationRequest', { statusCode: 500, body: { message: 'Internal Server Error' } }).as('getOperationRequestsError');
        cy.visit('/operation-requests');
        cy.wait('@getOperationRequestsError');
        cy.get('.error-message').should('contain', 'Failed to load operation requests: Internal Server Error');
    });

    it('should navigate to the operation request details page when a request is clicked', () => {
        cy.intercept('GET', '/api/OperationRequest', { fixture: 'operationRequests.json' }).as('getOperationRequests');
        cy.visit('/operation-requests');
        cy.wait('@getOperationRequests');
        cy.get('tbody tr').first().click();
        cy.url().should('include', '/operation-requests/');
    });
});