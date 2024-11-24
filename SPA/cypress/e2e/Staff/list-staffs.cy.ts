describe('Staff List Page', () => {
    beforeEach(() => {
        cy.loginAsAdmin(); // Realiza login como administrador
        cy.visit('/staffs'); // Acessa a página de listagem de colaboradores
    });

    it('should display the staff list page', () => {
        cy.get('.list-staff-container').should('be.visible');
        cy.get('h1').contains('Staff List'); // Verifica o título da página
    });

    it('should display the create staff button', () => {
        cy.get('.create-staff-button')
            .should('be.visible')
            .and('contain', 'Create Staff Profile');
    });

    it('should navigate to the create staff page on button click', () => {
        cy.get('.create-staff-button').click();
        cy.url().should('include', '/staff/create');
    });

    it('should display staff members in the table', () => {
        cy.get('table').should('be.visible');
        cy.get('tbody tr').should('have.length.greaterThan', 0);
    });

    it('should navigate to staff details page on details button click', () => {
        cy.get('tbody tr').first().find('button').contains('Details').click();
        cy.url().should('include', '/staff/details');
    });

    it('should navigate to edit staff page on edit button click', () => {
        cy.get('tbody tr').first().find('button').contains('Edit').click();
        cy.url().should('include', '/staff/edit');
    });

    it('should navigate to delete staff page on delete button click', () => {
        cy.get('tbody tr').first().find('button').contains('Delete').click();
        cy.url().should('include', '/staff/delete');
    });

    it('should filter staff by name', () => {
        cy.get('.filter-input-name').type('Dr. Smith'); // Insere um nome no campo de filtro
        cy.get('.filter-button').click(); // Clica no botão de filtrar
        cy.get('tbody tr').each((row) => {
            cy.wrap(row).should('contain', 'Dr. Smith'); // Verifica se cada linha contém o nome filtrado
        });
    });

    it('should paginate the staff list', () => {
        cy.get('.pagination-next').click(); // Vai para a próxima página
        cy.get('.current-page').should('contain', '2'); // Verifica se a página atualizou para 2
        cy.get('.pagination-previous').click(); // Volta para a página anterior
        cy.get('.current-page').should('contain', '1');
    });

    it('should display an error message if the staff list fails to load', () => {
        cy.intercept('GET', '/api/staffs', { statusCode: 500, body: { message: 'Failed to fetch staff' } });
        cy.visit('/staffs'); // Recarrega a página para aplicar o intercept
        cy.get('.error-message').should('contain', 'An error occurred while fetching staff');
    });
});
