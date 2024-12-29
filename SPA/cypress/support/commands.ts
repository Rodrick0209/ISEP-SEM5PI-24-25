// ***********************************************
// This example namespace declaration will help
// with Intellisense and code completion in your
// IDE or Text Editor.
// ***********************************************
// declare namespace Cypress {
//   interface Chainable<Subject = any> {
//     customCommand(param: any): typeof customCommand;
//   }
// }
//
// function customCommand(param: any): void {
//   console.warn(param);
// }
//
// NOTE: You can use it like so:
// Cypress.Commands.add('customCommand', customCommand);
//
// ***********************************************
// This example commands.js shows you how to
// create various custom commands and overwrite
// existing commands.
//
// For more comprehensive examples of custom
// commands please read more here:
// https://on.cypress.io/custom-commands
// ***********************************************
//
//
// -- This is a parent command --
// Cypress.Commands.add("login", (email, password) => { ... })
//
//
// -- This is a child command --
// Cypress.Commands.add("drag", { prevSubject: 'element'}, (subject, options) => { ... })
//
//
// -- This is a dual command --
// Cypress.Commands.add("dismiss", { prevSubject: 'optional'}, (subject, options) => { ... })
//
//
// -- This will overwrite an existing command --
// Cypress.Commands.overwrite("visit", (originalFn, url, options) => { ... })

declare namespace Cypress {
    interface Chainable {
        intercept(arg0: string, arg1: string, arg2: { statusCode: number; }): unknown;
        loginAsAdmin(): Chainable<void>;
        loginAsDoctor(): Chainable<void>;
        loginAsPatient(): Chainable<void>;
    }
}

Cypress.Commands.add('loginAsAdmin', () => {
    cy.visit('/login')
        .intercept

        (
            {
                method: 'POST',
                url: '/api/login/login'
            },
            {
                statusCode: 200,
                fixture: 'login-admin.json'
            }
        )
    .as('login')
        .get('input#email').type('admin@teste.com')
        .get('input#password').type('password')
        .get('button[type="submit"]').click();
    })

Cypress.Commands.add('loginAsDoctor', () => {
    cy.visit('/login')
        .get('input#email').type('D202512344@gmail.com')
        .get('input#password').type('password')
        .get('button[type="submit"]').click();
});

Cypress.Commands.add('loginAsPatient', () => {
    cy.intercept(
        {
            method: 'POST',
            url: '/api/login/login'
        },
        {
            statusCode: 200,
            fixture: 'login-patient.json'
        }
    ).as('login');

    cy.visit('/login')
        .get('input#email').type('john.cena@gmail.com')
        .get('input#password').type('password')
        .get('button[type="submit"]').click();
})