import { getElementFromAlias } from '../../../../helpers/dataHelpers';

// fake inconsistentMetadata api response

const addDomainOneByOne = (domain: string) => {
  // click Add Domain
  cy.get(getElementFromAlias('add-insecure-domain')).click();
  // type domain name-1
  cy.get(getElementFromAlias('domain-name')).type(domain);
  // click Add to Allow List
  cy.get(getElementFromAlias('add-tls-allow-list')).click();
  // check metadata
  cy.get(getElementFromAlias(domain)).contains(domain);
};

export const addSuccessfulInsecureDomain = () => {
  // open insecure domain page
  cy.visit('/settings/insecure-domain');
  // add 1st domain
  addDomainOneByOne('www.insecuredomain-1.com');
  // add 2nd domain
  addDomainOneByOne('www.insecuredomain-2.com');
  // add 3rd domain
  addDomainOneByOne('www.insecuredomain-3.com');
  cy.wait(2000);
};

export const duplicateAndEmptyDomainError = () => {
  // click add domain
  cy.get(getElementFromAlias('add-insecure-domain')).click();
  // type duplicate domain name
  cy.get(getElementFromAlias('domain-name')).type('www.insecuredomain-1.com');
  // click Add to Allow List
  cy.get(getElementFromAlias('add-tls-allow-list')).click();
  cy.wait(1000);
  // duplicate error
  cy.get('.notification-error', { timeout: 5000 })
    .should('be.visible')
    .and('contain', 'Adding domain to insecure TLS allow list failed');
  // close add domain dialog box
  cy.get(getElementFromAlias('cancel-domain')).click();
  // click add domain
  cy.get(getElementFromAlias('add-insecure-domain')).click();
  // click Add to Allow List with empty domain name
  cy.get(getElementFromAlias('add-tls-allow-list')).click();
  // empty domain error
  cy.get('.notification-error', { timeout: 5000 })
    .should('be.visible')
    .and('contain', 'No domain found');
  cy.get(getElementFromAlias('cancel-domain')).click();
};

export const deleteInsecureDomain = () => {
  // click to delete domain
  cy.get(getElementFromAlias('delete-domain-www.insecuredomain-1.com')).click({
    force: true,
  });
  cy.get(getElementFromAlias('delete-domain-www.insecuredomain-2.com')).click({
    force: true,
  });
  cy.get(getElementFromAlias('delete-domain-www.insecuredomain-3.com')).click({
    force: true,
  });
  cy.get(getElementFromAlias('label-no-domain-found'), { timeout: 10000 })
    .should('be.visible')
    .and('contain', 'No domains added to insecure TLS allow list');
};
