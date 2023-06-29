describe('Create Scheduled trigger', () => {
  it('with longest path everything should work', () => {
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 1: Create an Scheduled trigger with longest path**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    cy.visit('/events/one-off-scheduled-events/add', {
      timeout: 10000,
    });

    // add webhook url
    cy.log('**--- Add webhook url');
    cy.get('[name=webhook]').type('http://httpbin.org/post');

    // add scheduled time
    cy.log('**--- Add scheduled time');
    cy.get('.rdt').click();
    cy.get('.rdtNext').click();
    cy.get('.rdtDays').find('td').eq(1).click();

    // add payload
    cy.log('**--- Add request payload');
    cy.get('.ace_content').type('{{}"name":"json_payload"}');

    // Add headers
    cy.log('**--- Add headers');
    cy.findByText('Advance Settings').click();
    cy.findAllByRole('button', { name: 'Add request headers' }).click();
    cy.findByPlaceholderText('Key...').type('user_id');
    cy.findByPlaceholderText('Value...').type('1234');

    // Add headers retry config
    cy.log('**--- Add headers');
    cy.findByText('Retry Configuration').click();
    cy.get('[name=num_retries]').clear().type('3');
    cy.get('[name=retry_interval_seconds]').clear().type('20');
    cy.get('[name=timeout_seconds]').clear().type('80');

    // click on create button to save scheduled trigger
    cy.log('**--- Click on Create scheduled event');
    cy.findAllByRole('button', { name: 'Create scheduled event' }).click();

    // expect success notification
    cy.log('**--- Expect success notification');
    cy.expectSuccessNotificationWithMessage('Event scheduled successfully');

    // expect scheduled event in pending events table
    cy.get('[data-test=event-filter-table').should('exist');
    cy.get('[data-test=event-filter-table')
      .find('.rt-tbody')
      .find('div')
      .first()
      .within(() => {
        cy.get('div').should('have.length', 12);
      });
  });
  it('with shortest path everything should work', () => {
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 1: Create an Scheduled trigger with shortest path**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    cy.visit('/events/one-off-scheduled-events/add', {
      timeout: 10000,
    });

    // add webhook url
    cy.log('**--- Add webhook url');
    cy.get('[name=webhook]').type('http://httpbin.org/post');

    // add payload
    cy.log('**--- Add request payload');
    cy.get('.ace_content').type('{{}"name":"json_payload"}');

    // click on create button to save scheduled trigger
    cy.log('**--- Click on Create scheduled event');
    cy.findAllByRole('button', { name: 'Create scheduled event' }).click();

    // expect success notification
    cy.log('**--- Expect success notification');
    cy.expectSuccessNotificationWithMessage('Event scheduled successfully');

    // expect scheduled event in pending events table
    cy.get('[data-test=event-filter-table').should('exist');

    cy.get('[data-test=event-filter-table')
      .find('.rt-tbody')
      .find('div')
      .first()
      .within(() => {
        cy.get('div').should('have.length', 12);
      });
  });
});
