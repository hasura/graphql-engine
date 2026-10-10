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
    cy.get('#time').click();
    cy.get('.react-datepicker__navigation--next').click();
    cy.get('.react-datepicker__day:not(.react-datepicker__day--outside-month)')
      .eq(1)
      .click();

    // add payload
    cy.log('**--- Add request payload');
    cy.get('.ace_content').type('{{}"name":"json_payload"}');

    // Add headers
    cy.log('**--- Add headers');
    cy.findByText('Advanced Settings').click();
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
    // The scheduled event is rendered as a complete row of the events table.
    cy.get('[data-test=event-filter-table] thead th').then(($headers) => {
      cy.get('[data-test=event-filter-table] tbody tr')
        .first()
        .find('td')
        .should('have.length', $headers.length);
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

    // The scheduled event is rendered as a complete row of the events table.
    cy.get('[data-test=event-filter-table] thead th').then(($headers) => {
      cy.get('[data-test=event-filter-table] tbody tr')
        .first()
        .find('td')
        .should('have.length', $headers.length);
    });
  });
});
