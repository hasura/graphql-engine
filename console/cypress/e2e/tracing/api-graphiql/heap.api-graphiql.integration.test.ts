import { blockServerRequests } from './utils/blockServerRequests';
import {
  stubInitialServerRequests,
  waitForInitialServerRequests,
} from './fixtures/initialRequests/stubInitialServerRequests';

describe('Tracing GraphiQL plugins usage', () => {
  before(() => {
    cy.log('**--- Start controlling the server**');
    blockServerRequests();
    stubInitialServerRequests();

    cy.log('**--- Load the GraphiQL page**');
    cy.visit('/');

    waitForInitialServerRequests();

    // If GraphiQL does not appear, chances are that the Console performs different requests compared
    // to the stubbed ones above
    cy.log('**--- Wait the Console fully loads by looking for GraphiQL**');
    cy.findAllByText('GraphiQL').should('be.visible');

    // --------------------
    cy.log('**--- Create a fake window.heap object**');
    cy.window().then(win => {
      win.heap = {
        track: () => {},
      };
      cy.spy(win.heap, 'track').as('spyHeapTrack');
    });
  });

  it('When the users interact with GraphiQL, then some custom Heap events must be traced', () => {
    // --------------------
    cy.log('**--- Click the GraphiQL Prettify button**');
    cy.get('.graphiql-container').within(() => {
      cy.findByRole('button', { name: 'Prettify' }).click();

      cy.get('@spyHeapTrack').should(
        'have.been.calledWith',
        'GraphiQl > Prettify > click'
      );
    });

    // --------------------
    cy.log('**--- Click the GraphiQL History button**');
    cy.get('.graphiql-container').within(() => {
      cy.findByRole('button', { name: 'History' }).click();

      cy.get('@spyHeapTrack').should(
        'have.been.calledWith',
        'GraphiQl > History > click'
      );
    });

    // --------------------
    cy.log('**--- Click the GraphiQL Explorer button**');
    cy.get('.graphiql-container').within(() => {
      cy.findByRole('button', { name: 'Explorer' }).click();

      cy.get('@spyHeapTrack').should(
        'have.been.calledWith',
        'GraphiQl > Explorer > click'
      );
    });

    // --------------------
    cy.log('**--- Click the GraphiQL REST button**');
    cy.get('.graphiql-container').within(() => {
      cy.findByRole('button', { name: 'REST' }).click();

      cy.get('@spyHeapTrack').should(
        'have.been.calledWith',
        'GraphiQl > REST > click'
      );
    });

    // --------------------
    cy.log('**--- Click the GraphiQL Derive action button**');
    cy.get('.graphiql-container').within(() => {
      cy.findByRole('button', { name: 'Derive action' }).click();

      cy.get('@spyHeapTrack').should(
        'have.been.calledWith',
        'GraphiQl > Derive action > click'
      );
    });

    // --------------------
    cy.log('**--- Click the GraphiQL Analyze button**');
    cy.get('.graphiql-container').within(() => {
      cy.log(
        '**--- Type a valid query, otherwise the Analyze button does not work**'
      );
      cy.get('.query-editor').type(
        `
        query MyQuery {
          temp_1_temp_table {
            name
        `,
        {
          delay: 0,
          parseSpecialCharSequences: false,
        }
      );

      // --------------------
      cy.findByRole('button', { name: 'Analyze' }).click();

      cy.get('@spyHeapTrack').should(
        'have.been.calledWith',
        'GraphiQl > Analyze > click'
      );
    });

    // --------------------
    cy.log(
      `**--- ⚠️ 'GraphiQl > Cache > click' is not tested because it's part of the Pro Console for which we do not have tests yet**`
    );

    // --------------------
    cy.log('**--- Click the GraphiQL Code Exporter button**');
    cy.get('.graphiql-container').within(() => {
      cy.findByRole('button', { name: 'Code Exporter' }).click();

      cy.get('@spyHeapTrack').should(
        'have.been.calledWith',
        'GraphiQl > Code Exporter > click'
      );
    });

    // --------------------
    cy.get('.graphiql-container .graphiql-code-exporter').within(() => {
      cy.log('**--- Click the JavaScript button**');
      cy.findAllByText('JavaScript').filter('[title=Language]').click();

      cy.get('@spyHeapTrack').should(
        'have.been.calledWith',
        'GraphiQl > Code Exporter > JavaScript/TypeScript > click'
      );

      // --------------------
      cy.log('**--- Click the fetch button**');
      cy.findAllByText('fetch').filter('[title=Mode]').click();

      cy.get('@spyHeapTrack').should(
        'have.been.calledWith',
        'GraphiQl > Code Exporter > Fetch > click'
      );

      // --------------------
      cy.log('**--- Click the server-side usage button**');
      cy.findAllByText('server-side usage').click();

      cy.get('@spyHeapTrack').should(
        'have.been.calledWith',
        'GraphiQl > Code Exporter > server-side usage > click'
      );

      // --------------------
      cy.log('**--- Click the async/await button**');
      cy.findAllByText('async/await').click();

      cy.get('@spyHeapTrack').should(
        'have.been.calledWith',
        'GraphiQl > Code Exporter > async/await > click'
      );

      // --------------------
      cy.log('**--- Click the copy button**');
      // There is no way to find the button since it does not have a unique selector nor we can change
      // its HTML. So we look fot the "Copied!" tooltip that's inside it.
      // The structure at the time of writing is
      //
      // <button class="toolbar-button"> <-- The button to click
      //   <div>Copied!</div> <-- The tooltip
      //   <svg> <-- The icon
      //     ...
      //   </svg>
      // </button>
      cy.findAllByText('Copied!').parent().click();

      cy.get('@spyHeapTrack').should(
        'have.been.calledWith',
        'GraphiQl > Code Exporter > copy > click'
      );
    });
  });
});
