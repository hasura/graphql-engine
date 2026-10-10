// This suite predates the rewrite of the Template Gallery feature from a
// Redux-backed implementation (`templateGalleryReducer`,
// `fetchGlobalSchemaSharingConfiguration`, etc. from `../Actions`) to the
// current react-query based implementation. Those APIs no longer exist, so
// the original test body (kept below for reference) cannot be restored as-is.
// Coverage for the current implementation lives in
// `TemplateGalleryTable.test.tsx` and `TemplateGalleryModal.test.tsx`.
//
// import { setupServer } from 'msw/node';
// import {
//   render,
//   screen,
//   waitForElementToBeRemoved,
// } from '@testing-library/react';
// import { act } from 'react-dom/test-utils';
// import TemplateGallery from '../TemplateGallery';
// import { templateGalleryReducer } from '../Actions';
// import { networkStubs } from './stubs/schemaSharingNetworkStubs';

// const server = setupServer();

// beforeAll(() => server.listen());
// afterEach(() => server.resetHandlers());
// afterAll(() => server.close());

// const renderSchemaGallery = () => {
//   const store = configureStore<any>({
//     reducer: {
//       templateGallery: templateGalleryReducer,
//       tables: () => ({ currentDataSource: 'postgres' }),
//       metadata: () => ({ metadataObject: { version: 3 } }),
//     },
//   });
//   render(
//     <Provider store={store} key="provider">
//       <TemplateGallery />
//     </Provider>,
//   );
// };
// describe('SchemaGallery', () => {
//   it('should the list of templates', async () => {
//     server.use(
//       networkStubs.rootJson,
//       networkStubs.template1.metadata,
//       networkStubs.template1.firstSql,
//       networkStubs.template1.secondSql,
//       networkStubs.template1.config,
//     );
//     renderSchemaGallery();

//     expect(screen.getByText(/loading templates/i)).toBeVisible();
//     await waitForElementToBeRemoved(() =>
//       screen.getByText(/loading templates/i),
//     );

//     screen.getByText(/template-1/i).click();

//     expect(screen.getByText(/loading template/i)).toBeVisible();
//     await waitForElementToBeRemoved(() =>
//       screen.getByText(/loading template/i),
//     );

//     expect(screen.getByText(/long long description/i)).toBeVisible();

//     act(() => {
//       screen
//         .getByRole('button', {
//           name: /install template/i,
//         })
//         .click();
//     });
//     await waitForElementToBeRemoved(() =>
//       screen.getByText(/long long description/i),
//     );

//     expect(
//       screen.queryByText(/long long description/i),
//     ).not.toBeInTheDocument();
//   });
// });

describe('SchemaGallery', () => {
  it.todo(
    'should show the list of templates (superseded by TemplateGalleryTable.test.tsx / TemplateGalleryModal.test.tsx; original test used a Redux API that no longer exists)',
  );
});
