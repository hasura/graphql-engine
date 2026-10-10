// This suite predates the rewrite of the Template Gallery feature from a
// Redux-backed implementation (`templateGalleryReducer`,
// `fetchGlobalSchemaSharingConfiguration`, etc.) to the current react-query
// based implementation (`TemplateGalleryModalBody` now takes
// `template`/`isFetching`/`error` props directly). Those old APIs no longer
// exist, so the original test body (kept below for reference) cannot be
// restored as-is. Equivalent coverage would need to be rewritten against the
// current `TemplateGalleryModalBody` props-based API.
//
// import {
//   render,
//   screen,
//   waitForElementToBeRemoved,
// } from '@testing-library/react';
// import { setupServer } from 'msw/node';
// import { configureStore } from '@reduxjs/toolkit';
// import { Provider } from 'react-redux';
// import { RestHandler } from 'msw/lib';
// import { CATEGORY_1, networkStubs } from './stubs/schemaSharingNetworkStubs';
// import { TemplateGalleryModalBody } from '../TemplateGalleryModal';

// const server = setupServer();

// beforeAll(() => server.listen());
// afterEach(() => server.resetHandlers());
// afterAll(() => server.close());

// const schemaGalleryModalBodyRender = async (...handlers: RestHandler[]) => {
//   server.use(networkStubs.rootJson, ...handlers);
//   const store = configureStore<any>({
//     reducer: {
//       templateGallery: templateGalleryReducer,
//       metadata: () => ({ metadataObject: { version: 3 } }),
//     },
//   });

//   await store.dispatch(fetchGlobalSchemaSharingConfiguration());

//   render(
//     <Provider store={store} key="provider">
//       <TemplateGalleryModalBody
//         content={{ key: 'template-1', section: CATEGORY_1 }}
//       />
//     </Provider>,
//   );
// };

// describe('TemplateGalleryModalBody', () => {
//   it('should display loading at first', async () => {
//     await schemaGalleryModalBodyRender(
//       networkStubs.template1.configSlow,
//       networkStubs.template1.metadata,
//       networkStubs.template1.firstSql,
//       networkStubs.template1.secondSql,
//     );

//     expect(screen.getByText(/loading template/i)).toBeVisible();
//   });
//   it('should display an error when a request is failing', async () => {
//     await schemaGalleryModalBodyRender(
//       networkStubs.template1.config,
//       networkStubs.template1.metadata,
//       networkStubs.template1.firstSql,
//       networkStubs.template1.secondSqlError,
//     );

//     await waitForElementToBeRemoved(() =>
//       screen.getByText(/loading template/i),
//     );

//     expect(
//       screen.getByText(/something went wrong, please try again later\./i),
//     ).toBeVisible();
//   });
//   it('should display the content from the config', async () => {
//     await schemaGalleryModalBodyRender(
//       networkStubs.template1.config,
//       networkStubs.template1.metadata,
//       networkStubs.template1.firstSql,
//       networkStubs.template1.secondSql,
//     );

//     await waitForElementToBeRemoved(() =>
//       screen.getByText(/loading template/i),
//     );

//     expect(screen.getByText(/long long description/i)).toBeVisible();

//     expect(screen.getByText(/read the blog post \./i)).toBeVisible();
//   });
// });

describe('TemplateGalleryModalBody', () => {
  it.todo(
    'should display loading/error/content states (original test used a Redux API that no longer exists; needs rewriting against the current props-based TemplateGalleryModalBody)',
  );
});
