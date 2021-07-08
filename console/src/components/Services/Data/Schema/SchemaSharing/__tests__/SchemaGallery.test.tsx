import { setupServer } from 'msw/node';
import {
  render,
  screen,
  waitForElementToBeRemoved,
} from '@testing-library/react';
import { act } from 'react-dom/test-utils';
import { configureStore } from '@reduxjs/toolkit';
import { Provider } from 'react-redux';
import React from 'react';
import SchemaGallery from '../SchemaGallery';
import { schemaSharingReducer } from '../Actions';
import { networkStubs } from './stubs/schemaSharingNetworkStubs';

const server = setupServer();

beforeAll(() => server.listen());
afterEach(() => server.resetHandlers());
afterAll(() => server.close());

const renderSchemaGallery = () => {
  const store = configureStore<any>({
    reducer: {
      schemaSharing: schemaSharingReducer,
      tables: () => ({ currentDataSource: 'postgres' }),
    },
  });
  render(
    <Provider store={store} key="provider">
      <SchemaGallery />
    </Provider>
  );
};
describe('SchemaGallery', () => {
  it('should the list of templates', async () => {
    server.use(
      networkStubs.rootJson,
      networkStubs.template1.metadata,
      networkStubs.template1.firstSql,
      networkStubs.template1.secondSql,
      networkStubs.template1.config
    );
    renderSchemaGallery();

    expect(screen.getByText(/loading schemas/i)).toBeVisible();
    await waitForElementToBeRemoved(() => screen.getByText(/loading schemas/i));

    screen.getByText(/template-1/i).click();

    expect(screen.getByText(/loading schema/i)).toBeVisible();
    await waitForElementToBeRemoved(() => screen.getByText(/loading schema/i));

    expect(screen.getByText(/long long description/i)).toBeVisible();

    act(() => {
      screen
        .getByRole('button', {
          name: /install schema/i,
        })
        .click();
    });
    await waitForElementToBeRemoved(() =>
      screen.getByText(/long long description/i)
    );

    expect(
      screen.queryByText(/long long description/i)
    ).not.toBeInTheDocument();
  });
});
