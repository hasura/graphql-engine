import React from 'react';
import {
  render,
  screen,
  waitForElementToBeRemoved,
  within,
} from '@testing-library/react';
import { setupServer } from 'msw/node';
import { configureStore } from '@reduxjs/toolkit';
import { Provider } from 'react-redux';
import { schemaSharingReducer, SchemaSharingTemplateItem } from '../Actions';
import {
  SchemaGalleryBody,
  SchemaGalleryContentRow,
} from '../SchemaGalleryTable';
import { networkStubs } from './stubs/schemaSharingNetworkStubs';

const templateItem: SchemaSharingTemplateItem = {
  key: 'template-1',
  description: 'Some description of the schema',
  fetchingStatus: 'success',
  title: 'Title of the schema',
  isPartialData: true,
  dialect: 'postgres',
  type: 'database',
  relativeFolderPath: './test',
};

describe('SchemaGalleryContentRow', () => {
  it('should display the content', () => {
    const openModal = jest.fn();
    render(
      <table>
        <tbody>
          <SchemaGalleryContentRow
            template={templateItem}
            openModal={openModal}
          />
        </tbody>
      </table>
    );

    expect(
      screen.getByRole('cell', {
        name: /some description of the schema/i,
      })
    ).toBeVisible();

    expect(screen.getByText(/title of the schema/i)).toBeVisible();
    screen.getByText(/title of the schema/i).click();
    expect(openModal).toHaveBeenCalled();
  });
});

const server = setupServer();

beforeAll(() => server.listen());
afterEach(() => server.resetHandlers());
afterAll(() => server.close());

const renderSchemaGalleryBody = () => {
  const openModalFn = jest.fn();

  const store = configureStore<any>({
    reducer: {
      schemaSharing: schemaSharingReducer,
    },
  });
  render(
    <Provider store={store} key="provider">
      <SchemaGalleryBody onModalOpen={openModalFn} />
    </Provider>
  );

  return {
    openModalFn,
  };
};

describe('SchemaGalleryBody', () => {
  it('should display loading at first', async () => {
    server.use(networkStubs.rootJsonWithLoading);

    renderSchemaGalleryBody();

    expect(screen.getByText(/loading schemas/i)).toBeVisible();
  });
  it('should display an error when the api return something bad', async () => {
    server.use(networkStubs.rootJsonError);

    renderSchemaGalleryBody();

    await waitForElementToBeRemoved(() => screen.getByText(/loading schemas/i));

    expect(
      screen.getByText(/something went wrong, please try again later\./i)
    ).toBeVisible();
  });
  it('should display an error when the api return something bad', async () => {
    server.use(networkStubs.rootJsonEmpty);

    renderSchemaGalleryBody();

    await waitForElementToBeRemoved(() => screen.getByText(/loading schemas/i));

    expect(screen.getByText(/no template/i)).toBeVisible();
  });
  it('should display the list from the api', async () => {
    server.use(networkStubs.rootJson);

    const { openModalFn } = renderSchemaGalleryBody();

    await waitForElementToBeRemoved(() => screen.getByText(/loading schemas/i));

    expect(screen.getByText(/aaaaaaaa/i)).toBeVisible();

    expect(
      within(screen.getAllByRole('cell')[1]).getByText(/template-1/i)
    ).toBeVisible();

    expect(
      within(screen.getAllByRole('cell')[2]).getByText(
        /this is the description of template one/i
      )
    ).toBeVisible();
    expect(
      within(screen.getAllByRole('cell')[3]).getByText(/bbbbbbbb/i)
    ).toBeVisible();
    expect(
      within(screen.getAllByRole('cell')[4]).getByText(/template-2/i)
    ).toBeVisible();
    expect(
      within(screen.getAllByRole('cell')[5]).getByText(
        /this is the description of template two/i
      )
    ).toBeVisible();

    screen.getByText(/template-1/i).click();

    expect(openModalFn).toHaveBeenCalledWith({
      key: 'template-1',
      section: 'AAAAAAAA',
    });
  });
});
