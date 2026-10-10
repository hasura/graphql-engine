import {
  render,
  screen,
  waitForElementToBeRemoved,
  within,
} from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { setupServer } from 'msw/node';
import { vi } from 'vitest';
import { Source } from '@hasura/shared/types';
import { TemplateGalleryBody } from '../TemplateGalleryTable';
import { networkStubs } from './stubs/schemaSharingNetworkStubs';

const mockSource: Source = {
  name: 'default',
  kind: 'postgres',
  tables: [],
  configuration: {} as Source['configuration'],
};

const server = setupServer();

beforeAll(() => server.listen());
afterEach(() => server.resetHandlers());
afterAll(() => server.close());

const renderSchemaGalleryBody = () => {
  const openModalFn = vi.fn();

  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  });

  render(
    <QueryClientProvider client={queryClient}>
      <TemplateGalleryBody onModalOpen={openModalFn} source={mockSource} />
    </QueryClientProvider>,
  );

  return {
    openModalFn,
  };
};

describe('TemplateGalleryBody', () => {
  it('should display loading at first', async () => {
    server.use(networkStubs.rootJsonWithLoading);

    renderSchemaGalleryBody();

    expect(screen.getByText(/loading templates/i)).toBeVisible();
  });
  it('should display an error when the api return something bad', async () => {
    server.use(networkStubs.rootJsonError);

    renderSchemaGalleryBody();

    await waitForElementToBeRemoved(() =>
      screen.getByText(/loading templates/i),
    );

    expect(
      screen.getByText(/something went wrong, please try again later\./i),
    ).toBeVisible();
  });
  it('should display an error when the api return something bad', async () => {
    server.use(networkStubs.rootJsonEmpty);

    renderSchemaGalleryBody();

    await waitForElementToBeRemoved(() =>
      screen.getByText(/loading templates/i),
    );

    expect(screen.getByText(/no templates/i)).toBeVisible();
  });
  it('should display the list from the api', async () => {
    server.use(networkStubs.rootJson);

    const { openModalFn } = renderSchemaGalleryBody();

    await waitForElementToBeRemoved(() =>
      screen.getByText(/loading templates/i),
    );

    expect(
      within(screen.getAllByRole('cell')[1]).getByText(/template-1/i),
    ).toBeVisible();

    expect(
      within(screen.getAllByRole('cell')[2]).getByText(
        /this is the description of template one/i,
      ),
    ).toBeVisible();

    screen.getByText(/template-1/i).click();

    expect(openModalFn).toHaveBeenCalledWith(
      expect.objectContaining({
        key: 'template-1',
      }),
    );
  });
});
