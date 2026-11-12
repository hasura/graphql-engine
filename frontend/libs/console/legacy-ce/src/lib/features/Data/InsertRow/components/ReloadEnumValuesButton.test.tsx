import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { Theme } from '@radix-ui/themes';
import { hasuraToast } from '@hasura/shared/ui';
import { invalidateEnumColumnValues } from '@hasura/metadata/data-source';
import { ReloadEnumValuesButton } from './ReloadEnumValuesButton';

const reloadMetadata = vi.fn();
const showErrorNotification = vi.fn();

vi.mock('@hasura/metadata/api', () => ({
  useReloadMetadata: () => ({ reloadMetadata }),
  useErrorNotification: () => showErrorNotification,
}));

vi.mock('@hasura/metadata/data-source', () => ({
  invalidateEnumColumnValues: vi.fn(),
}));

vi.mock('@hasura/shared/ui', async (importOriginal) => ({
  ...(await importOriginal<typeof import('@hasura/shared/ui')>()),
  hasuraToast: vi.fn(),
}));

const renderButton = () =>
  render(
    <QueryClientProvider client={new QueryClient()}>
      <Theme>
        <ReloadEnumValuesButton sourceName="chinook" />
      </Theme>
    </QueryClientProvider>,
  );

describe('ReloadEnumValuesButton', () => {
  beforeEach(() => vi.clearAllMocks());

  it('reloads the source and refreshes its enum values', async () => {
    reloadMetadata.mockResolvedValue(true);
    renderButton();

    await userEvent.click(
      screen.getByRole('button', { name: 'Reload enum values' }),
    );

    expect(reloadMetadata).toHaveBeenCalledWith({ sources: ['chinook'] });
    await waitFor(() =>
      expect(hasuraToast).toHaveBeenCalledWith({
        type: 'success',
        title: 'Enum values reloaded',
      }),
    );
    expect(invalidateEnumColumnValues).toHaveBeenCalledWith(
      expect.any(QueryClient),
      'chinook',
    );
  });

  it('notifies when the reload fails', async () => {
    const error = new Error('boom');
    reloadMetadata.mockRejectedValue(error);
    renderButton();

    await userEvent.click(
      screen.getByRole('button', { name: 'Reload enum values' }),
    );

    await waitFor(() =>
      expect(showErrorNotification).toHaveBeenCalledWith({
        title: 'Error reloading enum values',
        error,
      }),
    );
    expect(invalidateEnumColumnValues).not.toHaveBeenCalled();
  });
});
