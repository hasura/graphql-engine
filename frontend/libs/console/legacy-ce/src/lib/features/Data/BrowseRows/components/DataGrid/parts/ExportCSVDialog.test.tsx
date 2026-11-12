import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { LS_KEYS } from '@hasura/shared/types';
import { DEFAULT_CSV_EXPORT_OPTIONS } from '@hasura/shared/utils';
import { ExportCSVDialog, getStoredCSVExportOptions } from './ExportCSVDialog';

class ResizeObserverStub {
  observe() {
    /* no-op */
  }
  unobserve() {
    /* no-op */
  }
  disconnect() {
    /* no-op */
  }
}
globalThis.ResizeObserver =
  globalThis.ResizeObserver ?? (ResizeObserverStub as never);

const setup = () => {
  const onExport = vi.fn();
  render(
    <ExportCSVDialog scope="table" onExport={onExport} onClose={vi.fn()} />,
  );
  return { onExport };
};

const submit = () =>
  userEvent.click(screen.getByRole('button', { name: 'Export' }));

describe('ExportCSVDialog', () => {
  beforeEach(() => {
    window.localStorage.clear();
  });

  it('exports with the default options', async () => {
    const { onExport } = setup();
    await submit();

    await waitFor(() =>
      expect(onExport).toHaveBeenCalledWith(DEFAULT_CSV_EXPORT_OPTIONS),
    );
  });

  it('exports with the chosen delimiter and remembers it', async () => {
    const { onExport } = setup();
    await userEvent.click(screen.getByRole('radio', { name: 'Semicolon (;)' }));
    await userEvent.click(
      screen.getByRole('checkbox', {
        name: 'Add a UTF-8 byte order mark (BOM)',
      }),
    );
    await submit();

    await waitFor(() => expect(onExport).toHaveBeenCalled());
    expect(onExport.mock.calls[0][0]).toMatchObject({
      delimiter: ';',
      includeBOM: true,
    });
    expect(getStoredCSVExportOptions()).toMatchObject({
      delimiter: 'semicolon',
      includeBOM: true,
    });
  });

  it('requires a custom delimiter', async () => {
    const { onExport } = setup();
    await userEvent.click(screen.getByRole('radio', { name: 'Custom' }));
    await submit();

    expect(await screen.findByText('Enter a delimiter')).toBeInTheDocument();
    expect(onExport).not.toHaveBeenCalled();
  });

  it('exports with a custom delimiter', async () => {
    const { onExport } = setup();
    await userEvent.click(screen.getByRole('radio', { name: 'Custom' }));
    await userEvent.type(await screen.findByPlaceholderText('e.g. ~'), '~');
    await submit();

    await waitFor(() => expect(onExport).toHaveBeenCalled());
    expect(onExport.mock.calls[0][0]).toMatchObject({ delimiter: '~' });
  });
});

describe('getStoredCSVExportOptions', () => {
  beforeEach(() => {
    window.localStorage.clear();
  });

  it('falls back to the defaults when the stored value is invalid', () => {
    window.localStorage.setItem(LS_KEYS.dataCSVExportOptions, '{not json');
    expect(getStoredCSVExportOptions()).toMatchObject({
      delimiter: 'comma',
      includeHeader: true,
    });

    window.localStorage.setItem(
      LS_KEYS.dataCSVExportOptions,
      JSON.stringify({ delimiter: 'nope' }),
    );
    expect(getStoredCSVExportOptions().delimiter).toBe('comma');
  });
});
