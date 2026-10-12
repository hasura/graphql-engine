import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';
import { InsertRowForm, InsertRowFormProps } from './InsertRowForm';

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

vi.mock('@hasura/metadata/data-source', () => ({
  useSupportedDataTypes: () => ({ data: undefined }),
}));

const setup = (props: Partial<InsertRowFormProps> = {}) =>
  render(
    <Theme>
      <InsertRowForm
        columns={[]}
        isInserting={false}
        isLoading={false}
        onInsertRow={vi.fn()}
        driver="postgres"
        {...props}
      />
    </Theme>,
  );

describe('InsertRowForm migration checkbox', () => {
  it('is hidden when creating migrations is not available', () => {
    setup();
    expect(screen.queryByText('This is a migration')).not.toBeInTheDocument();
  });

  it('toggles the migration flag', async () => {
    const onChange = vi.fn();
    setup({ migration: { checked: false, onChange } });

    await userEvent.click(
      screen.getByRole('checkbox', { name: 'This is a migration' }),
    );

    expect(onChange).toHaveBeenCalledWith(true);
  });
});
