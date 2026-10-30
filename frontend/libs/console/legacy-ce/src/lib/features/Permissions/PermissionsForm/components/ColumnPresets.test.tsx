import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';
import { FormProvider, useForm } from 'react-hook-form';
import { ColumnPresetsSection } from './ColumnPresets';

const Form = ({
  presets,
}: {
  presets: { columnName: string; presetType: string; columnValue: string }[];
}) => {
  const methods = useForm({ defaultValues: { checkType: 'custom', presets } });
  return (
    <Theme>
      <FormProvider {...methods}>
        <ColumnPresetsSection queryType="insert" columns={['id', 'name']} />
      </FormProvider>
    </Theme>
  );
};

// jsdom lacks ResizeObserver, which the Radix select depends on
beforeAll(() => {
  globalThis.ResizeObserver =
    globalThis.ResizeObserver ??
    class {
      observe() {
        /* noop */
      }
      unobserve() {
        /* noop */
      }
      disconnect() {
        /* noop */
      }
    };
});

// the section starts collapsed without presets
const open = () => userEvent.click(screen.getByText('Column presets'));

describe('ColumnPresetsSection', () => {
  it('shows a blank row to add a preset with when there are no presets', async () => {
    render(<Form presets={[]} />);
    await open();

    expect(await screen.findAllByText('Column Name')).toHaveLength(1);
    expect(screen.getByText('No Presets')).toBeInTheDocument();
    // the preset type and value wait for a column
    const [columnSelect, presetTypeSelect] = screen.getAllByRole('combobox');
    expect(columnSelect).toBeEnabled();
    expect(presetTypeSelect).toBeDisabled();
    expect(screen.getByPlaceholderText('Column value')).toBeDisabled();
  });

  it('adds a blank row after the existing presets', async () => {
    render(
      <Form
        presets={[{ columnName: 'id', presetType: 'static', columnValue: '1' }]}
      />,
    );

    // the preset, then the blank row
    expect(await screen.findAllByText('Column Name')).toHaveLength(1);
    expect(screen.getAllByRole('combobox')).toHaveLength(4);
    expect(screen.getByText('Presets: id')).toBeInTheDocument();
    // only the existing preset can be deleted
    expect(screen.getAllByRole('button', { name: 'Delete' })).toHaveLength(1);
  });
});
