import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { z } from 'zod';
import { ReactSelectField } from './index';
import { SimpleForm } from '../../SimpleForm';

const options = ['json', 'jsonb', 'timestamp', 'timestamptz'].map((type) => ({
  value: type,
  label: type,
}));

const renderReactSelectField = (value: string | string[], multi = false) =>
  render(
    <SimpleForm
      onSubmit={() => {
        //
      }}
      schema={z.object({ type: z.any() })}
      options={{ defaultValues: { type: value } }}
    >
      <ReactSelectField name="type" options={options} multi={multi} />
    </SimpleForm>,
  );

describe('ReactSelectField', () => {
  it.each([
    ['jsonb', 'json'],
    ['timestamptz', 'timestamp'],
  ])(
    'selects only %s, not the option whose value is a substring of it',
    (value, substring) => {
      renderReactSelectField(value);

      expect(screen.getByText(value)).toBeInTheDocument();
      expect(screen.queryByText(substring)).not.toBeInTheDocument();
    },
  );

  it('selects every option of a multi select value', () => {
    renderReactSelectField(['json', 'timestamptz'], true);

    expect(screen.getByText('json')).toBeInTheDocument();
    expect(screen.getByText('timestamptz')).toBeInTheDocument();
    expect(screen.queryByText('jsonb')).not.toBeInTheDocument();
    expect(screen.queryByText('timestamp')).not.toBeInTheDocument();
  });

  describe('creatable', () => {
    const renderCreatable = (value?: string) => {
      const onSubmit = vi.fn();
      render(
        <SimpleForm
          onSubmit={onSubmit}
          schema={z.object({ type: z.any() })}
          options={{ defaultValues: { type: value } }}
        >
          <ReactSelectField name="type" options={options} creatable />
          <button type="submit">Submit</button>
        </SimpleForm>,
      );
      return { onSubmit };
    };

    it('shows a value that is not one of the options', () => {
      renderCreatable('text[]');
      expect(screen.getByText('text[]')).toBeInTheDocument();
    });

    it('accepts a typed value', async () => {
      const { onSubmit } = renderCreatable();

      // `[[` types a literal `[`
      await userEvent.type(screen.getByRole('combobox'), 'text[[]');
      await userEvent.click(await screen.findByText('Use "text[]"'));
      await userEvent.click(screen.getByRole('button', { name: 'Submit' }));

      await waitFor(() =>
        expect(onSubmit).toHaveBeenCalledWith(
          { type: 'text[]' },
          expect.anything(),
        ),
      );
    });
  });
});
