import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { vi } from 'vitest';
import { z } from 'zod';
import { InputField, InputFieldProps } from './index';
import { Button } from '../../../Button';
import { SimpleForm } from '../../SimpleForm';

const renderInputField = (
  props: Omit<InputFieldProps<any>, 'name'>,
  schema: any = z.object({ title: z.string() }),
) => {
  const onSubmit = vi.fn();
  render(
    <SimpleForm
      onSubmit={(args) => {
        onSubmit(args);
      }}
      schema={schema}
    >
      <>
        <InputField name="title" {...props} />
        <Button type="submit">Submit</Button>
      </>
    </SimpleForm>,
  );
  return {
    onSubmit,
  };
};

describe('InputField', () => {
  it('should display a input with label if provided with one', async () => {
    const { onSubmit } = renderInputField({
      label: 'My label',
      description: 'Some description',
    });

    expect(screen.getByText(/my label/i)).toBeInTheDocument();
    expect(screen.getByRole('textbox')).toBeInTheDocument();
    expect(screen.getByText(/some description/i)).toBeInTheDocument();

    fireEvent.change(screen.getByRole('textbox'), {
      target: { value: 'This is the new value' },
    });

    fireEvent.click(
      screen.getByRole('button', {
        name: /submit/i,
      }),
    );

    // This is to have a async callback to wait for the async validation from zod
    await waitFor(() => expect(screen.queryAllByRole('alert').length).toBe(0));

    expect(onSubmit).toHaveBeenCalledTimes(1);
    expect(onSubmit).toHaveBeenCalledWith({ title: 'This is the new value' });
  });

  it('should display a error if the validation is not correct', async () => {
    const { onSubmit } = renderInputField(
      {
        label: 'My label',
      },
      z.object({
        title: z.string().email(),
      }),
    );

    fireEvent.change(screen.getByRole('textbox'), {
      target: { value: 'This is the new value' },
    });

    fireEvent.click(
      screen.getByRole('button', {
        name: /submit/i,
      }),
    );

    const alert = await screen.findByRole('alert');
    expect(alert).toBeInTheDocument();

    // Note: the ARIA `alert` role only derives its accessible name from
    // `aria-label`/`aria-labelledby` (nameFrom: 'author'), never from text
    // content, so we assert on the rendered text directly instead of via
    // getByRole's `name` matcher.
    expect(alert).toHaveTextContent(/invalid email/i);

    expect(onSubmit).toHaveBeenCalledTimes(0);
  });
});
