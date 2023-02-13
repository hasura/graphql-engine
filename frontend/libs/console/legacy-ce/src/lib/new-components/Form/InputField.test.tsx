import React from 'react';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { z } from 'zod';
import { SimpleForm, InputField, InputFieldProps } from '.';
import { Button } from '../Button';

const renderInputField = (
  props: Omit<InputFieldProps<any>, 'name'>,
  schema: any = z.object({ title: z.string() })
) => {
  const onSubmit = jest.fn();
  render(
    <SimpleForm
      onSubmit={args => {
        onSubmit(args);
      }}
      schema={schema}
    >
      <>
        <InputField name="title" {...props} />
        <Button type="submit">Submit</Button>
      </>
    </SimpleForm>
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
      })
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
      })
    );

    fireEvent.change(screen.getByRole('textbox'), {
      target: { value: 'This is the new value' },
    });

    fireEvent.click(
      screen.getByRole('button', {
        name: /submit/i,
      })
    );

    expect(await screen.findByRole('alert')).toBeInTheDocument();

    expect(
      screen.getByRole('alert', {
        name: /invalid email/i,
      })
    ).toBeInTheDocument();

    expect(onSubmit).toHaveBeenCalledTimes(0);
  });
});
