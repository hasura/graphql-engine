import React from 'react';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { z } from 'zod';
import { Form, InputField, InputFieldProps } from '@/new-components/Form';

type FormValues = {
  title: string;
};

const renderInputField = (
  props: Omit<InputFieldProps, 'name'>,
  schema: any = z.object({ title: z.string() })
) => {
  const onSubmit = jest.fn();
  render(
    <Form<FormValues>
      onSubmit={args => {
        console.log('AAAA', args);
        onSubmit(args);
      }}
      schema={schema}
    >
      {() => (
        <>
          <InputField name="title" {...props} />
          <button type="submit">Submit</button>
        </>
      )}
    </Form>
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
