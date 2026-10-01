import React from 'react';
import { fireEvent, render, screen } from '@testing-library/react';
import { z } from 'zod';
import { Button } from '../Button';
import { SimpleForm } from '../Form';
import { RequestHeadersSelector } from './RequestHeadersSelector';
import { requestHeadersSelectorSchema } from './schema';

const schema = z.object({
  headers: requestHeadersSelectorSchema,
});

describe('RequestHeadersSelector', () => {
  it('notifies the caller when a header row is added', () => {
    const onAdd = jest.fn();

    render(
      <SimpleForm
        options={{ defaultValues: { headers: [] } }}
        onSubmit={jest.fn()}
        schema={schema}
      >
        <>
          <RequestHeadersSelector
            name="headers"
            addButtonText="Add introspection headers"
            onAdd={onAdd}
          />
          <Button type="submit">Submit</Button>
        </>
      </SimpleForm>
    );

    fireEvent.click(
      screen.getByRole('button', { name: 'Add introspection headers' })
    );

    expect(onAdd).toHaveBeenCalledTimes(1);
    expect(screen.getAllByRole('textbox')).toHaveLength(2);
  });
});
