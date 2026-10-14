import '@testing-library/jest-dom/vitest';
import { render, screen } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { vi } from 'vitest';
import { z } from 'zod';
import { ListMap } from './ListMap';
import { SimpleForm } from '../../SimpleForm';

const schema = z.object({
  type_selector: z.record(z.string(), z.string()),
});

const Wrapper = ({
  value,
  existingRelationshipName,
}: {
  value: Record<string, string>;
  existingRelationshipName?: string;
}) => (
  <Theme>
    <SimpleForm
      schema={schema}
      onSubmit={vi.fn()}
      options={{ defaultValues: { type_selector: {} } }}
    >
      <ListMap
        name="type_selector"
        existingRelationshipName={existingRelationshipName}
        value={value}
        from={{ options: ['apple', 'cherry', 'banana'] }}
        to={{ type: 'string', label: 'Ref' }}
      />
    </SimpleForm>
  </Theme>
);

describe('ListMap', () => {
  it('seeds the rows from the initial value', () => {
    render(<Wrapper value={{ apple: 'tomato' }} />);

    expect(screen.getByDisplayValue('tomato')).toBeInTheDocument();
  });

  it('re-seeds the rows when the value changes for an existing relationship', () => {
    const { rerender } = render(
      <Wrapper value={{ apple: 'tomato' }} existingRelationshipName="rel1" />,
    );

    expect(screen.getByDisplayValue('tomato')).toBeInTheDocument();

    rerender(
      <Wrapper value={{ banana: 'carrot' }} existingRelationshipName="rel1" />,
    );

    expect(screen.getByDisplayValue('carrot')).toBeInTheDocument();
    expect(screen.queryByDisplayValue('tomato')).not.toBeInTheDocument();
  });

  it('does not throw / loop when re-rendered with the same value', () => {
    const { rerender } = render(
      <Wrapper value={{ apple: 'tomato' }} existingRelationshipName="rel1" />,
    );

    rerender(
      <Wrapper value={{ apple: 'tomato' }} existingRelationshipName="rel1" />,
    );

    expect(screen.getByDisplayValue('tomato')).toBeInTheDocument();
  });
});
