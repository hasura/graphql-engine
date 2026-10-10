import { render, screen, fireEvent } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import type { CustomTypeObjectRelationship } from '@hasura/shared/types';
import RelationshipsTable from './RelationshipsTable';

const relationshipOne: CustomTypeObjectRelationship = {
  name: 'user_rel',
  type: 'object',
  source: 'default',
  remote_table: { schema: 'public', name: 'users' },
  field_mapping: { id: 'user_id' },
};

const relationshipTwo: CustomTypeObjectRelationship = {
  name: 'orders_rel',
  type: 'array',
  source: 'other_source',
  remote_table: { schema: 'public', name: 'orders' },
  field_mapping: { id: 'customer_id' },
};

const renderTable = (
  props: Partial<React.ComponentProps<typeof RelationshipsTable>> = {},
) => {
  const onEdit = vi.fn();
  const onRemove = vi.fn();

  render(
    <Theme>
      <RelationshipsTable
        typeName="SampleType"
        relationships={[relationshipOne, relationshipTwo]}
        readOnlyMode={false}
        onEdit={onEdit}
        onRemove={onRemove}
        {...props}
      />
    </Theme>,
  );

  return { onEdit, onRemove };
};

describe('RelationshipsTable', () => {
  it('renders an empty-state card when there are no relationships', () => {
    renderTable({ relationships: [] });

    expect(screen.getByText('No relationships found.')).toBeInTheDocument();
    expect(
      screen.getByText('No relationships have been added for this type yet.'),
    ).toBeInTheDocument();
    expect(screen.queryByRole('table')).not.toBeInTheDocument();
  });

  it('renders a row per relationship with name, type and source', () => {
    renderTable();

    expect(screen.getByRole('table')).toBeInTheDocument();
    expect(screen.getByText('user_rel')).toBeInTheDocument();
    expect(screen.getByText('orders_rel')).toBeInTheDocument();
    expect(screen.getByText('object')).toBeInTheDocument();
    expect(screen.getByText('array')).toBeInTheDocument();
    expect(screen.getByText('default')).toBeInTheDocument();
    expect(screen.getByText('other_source')).toBeInTheDocument();
  });

  it('invokes onEdit / onRemove with the relationship that was actioned', () => {
    const { onEdit, onRemove } = renderTable();

    const editButtons = screen.getAllByRole('button', { name: /edit/i });
    const removeButtons = screen.getAllByRole('button', { name: /remove/i });
    expect(editButtons).toHaveLength(2);
    expect(removeButtons).toHaveLength(2);

    fireEvent.click(editButtons[0]);
    expect(onEdit).toHaveBeenCalledTimes(1);
    expect(onEdit).toHaveBeenCalledWith(relationshipOne);

    fireEvent.click(removeButtons[1]);
    expect(onRemove).toHaveBeenCalledTimes(1);
    expect(onRemove).toHaveBeenCalledWith(relationshipTwo);
  });

  it('hides the Edit and Remove actions in read-only mode', () => {
    renderTable({ readOnlyMode: true });

    // rows are still rendered, only the action buttons are gone
    expect(screen.getByText('user_rel')).toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /edit/i }),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /remove/i }),
    ).not.toBeInTheDocument();
  });
});
