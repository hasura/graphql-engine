import React from 'react';
import { render, screen, fireEvent, within } from '@testing-library/react';

import LeftSidebar from '../../Sidebar/LeftSidebar';

const defaultProps = {
  appPrefix: '/actions',
  common: { currentAction: '' },
  readOnlyMode: false,
  actions: [],
};

describe('LeftSidebar', () => {
  it('renders as expected', async () => {
    const props = { ...defaultProps };

    render(<LeftSidebar {...props} />);

    expect(screen.getByPlaceholderText('search actions')).toBeInTheDocument();
    const actionsList = screen.getByRole('list');
    expect(actionsList).toHaveAttribute('data-test', 'actions-table-links');
    expect(screen.getByText('No actions available')).toBeInTheDocument();
  });

  it('renders the list of actions', async () => {
    const actions = [
      { name: 'actionOne', definition: { type: 'query' } },
      { name: 'actionTwo', definition: { type: 'query' } },
      { name: 'actionThree', definition: { type: 'query' } },
    ];
    const props = { ...defaultProps, actions };

    render(<LeftSidebar {...props} />);

    expect(screen.getByPlaceholderText('search actions')).toBeInTheDocument();

    const actionsList = screen.getByRole('list');
    expect(actionsList).toHaveAttribute('data-test', 'actions-table-links');

    expect(within(actionsList).getAllByRole('listitem')).toHaveLength(3);
    const action = within(actionsList).getByText('actionOne');
    expect(action).toHaveAttribute('data-test', 'actionOne');
  });

  it('searches the list case-insensitive', async () => {
    const actions = [
      { name: 'actionOne', definition: { type: 'query' } },
      { name: 'oddAction', definition: { type: 'query' } },
      { name: 'actionTwo', definition: { type: 'query' } },
      { name: 'actionThree', definition: { type: 'query' } },
    ];
    const props = { ...defaultProps, actions };

    render(<LeftSidebar {...props} />);

    const input = screen.getByPlaceholderText('search actions');
    const actionsList = screen.getByRole('list');

    expect(within(actionsList).getAllByRole('listitem')).toHaveLength(4);

    // Search lower case
    fireEvent.change(input, { target: { value: 'two' } });
    const actionItems1 = within(actionsList).getAllByRole('listitem');
    expect(actionItems1.length).toBe(1);
    expect(actionItems1[0]).toHaveTextContent('actionTwo');

    // Search mixed case
    fireEvent.change(input, { target: { value: 'Three' } });
    const actionItems2 = within(actionsList).getAllByRole('listitem');
    expect(actionItems2.length).toBe(1);
    expect(actionItems2[0]).toHaveTextContent('actionThree');

    // Search starts with
    fireEvent.change(input, { target: { value: 'odd' } });
    const actionItems3 = within(actionsList).getAllByRole('listitem');
    expect(actionItems3.length).toBe(1);
    expect(actionItems3[0]).toHaveTextContent('odd');
  });

  it('shows no matches text', async () => {
    const actions = [
      { name: 'actionOne', definition: { type: 'query' } },
      { name: 'actionTwo', definition: { type: 'query' } },
      { name: 'actionThree', definition: { type: 'query' } },
    ];
    const props = { ...defaultProps, actions };

    render(<LeftSidebar {...props} />);

    const input = screen.getByPlaceholderText('search actions');
    const actionsList = screen.getByRole('list');

    expect(within(actionsList).getAllByRole('listitem')).toHaveLength(3);

    // Search for non matching text
    fireEvent.change(input, { target: { value: 'ten' } });
    const actionItems1 = within(actionsList).getAllByRole('listitem');
    expect(actionItems1.length).toBe(1);
    expect(actionItems1[0]).toHaveTextContent('No actions available');
    expect(actionItems1[0]).toHaveAttribute(
      'data-test',
      'actions-sidebar-no-actions'
    );
  });
});
