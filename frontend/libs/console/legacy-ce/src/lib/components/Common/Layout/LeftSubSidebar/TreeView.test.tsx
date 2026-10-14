import { render, screen, fireEvent } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { TreeView } from './TreeView';

const renderTreeView = (props: {
  searchText?: string;
  currentItem?: { name: string; source: string };
}) =>
  render(
    <MemoryRouter>
      <TreeView
        service="tables"
        items={[{ name: 'orders', source: 'default' }]}
        getServiceEntityLink={(name) => `/data/${name}`}
        icon={<span data-testid="icon" />}
        {...props}
      />
    </MemoryRouter>,
  );

const item = () => screen.queryByText('orders');
const header = () => screen.getByText('default');

describe('TreeView (CollapsibleItems defaultOpen sync)', () => {
  it('is collapsed on initial render when defaultOpen is false', () => {
    renderTreeView({});
    expect(item()).not.toBeInTheDocument();
  });

  it('is expanded on initial render when defaultOpen is true', () => {
    renderTreeView({ currentItem: { name: 'orders', source: 'default' } });
    expect(item()).toBeInTheDocument();
  });

  it('opens/closes when the user clicks the source header', () => {
    renderTreeView({});
    expect(item()).not.toBeInTheDocument();

    fireEvent.click(header());
    expect(item()).toBeInTheDocument();

    fireEvent.click(header());
    expect(item()).not.toBeInTheDocument();
  });

  it('syncs the open state when the defaultOpen-derived prop changes', () => {
    const { rerender } = renderTreeView({});
    // collapsed (defaultOpen false)
    expect(item()).not.toBeInTheDocument();

    // searchText makes defaultOpen true -> opens
    rerender(
      <MemoryRouter>
        <TreeView
          service="tables"
          items={[{ name: 'orders', source: 'default' }]}
          getServiceEntityLink={(name) => `/data/${name}`}
          icon={<span data-testid="icon" />}
          searchText="ord"
        />
      </MemoryRouter>,
    );
    expect(item()).toBeInTheDocument();

    // removing searchText makes defaultOpen false again -> closes
    rerender(
      <MemoryRouter>
        <TreeView
          service="tables"
          items={[{ name: 'orders', source: 'default' }]}
          getServiceEntityLink={(name) => `/data/${name}`}
          icon={<span data-testid="icon" />}
        />
      </MemoryRouter>,
    );
    expect(item()).not.toBeInTheDocument();
  });

  it('does not enter an infinite render loop (renders once per stable prop set)', () => {
    // If the during-render sync were unguarded, React would throw
    // "Too many re-renders"; a successful render proves it is bounded.
    expect(() => renderTreeView({})).not.toThrow();
  });
});
