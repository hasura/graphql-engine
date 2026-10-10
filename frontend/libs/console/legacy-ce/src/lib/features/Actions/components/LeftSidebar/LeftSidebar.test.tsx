import { render, screen, fireEvent, within } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { Theme } from '@radix-ui/themes';
import { AppContext, defaultAppState } from '@hasura/shared/context';
import type { Action } from '@hasura/shared/types';
import LeftSidebar from './index';

type SidebarProps = React.ComponentProps<typeof LeftSidebar>;

const renderSidebar = (
  props: Partial<SidebarProps> = {},
  { readOnlyMode = false } = {},
) => {
  const finalProps: SidebarProps = {
    currentAction: '',
    actions: [],
    ...props,
  };

  return render(
    <AppContext.Provider value={{ ...defaultAppState, readOnlyMode }}>
      <MemoryRouter>
        <Theme>
          <LeftSidebar {...finalProps} />
        </Theme>
      </MemoryRouter>
    </AppContext.Provider>,
  );
};

// The child list is rendered inside a `<div data-test="actions-table-links">`
// by the shared `LeftSubSidebar`. Each action becomes a react-router link.
const getChildList = (container: HTMLElement) =>
  container.querySelector('[data-test="actions-table-links"]') as HTMLElement;

const makeActions = (names: Array<[string, 'query' | 'mutation']>): Action[] =>
  names.map(
    ([name, type]) => ({ name, definition: { type } }) as unknown as Action,
  );

describe('LeftSidebar', () => {
  it('renders the search box and an empty state when there are no actions', () => {
    const { container } = renderSidebar({ actions: [] });

    expect(screen.getByPlaceholderText('search actions')).toBeInTheDocument();
    expect(screen.getByText('Actions (0)')).toBeInTheDocument();

    const childList = getChildList(container);
    expect(childList).toBeInTheDocument();
    expect(within(childList).getByText('No actions available')).toBeVisible();
    expect(
      childList.querySelector('[data-test="actions-sidebar-no-actions"]'),
    ).toBeInTheDocument();
  });

  it('renders one link per action with the correct href and heading count', () => {
    const actions = makeActions([
      ['actionOne', 'query'],
      ['actionTwo', 'mutation'],
      ['actionThree', 'query'],
    ]);
    const { container } = renderSidebar({ actions });

    expect(screen.getByText('Actions (3)')).toBeInTheDocument();

    const childList = getChildList(container);
    const links = within(childList).getAllByRole('link');
    expect(links).toHaveLength(3);

    expect(within(childList).getByText('actionOne')).toBeInTheDocument();
    expect(links[0]).toHaveAttribute(
      'href',
      '/actions/manage/actionOne/modify',
    );
  });

  it('filters the list case-insensitively, keeping the original order', () => {
    const actions = makeActions([
      ['actionOne', 'query'],
      ['oddAction', 'query'],
      ['actionTwo', 'query'],
      ['actionThree', 'query'],
    ]);
    const { container } = renderSidebar({ actions });

    const input = screen.getByPlaceholderText('search actions');
    const childList = getChildList(container);

    expect(within(childList).getAllByRole('link')).toHaveLength(4);

    // lower-case substring match
    fireEvent.change(input, { target: { value: 'two' } });
    let links = within(childList).getAllByRole('link');
    expect(links).toHaveLength(1);
    expect(links[0]).toHaveTextContent('actionTwo');

    // mixed-case substring match
    fireEvent.change(input, { target: { value: 'Three' } });
    links = within(childList).getAllByRole('link');
    expect(links).toHaveLength(1);
    expect(links[0]).toHaveTextContent('actionThree');

    // a term contained in every entry keeps all of them in their original
    // order (the sidebar filters but does not re-sort matches).
    fireEvent.change(input, { target: { value: 'action' } });
    links = within(childList).getAllByRole('link');
    expect(links.map((l) => l.textContent)).toEqual([
      'actionOne',
      'oddAction',
      'actionTwo',
      'actionThree',
    ]);
  });

  it('shows the "no actions" message when the search matches nothing', () => {
    const actions = makeActions([
      ['actionOne', 'query'],
      ['actionTwo', 'query'],
    ]);
    const { container } = renderSidebar({ actions });

    const input = screen.getByPlaceholderText('search actions');
    fireEvent.change(input, { target: { value: 'does-not-exist' } });

    const childList = getChildList(container);
    expect(within(childList).queryAllByRole('link')).toHaveLength(0);
    expect(within(childList).getByText('No actions available')).toBeVisible();
    expect(screen.getByText('Actions (0)')).toBeInTheDocument();
  });

  it('renders the default "Create" add button when OpenAPI import is not allowed', () => {
    renderSidebar({ actions: [], allowOpenApiImport: false });

    const addButton = screen.getByRole('button', { name: 'Create' });
    expect(addButton).toHaveAttribute('data-test', 'actions-sidebar-add-table');
  });

  it('hides the default add button in read-only mode', () => {
    renderSidebar(
      { actions: [], allowOpenApiImport: false },
      { readOnlyMode: true },
    );

    expect(
      screen.queryByRole('button', { name: 'Create' }),
    ).not.toBeInTheDocument();
  });

  it('renders the OpenAPI import dropdown trigger when import is allowed', () => {
    renderSidebar({ actions: [], allowOpenApiImport: true });

    // The dropdown trigger replaces the plain add link and is not a router link.
    const trigger = screen.getByRole('button', { name: 'Create' });
    expect(trigger).toBeInTheDocument();
    expect(trigger).not.toHaveAttribute(
      'data-test',
      'actions-sidebar-add-table',
    );
  });
});
