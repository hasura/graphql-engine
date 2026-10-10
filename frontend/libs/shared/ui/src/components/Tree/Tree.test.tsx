import { fireEvent, render, screen } from '@testing-library/react';
import { vi } from 'vitest';
import { Tree, TreeDataNode } from '.';

const treeData: TreeDataNode[] = [
  {
    key: 'db',
    title: 'db',
    children: [
      {
        key: 'schema',
        title: 'schema',
        selectable: false,
        children: [{ key: 'table', title: 'table' }],
      },
    ],
  },
  { key: 'other', title: 'other' },
];

const expandButtons = () => screen.queryAllByRole('button', { name: 'Expand' });

describe('Tree', () => {
  it('renders only root nodes until expanded', () => {
    render(<Tree treeData={treeData} />);

    expect(screen.getByText('db')).toBeInTheDocument();
    expect(screen.getByText('other')).toBeInTheDocument();
    expect(screen.queryByText('schema')).not.toBeInTheDocument();
  });

  it('expands and collapses on its own when uncontrolled', () => {
    const onExpand = vi.fn();
    render(<Tree treeData={treeData} onExpand={onExpand} />);

    fireEvent.click(expandButtons()[0]);
    expect(screen.getByText('schema')).toBeInTheDocument();
    expect(onExpand).toHaveBeenLastCalledWith(
      expect.objectContaining({ key: 'db' }),
      true,
    );

    fireEvent.click(screen.getByRole('button', { name: 'Collapse' }));
    expect(screen.queryByText('schema')).not.toBeInTheDocument();
    expect(onExpand).toHaveBeenLastCalledWith(
      expect.objectContaining({ key: 'db' }),
      false,
    );
  });

  it('only shows a toggle for nodes with children', () => {
    render(<Tree treeData={treeData} />);
    // `db` has children, `other` does not
    expect(expandButtons()).toHaveLength(1);
  });

  it('expands the ancestors of defaultExpandedKeys', () => {
    render(<Tree treeData={treeData} defaultExpandedKeys={['table']} />);
    expect(screen.getByText('table')).toBeInTheDocument();
  });

  it('follows expandedKeys when controlled', () => {
    const onExpand = vi.fn();
    const { rerender } = render(
      <Tree treeData={treeData} expandedKeys={[]} onExpand={onExpand} />,
    );

    fireEvent.click(expandButtons()[0]);
    expect(onExpand).toHaveBeenCalledWith(
      expect.objectContaining({ key: 'db' }),
      true,
    );
    // still collapsed until the parent passes the new keys
    expect(screen.queryByText('schema')).not.toBeInTheDocument();

    rerender(
      <Tree treeData={treeData} expandedKeys={['db']} onExpand={onExpand} />,
    );
    expect(screen.getByText('schema')).toBeInTheDocument();
  });

  it('checks via the checkbox and via the title, and respects checkable/disabled', () => {
    const onCheck = vi.fn();
    render(
      <Tree
        checkable
        treeData={[
          { key: 'a', title: 'a' },
          { key: 'b', title: 'b', checkable: false },
          { key: 'c', title: 'c', disabled: true },
        ]}
        checkedKeys={['a']}
        onCheck={onCheck}
      />,
    );

    const checkboxes = screen.getAllByRole('checkbox');
    // `b` has no checkbox
    expect(checkboxes).toHaveLength(2);
    expect(checkboxes[0]).toBeChecked();
    expect(checkboxes[1]).toBeDisabled();

    fireEvent.click(checkboxes[0]);
    expect(onCheck).toHaveBeenLastCalledWith(
      expect.objectContaining({ key: 'a' }),
      false,
    );

    // clicking the title toggles the checkbox, like rc-tree
    fireEvent.click(screen.getByText('a'));
    expect(onCheck).toHaveBeenLastCalledWith(
      expect.objectContaining({ key: 'a' }),
      false,
    );

    onCheck.mockClear();
    fireEvent.click(screen.getByText('b'));
    fireEvent.click(screen.getByText('c'));
    expect(onCheck).not.toHaveBeenCalled();
  });

  it('selects nodes on title click, skipping non-selectable ones', () => {
    const onSelect = vi.fn();
    render(
      <Tree
        selectable
        treeData={treeData}
        defaultExpandedKeys={['table']}
        onSelect={onSelect}
      />,
    );

    fireEvent.click(screen.getByText('schema'));
    expect(onSelect).not.toHaveBeenCalled();

    fireEvent.click(screen.getByText('table'));
    expect(onSelect).toHaveBeenCalledWith(
      expect.objectContaining({ key: 'table' }),
    );
    expect(
      screen.getByRole('treeitem', { name: 'table', selected: true }),
    ).toBeInTheDocument();
  });

  it('calls onNodeClick for every title click', () => {
    const onNodeClick = vi.fn();
    render(<Tree treeData={treeData} onNodeClick={onNodeClick} />);

    fireEvent.click(screen.getByText('other'));
    expect(onNodeClick).toHaveBeenCalledWith(
      expect.objectContaining({ key: 'other' }),
    );
  });
});
