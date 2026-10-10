import { useMemo, useState } from 'react';

export type FlameGraphNode = {
  name: string;
  /**
   * Size of the node. Widths are relative to the root's value.
   */
  value: number;
  /**
   * Native tooltip text; falls back to `name`.
   */
  tooltip?: string;
  backgroundColor?: string;
  color?: string;
  children?: FlameGraphNode[];
};

type FlameGraphProps = {
  data: FlameGraphNode;
  width: number;
  height: number;
};

type LaidOutNode = {
  id: string;
  node: FlameGraphNode;
  depth: number;
  // Both relative to the root, from 0 to 1.
  left: number;
  width: number;
};

const ROW_HEIGHT = 20;
// Narrower nodes are not drawn at all; narrower than this, labels are hidden.
const MIN_WIDTH = 1;
const MIN_WIDTH_FOR_LABEL = 12;

const layout = (root: FlameGraphNode): LaidOutNode[] => {
  const total = root.value || 1;
  const nodes: LaidOutNode[] = [];

  const visit = (
    node: FlameGraphNode,
    depth: number,
    left: number,
    id: string,
  ) => {
    nodes.push({ id, node, depth, left, width: node.value / total });
    let offset = left;
    node.children?.forEach((child, index) => {
      visit(child, depth + 1, offset, `${id}.${index}`);
      offset += child.value / total;
    });
  };

  visit(root, 0, 0, '0');
  return nodes;
};

/**
 * A minimal flame graph: one row per depth, each node as wide as its share of
 * the root's value. Clicking a node zooms in on it; clicking one of the dimmed
 * rows above zooms back out.
 */
export const FlameGraph = ({ data, width, height }: FlameGraphProps) => {
  const nodes = useMemo(() => layout(data), [data]);
  const [focusedId, setFocusedId] = useState('0');

  const focused = nodes.find((n) => n.id === focusedId) ?? nodes[0];
  const scale = (value: number) => (value / focused.width) * width;
  const rows = Math.max(...nodes.map((n) => n.depth)) + 1;

  return (
    <div
      className="relative overflow-y-auto overflow-x-hidden"
      style={{ width, height }}
    >
      <div className="relative" style={{ height: rows * ROW_HEIGHT }}>
        {nodes.map((laidOut) => {
          const { id, node, depth, left } = laidOut;
          const nodeWidth = scale(laidOut.width);
          const x = scale(left) - scale(focused.left);

          // Skip nodes too small to see, or outside the zoomed-in window.
          if (
            nodeWidth < MIN_WIDTH ||
            left + laidOut.width < focused.left ||
            left > focused.left + focused.width
          ) {
            return null;
          }

          const isDimmed = depth < focused.depth;

          return (
            <button
              key={id}
              type="button"
              title={node.tooltip ?? node.name}
              aria-label={node.name}
              onClick={() => setFocusedId(id)}
              className="absolute box-border cursor-pointer overflow-hidden border border-white p-0 text-left transition-all duration-200 ease-in-out"
              style={{
                left: x,
                top: depth * ROW_HEIGHT,
                width: nodeWidth,
                height: ROW_HEIGHT,
                backgroundColor: node.backgroundColor ?? '#ddd',
                color: node.color ?? '#000',
                opacity: isDimmed ? 0.5 : 1,
                // Keep the label visible when the node starts left of the view.
                paddingLeft: x < 0 ? -x : 0,
              }}
            >
              {nodeWidth >= MIN_WIDTH_FOR_LABEL ? (
                <span className="mx-1 block truncate font-sans text-xs leading-[18px] select-none">
                  {node.name}
                </span>
              ) : null}
            </button>
          );
        })}
      </div>
    </div>
  );
};
