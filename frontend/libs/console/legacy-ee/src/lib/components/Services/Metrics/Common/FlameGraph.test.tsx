import { fireEvent, render, screen } from '@testing-library/react';
import { FlameGraph, FlameGraphNode } from './FlameGraph';

const data: FlameGraphNode = {
  name: 'root',
  value: 100,
  tooltip: '100ms',
  children: [
    {
      name: 'a',
      value: 60,
      children: [{ name: 'a1', value: 30 }],
    },
    { name: 'b', value: 40 },
  ],
};

const node = (name: string) => screen.queryByRole('button', { name });
const widthOf = (name: string) => node(name)?.style.width;
const leftOf = (name: string) => node(name)?.style.left;

describe('FlameGraph', () => {
  it('sizes and places nodes relative to the root', () => {
    render(<FlameGraph data={data} width={200} height={200} />);

    expect(widthOf('root')).toBe('200px');
    expect(widthOf('a')).toBe('120px');
    expect(widthOf('b')).toBe('80px');
    expect(widthOf('a1')).toBe('60px');

    // siblings are laid out left to right, children start at their parent
    expect(leftOf('a')).toBe('0px');
    expect(leftOf('b')).toBe('120px');
    expect(leftOf('a1')).toBe('0px');

    // one 20px row per depth
    expect(node('a1')?.style.top).toBe('40px');
  });

  it('uses the tooltip, falling back to the name', () => {
    render(<FlameGraph data={data} width={200} height={200} />);
    expect(node('root')).toHaveAttribute('title', '100ms');
    expect(node('b')).toHaveAttribute('title', 'b');
  });

  it('hides labels on nodes narrower than 12px', () => {
    render(
      <FlameGraph
        data={{
          name: 'root',
          value: 100,
          children: [{ name: 'tiny', value: 5 }],
        }}
        width={200}
        height={200}
      />,
    );
    // 5% of 200px = 10px
    expect(node('tiny')).toBeInTheDocument();
    expect(node('tiny')).toBeEmptyDOMElement();
  });

  it('zooms into a clicked node and back out via a dimmed ancestor', () => {
    render(<FlameGraph data={data} width={200} height={200} />);

    fireEvent.click(node('a')!);
    // `a` now fills the width; its child scales with it
    expect(widthOf('a')).toBe('200px');
    expect(widthOf('a1')).toBe('100px');
    // `b` is pushed past the visible width
    expect(leftOf('b')).toBe('200px');
    // the root above is dimmed
    expect(node('root')?.style.opacity).toBe('0.5');

    fireEvent.click(node('root')!);
    expect(widthOf('a')).toBe('120px');
    expect(leftOf('b')).toBe('120px');
  });

  it('skips nodes entirely outside the zoomed-in window', () => {
    render(
      <FlameGraph
        data={{
          name: 'root',
          value: 100,
          children: [
            { name: 'a', value: 50 },
            { name: 'gap', value: 10 },
            { name: 'c', value: 40 },
          ],
        }}
        width={200}
        height={200}
      />,
    );

    fireEvent.click(node('a')!);
    expect(node('c')).not.toBeInTheDocument();
  });
});
