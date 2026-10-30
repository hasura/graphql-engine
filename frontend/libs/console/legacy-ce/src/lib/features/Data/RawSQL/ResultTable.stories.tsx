import type { Meta, StoryObj } from '@storybook/react-webpack5';
import { expect, userEvent, within } from 'storybook/test';
import ResultTable from './ResultTable';

const meta = {
  title: 'Features/Data/RawSQL/ResultTable',
  component: ResultTable,
  parameters: { layout: 'padded' },
} satisfies Meta<typeof ResultTable>;

export default meta;
type Story = StoryObj<typeof meta>;

// `run_sql` returns every cell as text, JSON and arrays included
const paragraph =
  'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod ' +
  'tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim ' +
  'veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea ' +
  'commodo consequat.';

const json = JSON.stringify({
  id: 'b7a1f0c2-5e7d-4c4a-9a51-2f3d8e6b1c90',
  tags: ['alpha', 'beta', 'gamma', 'delta'],
  address: { street: '221B Baker Street', city: 'London', zip: 'NW1 6XE' },
  preferences: { theme: 'dark', notifications: true, language: 'en-GB' },
});

// no whitespace to break on
const token = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9'.repeat(8);

const multiline = 'first line\nsecond line\n\tindented third line';

export const Basic: Story = {
  args: {
    headers: ['id', 'name', 'email'],
    rows: [
      ['1', 'Alice', 'alice@example.com'],
      ['2', 'Bob', 'bob@example.com'],
      ['3', 'Carol', 'carol@example.com'],
    ],
  },
};

export const LargeCells: Story = {
  args: {
    headers: ['id', 'description', 'payload', 'token', 'notes', 'deleted_at'],
    rows: Array.from({ length: 25 }, (_, i) => [
      String(i + 1),
      i % 3 === 0 ? paragraph : `short description ${i + 1}`,
      json,
      token,
      multiline,
      // nulls are left out of the width calculation
      null as unknown as string,
    ]),
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // widths are capped however long the content is
    for (const name of ['description', 'payload', 'token']) {
      await expect(canvas.getByRole('rowheader', { name })).toHaveStyle({
        width: '600px',
      });
    }

    // 25 rows are paginated 10 at a time
    const tokens = canvas.getAllByText(token);
    await expect(tokens).toHaveLength(10);

    // long values are cut to one line, and shown in full on hover
    await expect(tokens[0]).toHaveClass('truncate');
    await expect(tokens[0]).toHaveAttribute('title', token);
    await expect(tokens[0].scrollWidth).toBeGreaterThan(tokens[0].clientWidth);
  },
};

export const OpenRow: Story = {
  args: LargeCells.args,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(
      canvas.getAllByRole('button', { name: 'Open row' })[0],
    );

    // the dialog is rendered in a portal, outside of the story
    const dialog = within(
      await within(canvasElement.ownerDocument.body).findByRole('dialog'),
    );
    await expect(dialog.getByText('Result Row')).toBeVisible();
    await expect(dialog.getByText(paragraph)).toBeVisible();
    await expect(dialog.getByText(token)).toBeVisible();
    await expect(dialog.getByText('"preferences"')).toBeVisible();
  },
};

export const ManyColumns: Story = {
  args: {
    headers: Array.from({ length: 30 }, (_, i) => `column_with_long_name_${i}`),
    rows: Array.from({ length: 5 }, (_, r) =>
      Array.from({ length: 30 }, (_, c) => `value ${r}-${c} ${paragraph}`),
    ),
  },
};

// 10,000 rows of 30 short, medium and long values
const largeValue = (r: number, c: number) => {
  switch (c % 5) {
    case 0:
      return String(r * 30 + c);
    case 1:
      return `user name ${r}`;
    case 2:
      return r % 3 === 0 ? paragraph : `short text ${r}`;
    case 3:
      return json;
    default:
      return `2026-10-30T23:19:50.${String(r).padStart(6, '0')}Z`;
  }
};

export const LargeResult: Story = {
  args: {
    headers: Array.from({ length: 30 }, (_, c) => `column_${c}`),
    rows: Array.from({ length: 10000 }, (_, r) =>
      Array.from({ length: 30 }, (_, c) => largeValue(r, c)),
    ),
  },
};

export const DuplicateAndDottedColumnNames: Story = {
  args: {
    // e.g. `select a.id, b.id, a.payload as "a.payload" from a join b ...`
    headers: ['id', 'id', 'a.payload'],
    rows: [
      ['1', 'b7a1f0c2-5e7d-4c4a-9a51-2f3d8e6b1c90', paragraph],
      ['2', 'e3c9d4a8-1b2f-4f6e-8d7a-0c5b9e2f4a31', json],
    ],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const ids = canvas.getAllByRole('rowheader', { name: 'id' });
    await expect(ids).toHaveLength(2);
    // each `id` column is sized from its own values
    await expect(ids[0].style.width).not.toBe(ids[1].style.width);
    await expect(
      canvas.getByText('b7a1f0c2-5e7d-4c4a-9a51-2f3d8e6b1c90'),
    ).toBeVisible();
    await expect(
      canvas.getByRole('rowheader', { name: 'a.payload' }),
    ).toBeVisible();
  },
};
