import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';
import type { MetadataTable, Source } from '@hasura/shared/types';
import { SetAsEnum } from './SetAsEnum';

const setTableAsEnum = vi.fn();
let isEnumValue = false;

vi.mock('@hasura/metadata/api', () => ({
  useMetadata: () => ({ data: isEnumValue }),
}));

vi.mock('@hasura/metadata/helpers', () => ({
  MetadataSelectors: { findMetadataTable: () => ({ is_enum: isEnumValue }) },
}));

vi.mock('../hooks/useSetTableAsEnum', () => ({
  useSetTableAsEnum: () => ({ setTableAsEnum, isPending: false }),
}));

const source = { name: 'default', kind: 'postgres' } as Source;
const table = { table: { name: 'colors', schema: 'public' } } as MetadataTable;

const setup = () =>
  render(
    <Theme>
      <SetAsEnum source={source} table={table} isView={false} />
    </Theme>,
  );

describe('SetAsEnum', () => {
  beforeEach(() => {
    setTableAsEnum.mockClear();
    isEnumValue = false;
  });

  it('shows the not-an-enum state and toggles it on', async () => {
    setup();
    expect(screen.getByText(/this table is not an enum/i)).toBeInTheDocument();
    await userEvent.click(screen.getByRole('switch'));
    expect(setTableAsEnum).toHaveBeenCalledWith(true);
  });

  it('reflects the enum state when the table is already an enum', () => {
    isEnumValue = true;
    setup();
    expect(
      screen.getByText(/this table is exposed as an enum/i),
    ).toBeInTheDocument();
    expect(screen.getByRole('switch')).toBeChecked();
  });
});
