import { getCloneRow, toCloneRowState } from './cloneRow';

describe('cloneRow router state', () => {
  it('reads back the row to clone', () => {
    const row = { id: 1, title: 'a' };
    expect(getCloneRow(toCloneRowState(row))).toEqual(row);
  });

  it.each([undefined, null, 'row', {}, { cloneRow: 'row' }])(
    'ignores a state without a row: %j',
    (state) => {
      expect(getCloneRow(state)).toBeUndefined();
    },
  );
});
