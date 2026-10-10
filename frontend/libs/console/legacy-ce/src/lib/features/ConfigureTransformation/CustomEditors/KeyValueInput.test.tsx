import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';
import { NameValue } from '@hasura/shared/types';
import KeyValueInput from './KeyValueInput';

// Stateful harness: KeyValueInput is controlled, so the parent owns `pairs`.
const Harness = ({ initial }: { initial: NameValue[] }) => {
  const [pairs, setPairs] = React.useState<NameValue[]>(initial);
  return (
    <Theme>
      <KeyValueInput pairs={pairs} setPairs={setPairs} testId="t" />
    </Theme>
  );
};

const keyInputs = () => screen.getAllByPlaceholderText('Key...');
const valueInputs = () => screen.getAllByPlaceholderText('Value...');

describe('KeyValueInput row keys (object-identity)', () => {
  it('typing a full key/value pair updates it and appends a fresh empty row', async () => {
    const user = userEvent.setup();
    render(<Harness initial={[{ name: '', value: '' }]} />);

    // one row to start
    expect(keyInputs()).toHaveLength(1);

    await user.type(keyInputs()[0], 'authorization');
    await user.type(valueInputs()[0], 'Bearer x');

    expect(keyInputs()[0]).toHaveValue('authorization');
    expect(valueInputs()[0]).toHaveValue('Bearer x');
    // once the last row is fully filled, a new empty row is appended
    expect(keyInputs()).toHaveLength(2);
    expect(keyInputs()[1]).toHaveValue('');
  });

  it('removing an earlier row keeps the later rows correct AND preserves focus on an unremoved row (regression vs index keys)', () => {
    render(
      <Harness
        initial={[
          { name: 'A', value: '1' },
          { name: 'B', value: '2' },
          { name: 'C', value: '3' },
        ]}
      />,
    );

    // focus the last row's (C) value input, then remove the FIRST row (A).
    const cValueBefore = valueInputs()[2];
    cValueBefore.focus();
    expect(cValueBefore).toHaveFocus();

    // rows 0 and 1 render a Remove button (the last row does not). fireEvent so
    // the click itself does not move focus onto the button.
    const removeButtons = screen.getAllByRole('button', { name: 'Remove' });
    expect(removeButtons).toHaveLength(2);
    fireEvent.click(removeButtons[0]);

    // list is now [B, C], values intact and in order
    expect(valueInputs()).toHaveLength(2);
    expect(valueInputs()[0]).toHaveValue('2');
    expect(valueInputs()[1]).toHaveValue('3');

    // C moved from index 2 to index 1 but is the SAME element (stable
    // object-identity key), so focus is retained. With index keys React would
    // have unmounted C's node and focus would be lost.
    expect(valueInputs()[1]).toHaveFocus();
  });
});
