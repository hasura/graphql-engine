import { useState } from 'react';
import { render, screen, waitFor } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import userEvent from '@testing-library/user-event';
import { QueryCollectionsOperationsSearchForm } from './QueryCollectionOperationsSearchForm';

// Regression for the exhaustive-deps fix. `setSearch` is typically an inline
// callback recreated every parent render that ALSO sets parent state (resetting
// a fresh array). If it were added to the effect deps directly it would notify
// on every render and loop. The fix propagates only when the searched value
// changes, via a ref. This test reproduces that unstable parent.

const propagateSpy = vi.fn();

// Mimics the real parent: a brand-new setSearch each render that sets parent
// state to a new array (which, with the buggy version, triggers a render loop).
const UnstableParent = () => {
  const [, setSelected] = useState<string[]>([]);
  return (
    <QueryCollectionsOperationsSearchForm
      setSearch={(s) => {
        propagateSpy(s);
        setSelected([]);
      }}
    />
  );
};

describe('QueryCollectionsOperationsSearchForm', () => {
  beforeEach(() => {
    propagateSpy.mockClear();
  });

  it('renders the search input', () => {
    render(
      <Theme>
        <UnstableParent />
      </Theme>,
    );
    expect(
      screen.getByPlaceholderText('Search Operations...'),
    ).toBeInTheDocument();
  });

  it('does not loop with an unstable setSearch (propagates once on mount)', async () => {
    render(
      <Theme>
        <UnstableParent />
      </Theme>,
    );
    // Give any erroneously-scheduled renders a chance to pile up.
    await waitFor(() => {
      expect(propagateSpy).toHaveBeenCalled();
    });
    // Mount propagates the initial value exactly once; no runaway loop.
    expect(propagateSpy.mock.calls.length).toBeLessThanOrEqual(1);
  });

  it('propagates the new value when the user types', async () => {
    const user = userEvent.setup();
    render(
      <Theme>
        <UnstableParent />
      </Theme>,
    );
    propagateSpy.mockClear();

    await user.type(screen.getByPlaceholderText('Search Operations...'), 'abc');

    await waitFor(() => {
      expect(propagateSpy).toHaveBeenLastCalledWith('abc');
    });
    // One propagation per keystroke at most — no runaway.
    expect(propagateSpy.mock.calls.length).toBeLessThanOrEqual(3);
  });
});
