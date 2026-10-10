import { render, screen, fireEvent } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import DerivedFrom from './DerivedFrom';

const renderDerivedFrom = (
  props: Partial<React.ComponentProps<typeof DerivedFrom>> = {},
) => {
  const toggleDerivation = vi.fn();
  const utils = render(
    <Theme>
      <DerivedFrom
        shouldDerive
        parentMutation="mutation { insert_user { id } }"
        toggleDerivation={toggleDerivation}
        {...props}
      />
    </Theme>,
  );
  return { ...utils, toggleDerivation };
};

describe('DerivedFrom', () => {
  it('renders nothing when there is no parent mutation', () => {
    renderDerivedFrom({ parentMutation: '' });
    expect(screen.queryByText('Derived operation')).not.toBeInTheDocument();
    expect(screen.queryByRole('checkbox')).not.toBeInTheDocument();
  });

  it('renders the derived-operation section when a parent mutation exists', () => {
    renderDerivedFrom();

    expect(screen.getByText('Derived operation')).toBeInTheDocument();
    expect(
      screen.getByText('Generate code with delegation to the derived mutation'),
    ).toBeInTheDocument();
    expect(screen.getByRole('checkbox')).toBeInTheDocument();
  });

  it('reflects the shouldDerive flag on the checkbox', () => {
    const { rerender, toggleDerivation } = renderDerivedFrom({
      shouldDerive: false,
    });
    expect(screen.getByRole('checkbox')).not.toBeChecked();

    rerender(
      <Theme>
        <DerivedFrom
          shouldDerive
          parentMutation="mutation { insert_user { id } }"
          toggleDerivation={toggleDerivation}
        />
      </Theme>,
    );
    expect(screen.getByRole('checkbox')).toBeChecked();
  });

  it('calls toggleDerivation when the checkbox is clicked', () => {
    const { toggleDerivation } = renderDerivedFrom({ shouldDerive: false });

    fireEvent.click(screen.getByRole('checkbox'));

    expect(toggleDerivation).toHaveBeenCalledTimes(1);
  });
});
