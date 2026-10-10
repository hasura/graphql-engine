import { render, screen, fireEvent } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import HeaderConfEditor from './HeaderConfEditor';

const renderEditor = (
  props: Partial<React.ComponentProps<typeof HeaderConfEditor>> = {},
) => {
  const toggleForwardClientHeaders = vi.fn();
  const setHeaders = vi.fn();
  render(
    <Theme>
      <HeaderConfEditor
        forwardClientHeaders={false}
        toggleForwardClientHeaders={toggleForwardClientHeaders}
        headers={[]}
        setHeaders={setHeaders}
        {...props}
      />
    </Theme>,
  );
  return { toggleForwardClientHeaders, setHeaders };
};

describe('HeaderConfEditor', () => {
  it('renders the label, description and forward-headers checkbox', () => {
    renderEditor();

    expect(screen.getByText('Headers')).toBeInTheDocument();
    expect(
      screen.getByText(
        'Headers Hasura will send to the webhook with the POST request.',
      ),
    ).toBeInTheDocument();
    expect(
      screen.getByText('Forward client headers to webhook'),
    ).toBeInTheDocument();
    expect(screen.getByRole('checkbox')).not.toBeChecked();
  });

  it('reflects the forwardClientHeaders flag', () => {
    renderEditor({ forwardClientHeaders: true });
    expect(screen.getByRole('checkbox')).toBeChecked();
  });

  it('calls toggleForwardClientHeaders when the checkbox is clicked', () => {
    const { toggleForwardClientHeaders } = renderEditor();

    fireEvent.click(screen.getByRole('checkbox'));

    expect(toggleForwardClientHeaders).toHaveBeenCalledTimes(1);
    expect(toggleForwardClientHeaders).toHaveBeenCalledWith(true);
  });

  it('disables the checkbox when disabled is true', () => {
    renderEditor({ disabled: true });
    expect(screen.getByRole('checkbox')).toBeDisabled();
  });
});
