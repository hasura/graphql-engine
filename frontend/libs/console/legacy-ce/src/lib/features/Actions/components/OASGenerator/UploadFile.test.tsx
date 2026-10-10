import { render, screen, fireEvent } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { UploadFile } from './UploadFile';

const renderUploadFile = () => {
  const onChange = vi.fn();
  render(
    <Theme>
      <UploadFile onChange={onChange} />
    </Theme>,
  );
  return { onChange };
};

describe('UploadFile', () => {
  it('renders a "From File" button and a hidden file input', () => {
    renderUploadFile();

    expect(
      screen.getByRole('button', { name: /from file/i }),
    ).toBeInTheDocument();

    const input = screen.getByTestId('file');
    expect(input).toHaveAttribute('type', 'file');
    expect(input).not.toBeVisible();
  });

  it('forwards the button click to the hidden file input', () => {
    const clickSpy = vi.spyOn(HTMLInputElement.prototype, 'click');
    renderUploadFile();

    fireEvent.click(screen.getByRole('button', { name: /from file/i }));

    expect(clickSpy).toHaveBeenCalledTimes(1);
    clickSpy.mockRestore();
  });

  it('calls onChange when a file is selected', () => {
    const { onChange } = renderUploadFile();

    const input = screen.getByTestId('file') as HTMLInputElement;
    const file = new File(['{}'], 'spec.json', { type: 'application/json' });
    fireEvent.change(input, { target: { files: [file] } });

    expect(onChange).toHaveBeenCalledTimes(1);
  });
});
