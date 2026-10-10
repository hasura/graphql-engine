import { fireEvent, render, screen } from '@testing-library/react';
import { vi } from 'vitest';
import { FiSearch } from 'react-icons/fi';
import { Input } from '.';
import { Button } from '../../../Button';

describe('InputField', () => {
  it('renders an input', () => {
    render(<Input name="title" placeholder="my placeholder..." />);

    expect(screen.getByRole('textbox')).toBeInTheDocument();
    expect(
      screen.getByPlaceholderText('my placeholder...'),
    ).toBeInTheDocument();
  });

  it('renders an icon', () => {
    const { container } = render(<Input name="title" icon={FiSearch} />);

    // react-icons doesn't set an accessible role on the underlying <svg>,
    // so we assert on its presence directly rather than via getByRole('img').
    expect(container.querySelector('svg')).toBeInTheDocument();
  });

  it('renders disabled input', () => {
    render(<Input name="title" disabled />);

    expect(screen.getByRole('textbox')).toBeDisabled();
  });

  it('renders prepended label', () => {
    render(<Input name="title" prependLabel="my label" />);

    expect(screen.getByText('my label')).toBeInTheDocument();
  });

  it('renders appended label', () => {
    render(<Input name="title" appendLabel="my label" />);

    expect(screen.getByText('my label')).toBeInTheDocument();
  });

  it('renders a clear button', () => {
    const onClear = vi.fn();
    render(<Input name="title" clearable onClear={onClear} />);

    expect(screen.getByRole('button')).toBeInTheDocument();

    fireEvent.click(screen.getByRole('button'));

    expect(onClear).toHaveBeenCalled();
  });

  it('renders error state', () => {
    render(<Input name="title" isInvalid />);

    expect(screen.getByRole('textbox')).toBeInvalid();
  });

  it('renders right button', () => {
    render(<Input name="title" rightButton={<Button>my button</Button>} />);

    expect(screen.getByRole('textbox')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: 'my button' }),
    ).toBeInTheDocument();
  });

  it('renders invokes callbacks', () => {
    const onChange = vi.fn();
    const onInput = vi.fn();
    render(<Input name="title" onChange={onChange} onInput={onInput} />);

    fireEvent.change(screen.getByRole('textbox'), { target: { value: 'abc' } });
    fireEvent.input(screen.getByRole('textbox'), { target: { value: 'abc' } });

    expect(onChange).toHaveBeenCalled();
    expect(onInput).toHaveBeenCalled();
  });
});
