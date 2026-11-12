import { describe, expect, it, vi } from 'vitest';
import { fireEvent, render, screen } from '@testing-library/react';
import { JsonInput } from './JsonInput';
import { ExpandableTextInput } from './ExpandableTextInput';

// Exercise the real toggle/state logic without Ace's browser layout machinery.
vi.mock('react-ace', async (importActual) => ({
  // the real module loads the `ace` global the editor extensions need
  ...(await importActual<object>()),
  default: ({
    value,
    onChange,
    annotations,
  }: {
    value: string;
    onChange: (value: string) => void;
    annotations?: unknown[];
  }) => (
    <textarea
      aria-label="Expanded editor"
      data-annotations={JSON.stringify(annotations ?? [])}
      value={value}
      onChange={(event) => onChange(event.target.value)}
    />
  ),
}));

const annotations = () =>
  JSON.parse(
    screen.getByLabelText('Expanded editor').dataset.annotations ?? '',
  );

const compact = '{"nested":{"items":[1,2]},"message":"hello  world"}';
const pretty = JSON.stringify(JSON.parse(compact), null, 2);

describe('JsonInput', () => {
  it('starts compact, expands prettified and collapses back to one line', () => {
    const onChange = vi.fn();
    const onInput = vi.fn();
    render(
      <JsonInput initialValue={pretty} onChange={onChange} onInput={onInput} />,
    );
    expect(screen.getByRole('textbox')).toHaveValue(compact);
    fireEvent.click(screen.getByTitle('Expand'));
    expect(screen.getByRole('textbox')).toHaveValue(pretty);
    fireEvent.click(screen.getByTitle('Collapse'));
    expect(screen.getByRole('textbox')).toHaveValue(compact);
    // Formatting is presentation-only: don't mark an untouched row dirty.
    expect(onChange).not.toHaveBeenCalled();
    expect(onInput).not.toHaveBeenCalled();
  });

  it('preserves edits and forwards callbacks without reformatting as you type', () => {
    const onChange = vi.fn();
    const onInput = vi.fn();
    render(<JsonInput onChange={onChange} onInput={onInput} />);
    fireEvent.change(screen.getByRole('textbox'), {
      target: { value: compact },
    });
    expect(onChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        target: expect.objectContaining({ value: compact }),
      }),
    );
    fireEvent.click(screen.getByTitle('Expand'));
    const edited = '{\n  "id": 9007199254740993,\n  "items": [ 3, 4 ]\n}';
    fireEvent.change(screen.getByRole('textbox'), {
      target: { value: edited },
    });
    expect(screen.getByRole('textbox')).toHaveValue(edited);
    expect(onChange).toHaveBeenLastCalledWith({ target: { value: edited } });
    expect(onInput).toHaveBeenLastCalledWith({ target: { value: edited } });
    fireEvent.click(screen.getByTitle('Collapse'));
    expect(screen.getByRole('textbox')).toHaveValue(
      '{"id":9007199254740993,"items":[3,4]}',
    );
    fireEvent.click(screen.getByTitle('Expand'));
    expect(screen.getByRole('textbox')).toHaveValue(
      '{\n  "id": 9007199254740993,\n  "items": [\n    3,\n    4\n  ]\n}',
    );
  });

  it.each(['', '{"unfinished":'])(
    'keeps invalid or empty drafts %j',
    (value) => {
      render(<JsonInput initialValue={value} />);
      fireEvent.click(screen.getByTitle('Expand'));
      expect(screen.getByRole('textbox')).toHaveValue(value);
      const draft = '{"new":';
      fireEvent.change(screen.getByRole('textbox'), {
        target: { value: draft },
      });
      fireEvent.click(screen.getByTitle('Collapse'));
      expect(screen.getByRole('textbox')).toHaveValue(draft);
      fireEvent.click(screen.getByTitle('Expand'));
      expect(screen.getByRole('textbox')).toHaveValue(draft);
    },
  );

  it('marks invalid JSON in the editor and the input, but not a blank one', () => {
    render(<JsonInput initialValue="" />);
    expect(screen.getByRole('textbox')).toHaveAttribute(
      'aria-invalid',
      'false',
    );
    fireEvent.click(screen.getByTitle('Expand'));
    expect(annotations()).toEqual([]);

    fireEvent.change(screen.getByRole('textbox'), {
      target: { value: '{\n  "id" 1\n}' },
    });
    expect(annotations()).toEqual([
      expect.objectContaining({ row: 1, type: 'error' }),
    ]);
    fireEvent.click(screen.getByTitle('Collapse'));
    expect(screen.getByRole('textbox')).toHaveAttribute('aria-invalid', 'true');

    fireEvent.change(screen.getByRole('textbox'), {
      target: { value: '{"id":1}' },
    });
    expect(screen.getByRole('textbox')).toHaveAttribute(
      'aria-invalid',
      'false',
    );
    fireEvent.click(screen.getByTitle('Expand'));
    expect(annotations()).toEqual([]);
  });

  it('does not format a non-JSON expandable text input', () => {
    const text = '  { "looks": "like json" }  ';
    render(<ExpandableTextInput initialValue={text} />);
    fireEvent.click(screen.getByTitle('Expand'));
    expect(screen.getByRole('textbox')).toHaveValue(text);
    fireEvent.click(screen.getByTitle('Collapse'));
    expect(screen.getByRole('textbox')).toHaveValue(text);
  });
});
