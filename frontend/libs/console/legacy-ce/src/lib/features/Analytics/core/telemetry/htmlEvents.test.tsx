import * as React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { Analytics } from '../../../Analytics';
import { setupTelemetryEventListeners } from './htmlEvents';

let mockTracker = jest.fn();

beforeEach(() => {
  mockTracker = jest.fn();
});

afterEach(() => {
  jest.clearAllMocks();
});

const useSetupTelemetryEventListeners = () => {
  React.useEffect(() => {
    const listenersCleaner = setupTelemetryEventListeners(mockTracker);
    return listenersCleaner;
  }, []);
};

test('tracks button clicks with analytics attribute correctly', async () => {
  const TestComponent = () => {
    useSetupTelemetryEventListeners();
    return (
      <Analytics name="button-component">
        <button onClick={() => null} data-testid="click-me">
          Click Me
        </button>
      </Analytics>
    );
  };

  await render(<TestComponent />);

  fireEvent.click(screen.getByTestId('click-me'));

  expect(mockTracker).toHaveBeenLastCalledWith('button-component', 'click');
});

test('tracks nested button clicks with analytics attribute correctly', async () => {
  const TestComponent = () => {
    useSetupTelemetryEventListeners();
    return (
      <Analytics name="nested-button-component">
        <button onClick={() => null}>
          <div>
            <span data-testid="click-me" onClick={() => null}>
              Click me
            </span>
          </div>
        </button>
      </Analytics>
    );
  };

  await render(<TestComponent />);

  fireEvent.click(screen.getByTestId('click-me'));

  expect(mockTracker).toHaveBeenLastCalledWith(
    'nested-button-component',
    'click'
  );
});

test('does not track button clicks without analytics attribute', async () => {
  const TestComponent = () => {
    useSetupTelemetryEventListeners();
    return (
      <button onClick={() => null} data-testid="click-me">
        Click Me
      </button>
    );
  };

  await render(<TestComponent />);

  fireEvent.click(screen.getByTestId('click-me'));

  expect(mockTracker).toHaveBeenCalledTimes(0);
});

test('tracks onchange with of input[type=text] with analytics attribute', async () => {
  const TestComponent = () => {
    useSetupTelemetryEventListeners();
    const [value, setValue] = React.useState('');
    return (
      <Analytics name="text-component">
        <input
          onChange={e => setValue(e.target.value)}
          value={value}
          type="text"
          data-testid="type-here"
        />
      </Analytics>
    );
  };

  await render(<TestComponent />);

  fireEvent.change(screen.getByTestId('type-here'), {
    target: { value: 'text' },
  });

  expect(mockTracker).toHaveBeenLastCalledWith('text-component', 'change');
});

test('tracks onchange with of a nested input[type=text] with analytics attribute', async () => {
  const TestComponent = () => {
    useSetupTelemetryEventListeners();
    const [value, setValue] = React.useState('');
    return (
      <Analytics name="text-component">
        <div>
          <input
            onChange={e => setValue(e.target.value)}
            value={value}
            type="text"
            data-testid="type-here"
          />
        </div>
      </Analytics>
    );
  };

  await render(<TestComponent />);

  fireEvent.change(screen.getByTestId('type-here'), {
    target: { value: 'text' },
  });

  expect(mockTracker).toHaveBeenLastCalledWith('text-component', 'change');
});

test('does not track onchange with of an input[type=text] if analytics attribute absent', async () => {
  const TestComponent = () => {
    useSetupTelemetryEventListeners();
    const [value, setValue] = React.useState('');
    return (
      <div>
        <input
          onChange={e => setValue(e.target.value)}
          value={value}
          type="text"
          data-testid="type-here"
        />
      </div>
    );
  };

  await render(<TestComponent />);

  fireEvent.change(screen.getByTestId('type-here'), {
    target: { value: 'text' },
  });

  expect(mockTracker).not.toHaveBeenCalled();
});

test('tracks onchange with of input[type=radio] with analytics attribute', async () => {
  const TestComponent = () => {
    useSetupTelemetryEventListeners();
    const [checked, setChecked] = React.useState(false);
    return (
      <Analytics name="radio-component">
        <input
          type="radio"
          onChange={() => setChecked(c => !c)}
          checked={checked}
          data-testid="toggle-radio"
        />
      </Analytics>
    );
  };

  await render(<TestComponent />);

  fireEvent.change(screen.getByTestId('toggle-radio'), {
    target: { value: 'text' },
  });

  expect(mockTracker).toHaveBeenLastCalledWith('radio-component', 'change');
});

test('tracks onchange with of input[type=checkbox] with analytics attribute', async () => {
  const TestComponent = () => {
    useSetupTelemetryEventListeners();
    const [checked, setChecked] = React.useState(false);
    return (
      <Analytics name="checkbox-component">
        <input
          type="checkbox"
          onChange={() => setChecked(c => !c)}
          checked={checked}
          data-testid="toggle-checkbox"
        />
      </Analytics>
    );
  };

  await render(<TestComponent />);

  fireEvent.change(screen.getByTestId('toggle-checkbox'), {
    target: { value: 'text' },
  });

  expect(mockTracker).toHaveBeenLastCalledWith('checkbox-component', 'change');
});

test('tracks onchange with of <select> with analytics attribute', async () => {
  const TestComponent = () => {
    const [value, setValue] = React.useState('');
    useSetupTelemetryEventListeners();
    return (
      <Analytics name="select-component">
        <select
          onChange={e => setValue(e.target.value)}
          value={value}
          data-testid="select-option"
        >
          <option value="value1">First</option>
          <option value="value2">Second</option>
        </select>
      </Analytics>
    );
  };

  await render(<TestComponent />);

  fireEvent.change(screen.getByTestId('select-option'), {
    target: { value: 'value2' },
  });

  expect(mockTracker).toHaveBeenLastCalledWith('select-component', 'change');
});
