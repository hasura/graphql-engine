import { render } from '@testing-library/react';

import ConsoleDevTools from './ConsoleDevTools';

describe('ConsoleDevTools', () => {
  it('should render successfully', () => {
    const { baseElement } = render(<ConsoleDevTools />);
    expect(baseElement).toBeTruthy();
  });
});
