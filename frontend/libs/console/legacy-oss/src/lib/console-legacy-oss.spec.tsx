import { render } from '@testing-library/react';

import ConsoleLegacyOss from './console-legacy-oss';

describe('ConsoleLegacyOss', () => {
  it('should render successfully', () => {
    const { baseElement } = render(<ConsoleLegacyOss />);
    expect(baseElement).toBeTruthy();
  });
});
