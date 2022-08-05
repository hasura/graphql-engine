import { render } from '@testing-library/react';
import ConsoleLegacyPro from './console-legacy-pro';
describe('ConsoleLegacyPro', () => {
  it('should render successfully', () => {
    const { baseElement } = render(<ConsoleLegacyPro />);
    expect(baseElement).toBeTruthy();
  });
});
