import { render } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { PermissionsLegend } from './PermissionsLegend';

describe('PermissionsLegend', () => {
  it('renders both the "allowed" and "not allowed" legend entries', () => {
    const { container } = render(
      <Theme>
        <PermissionsLegend />
      </Theme>,
    );

    // "allowed" and "not allowed" render as bare text (with non-breaking
    // spaces) next to their icons, so normalize NBSP to a plain space and
    // assert against the combined text content.
    const nbsp = String.fromCharCode(160);
    const text = (container.textContent ?? '').split(nbsp).join(' ');
    expect(text).toContain('- allowed');
    expect(text).toContain('- not allowed');
  });
});
