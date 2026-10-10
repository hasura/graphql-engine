import { render } from '@testing-library/react';
import { createRef } from 'react';
import { AceEditor, AceEditorRef } from './index';
import { Theme } from '@radix-ui/themes';

it('probe', () => {
  const ref = createRef<AceEditorRef>();
  render(
    <Theme>
      <AceEditor ref={ref} mode="json" value={'{"a": 1, "b": true}'} />
    </Theme>,
  );
  const ed = (ref.current as any).editor;
  const s = ed.getSession();
  console.log(
    'MODE',
    s.$modeId,
    s.$mode?.$id,
    'THEME',
    ed.getTheme(),
    ed.renderer.theme?.cssClass,
  );
  console.log('TOKENS', JSON.stringify(s.getTokens(0)));
  console.log(
    'STYLES',
    Array.from(document.querySelectorAll('style'))
      .map((x) => x.id)
      .filter(Boolean)
      .join(','),
  );
});
