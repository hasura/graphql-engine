import type { ComponentStory } from '@storybook/react';
import React, { useEffect, useRef, useState } from 'react';

import { Analytics } from '../Analytics';

// --------------------------------------------------
// STORY
// --------------------------------------------------
export const HtmlElementPlayground: ComponentStory<typeof Analytics> = args => {
  const { ref, outerHtml } = useGetOuterHtml();

  return (
    <Analytics {...args}>
      <div ref={ref} data-foo="bar">
        The Analytics component decorates the passed HTML element with some
        extra HTML attributes.
        <br />
        The passed HTML element initially had only the{' '}
        <pre>data-foo=&quot;bar&quot;</pre> HTML attribute, now its outer HTML
        is the following
        <br />
        <pre>{outerHtml}</pre>
      </div>
    </Analytics>
  );
};
HtmlElementPlayground.storyName = '⚙️ Passing an HTML element';
HtmlElementPlayground.args = {
  name: 'elementName',
};

// --------------------------------------------------
// UTILS
// --------------------------------------------------
function useGetOuterHtml() {
  const ref = useRef<HTMLDivElement>(null);
  const [outerHtml, setOuterHtml] = useState('');

  useEffect(() => {
    if (!ref.current) return;
    const element = ref.current;

    setOuterHtml(element.outerHTML.replace(element.innerHTML, ''));
  }, []);

  return { ref, outerHtml };
}
