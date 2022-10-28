import type { ComponentStory } from '@storybook/react';
import React, { useEffect, useRef, useState } from 'react';

import { Analytics } from '../Analytics';

// --------------------------------------------------
// STORY
// --------------------------------------------------
export const ReactComponentThatAcceptHtmlAttributes: ComponentStory<
  typeof Analytics
> = args => {
  return (
    <Analytics {...args}>
      <Component />
    </Analytics>
  );
};
ReactComponentThatAcceptHtmlAttributes.storyName =
  '⚙️ Passing a React component that accept HTML attributes as props';
ReactComponentThatAcceptHtmlAttributes.args = {
  name: 'componentName',
  passHtmlAttributesToChildren: true,
};

// --------------------------------------------------
// COMPONENTS
// --------------------------------------------------
function Component(props: Record<string, string>) {
  const htmlAttributes = props;
  const { ref, outerHtml } = useGetOuterHtml();

  return (
    <div ref={ref} {...htmlAttributes}>
      The Analytics component decorates the passed React component with some
      extra HTML attributes as props. The outer HTML of the rendered div is the
      following
      <br />
      <pre>{outerHtml}</pre>
    </div>
  );
}
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
