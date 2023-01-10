import type { ComponentStory } from '@storybook/react';
import React, { useEffect, useRef, useState } from 'react';

import { Analytics } from '../Analytics';

// --------------------------------------------------
// STORY
// --------------------------------------------------
export const ReactComponent: ComponentStory<typeof Analytics> = args => {
  return (
    <Analytics {...args}>
      <Component />
    </Analytics>
  );
};
ReactComponent.storyName = '⚙️ Passing a React component';
ReactComponent.args = {
  name: 'componentName',
};

// --------------------------------------------------
// COMPONENTS
// --------------------------------------------------
function Component() {
  const { ref, outerHtml } = useGetParentOuterHtml();

  return (
    <div ref={ref}>
      The passed React component has been wrapped by the following element
      <br />
      <pre>{outerHtml}</pre>
    </div>
  );
}
// --------------------------------------------------
// UTILS
// --------------------------------------------------
function useGetParentOuterHtml() {
  const ref = useRef<HTMLDivElement>(null);
  const [outerHtml, setOuterHtml] = useState('');

  useEffect(() => {
    if (!ref.current || !ref.current.parentElement) return;
    const element = ref.current.parentElement;

    setOuterHtml(element.outerHTML.replace(element.innerHTML, ''));
  }, []);

  return { ref, outerHtml };
}
