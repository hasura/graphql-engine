import type { Meta, StoryObj } from '@storybook/react';

import { useEffect, useRef, useState } from 'react';

import { Button } from '../../../new-components/Button';
import { Analytics } from './Analytics';

export default {
  title: 'components/Analytics 🛠',
  component: Analytics,
  parameters: {
    docs: {
      description: {
        component: `Useful to add all the HTML stuff needed to everything tracking. Add
- some HTML attributes to the passed HTML elements
- or add a <div style="display:contents"> around React components

Passing an HTML element is better because at the time of writing (Sep 2022) [Safari just fixed all its a11y problems with CSS display:contents](https://caniuse.com/css-display-contents) but it's still not widely used.`,
      },
      source: { type: 'code' },
    },
  },
} as Meta<typeof Analytics>;

export const HtmlElementPlayground: StoryObj<typeof Analytics> = {
  render: args => {
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
  },

  name: '⚙️ Passing an HTML element',
  args: {
    name: 'elementName',
  },
};

export const ReactComponent: StoryObj<typeof Analytics> = {
  render: args => {
    return (
      <Analytics {...args}>
        <Component />
      </Analytics>
    );
  },

  name: '⚙️ Passing a React component',
  args: {
    name: 'componentName',
  },
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

export const ReactComponentThatAcceptHtmlAttributes: StoryObj<
  typeof Analytics
> = {
  render: args => {
    return (
      <Analytics {...args}>
        <ComponentThatAcceptHtmlAttributes />
      </Analytics>
    );
  },

  name: '⚙️ Passing a React component that accept HTML attributes as props',
  args: {
    name: 'componentName',
    passHtmlAttributesToChildren: true,
  },
};

// --------------------------------------------------
// COMPONENTS
// --------------------------------------------------
function ComponentThatAcceptHtmlAttributes(props: Record<string, string>) {
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

export const PassingButtonComponent: StoryObj<typeof Analytics> = {
  render: args => {
    return (
      <Analytics {...args}>
        <Button>
          The Analytics component passes the HTML attributes to the Button
          (please inspect the DOM to find the rendered HTML attributes)
        </Button>
      </Analytics>
    );
  },

  name: '⚙️ Passing the Button component',
  args: {
    name: 'componentName',
    passHtmlAttributesToChildren: true,
  },
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
