import type { ComponentPropsWithoutRef } from 'react';
import type { ComponentMeta, ComponentStory } from '@storybook/react';

import React, { useEffect, useRef, useState } from 'react';

import { Button } from '@/new-components/Button';
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
} as ComponentMeta<typeof Analytics>;

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// REACT COMPONENT STORY
// #region
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// --------------------------------------------------
// STORY DEFINITION
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

const htmlElementPlaygroundArgs: ComponentPropsWithoutRef<typeof Analytics> = {
  name: 'elementName',
  children: null, // will be overwritten by the story
};
HtmlElementPlayground.args = htmlElementPlaygroundArgs;

// #endregion

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// REACT COMPONENT STORY
// #region
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------

export const ReactComponent: ComponentStory<typeof Analytics> = args => {
  return (
    <Analytics {...args}>
      <Component />
    </Analytics>
  );
};
ReactComponent.storyName = '⚙️ Passing a React component';

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const reactComponentArgs: ComponentPropsWithoutRef<typeof Analytics> = {
  name: 'componentName',
  children: null, // will be overwritten by the story
};
ReactComponent.args = reactComponentArgs;

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

// #endregion

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// REACT COMPONENT STORY
// #region
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------

export const ReactComponentThatAcceptHtmlAttributes: ComponentStory<
  typeof Analytics
> = args => {
  return (
    <Analytics {...args}>
      <ComponentThatAcceptHtmlAttributes />
    </Analytics>
  );
};
ReactComponentThatAcceptHtmlAttributes.storyName =
  '⚙️ Passing a React component that accept HTML attributes as props';

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const reactComponentThatAcceptHtmlAttributesArgs: ComponentPropsWithoutRef<
  typeof Analytics
> = {
  name: 'componentName',
  children: null, // will be overwritten by the story
  passHtmlAttributesToChildren: true,
};
ReactComponentThatAcceptHtmlAttributes.args =
  reactComponentThatAcceptHtmlAttributesArgs;

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

// #endregion

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// PASSING BUTTON COMPONENT STORY
// #region
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------

export const PassingButtonComponent: ComponentStory<
  typeof Analytics
> = args => {
  return (
    <Analytics {...args}>
      <Button>
        The Analytics component passes the HTML attributes to the Button (please
        inspect the DOM to find the rendered HTML attributes)
      </Button>
    </Analytics>
  );
};
PassingButtonComponent.storyName = '⚙️ Passing the Button component';

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const passingButtonComponentArgs: ComponentPropsWithoutRef<typeof Analytics> = {
  name: 'componentName',
  children: null, // will be overwritten by the story
  passHtmlAttributesToChildren: true,
};
PassingButtonComponent.args = passingButtonComponentArgs;

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
