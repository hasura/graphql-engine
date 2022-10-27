import type { ComponentMeta } from '@storybook/react';

import { Analytics } from '../Analytics';

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

export { HtmlElementPlayground } from './HtmlElement.story';

export { ReactComponent } from './ReactComponent.story';
export { ReactComponentThatAcceptHtmlAttributes } from './ReactComponentThatAcceptHtmlAttributes.story';
