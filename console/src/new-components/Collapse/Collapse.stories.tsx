import React from 'react';
import { Story, Meta } from '@storybook/react';

import { Collapse, CollapseProps, CollapseHeaderProps } from './Collapse';

export default {
  title: 'Components/Collapse',
  component: Collapse,
} as Meta;

interface Props extends CollapseProps {
  header: CollapseHeaderProps;
  children: React.ReactNode;
}

export const Default: Story<Props> = args => (
  <Collapse {...args}>
    {args.header && <Collapse.Header>{args.header?.children}</Collapse.Header>}
    <Collapse.Content>{args.children}</Collapse.Content>
  </Collapse>
);
Default.args = {
  title: 'The Title',
  children:
    'Lorem ipsum dolor, sit amet consectetur adipisicing elit. Natus vero quo reiciendis consequuntur ut nobis autem facere beatae error, consectetur unde nostrum repudiandae possimus atque repellendus perspiciatis totam hic enim?',
  defaultOpen: true,
};

export const DefaultIsClosed: Story<Props> = args => (
  <Collapse {...args}>
    {args.header && <Collapse.Header>{args.header?.children}</Collapse.Header>}
    <Collapse.Content>{args.children}</Collapse.Content>
  </Collapse>
);
DefaultIsClosed.args = {
  ...Default.args,
  defaultOpen: false,
};

export const CustomHeader: Story<Props> = args => (
  <Collapse {...args}>
    {args.header && <Collapse.Header>{args.header?.children}</Collapse.Header>}
    <Collapse.Content>{args.children}</Collapse.Content>
  </Collapse>
);
CustomHeader.args = {
  header: {
    children: <div className="bg-red-400">This is a custom header</div>,
  },
  children:
    'Lorem ipsum dolor sit, amet consectetur adipisicing elit. Eum enim placeat dignissimos rem? Doloribus animi rerum consequatur labore aut facere dolor cum architecto consequuntur, quo tempora porro doloremque distinctio perferendis.',
};

export const WithToolTip: Story<Props> = args => (
  <Collapse {...args}>
    {args.header && <Collapse.Header>{args.header?.children}</Collapse.Header>}
    <Collapse.Content>{args.children}</Collapse.Content>
  </Collapse>
);
WithToolTip.args = {
  ...Default.args,
  tooltip: 'A tooltip',
  status: '- some status',
};
