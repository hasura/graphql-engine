import React from 'react';
import { ComponentMeta } from '@storybook/react';
import { ToolTip } from './Tooltip';

export default {
  title: 'components/Tooltip',
  component: ToolTip,
  parameters: {
    layout: 'centered',
  },
} as ComponentMeta<typeof ToolTip>;

const longMessage =
  'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed est nulla, aliquet elementum lectus vitae, eleifend semper ante. Nam tempor sollicitudin erat vitae posuere. Vivamus lobortis lorem vitae mauris convallis fermentum. Donec semper tincidunt tincidunt. Etiam pretium eu elit sit amet posuere. Quisque et mollis sem. Etiam at nibh et turpis viverra fermentum sed consequat lectus. Vivamus porttitor a nulla id malesuada. Phasellus lorem neque, facilisis quis mollis eu, scelerisque a nulla. Donec non nisl tempor, lacinia ante ut, interdum arcu. Etiam in arcu arcu. Integer finibus scelerisque purus non maximus. Suspendisse potenti. Curabitur tincidunt, mauris sit amet mollis tristique, odio est posuere diam, sed lobortis tellus leo quis neque. Sed ornare magna ut lorem vehicula, id elementum lectus condimentum.';

export const Showcase = () => (
  <div className="space-y-4">
    <ToolTip message={longMessage} />
    <ToolTip message="Lorem ipsum dolor sit amet." defaultOpen />
    <ToolTip message="Lorem ipsum dolor sit amet." side="left" />
    <ToolTip message="Lorem ipsum dolor sit amet." side="bottom" />
    <ToolTip message="Lorem ipsum dolor sit amet." side="top" />
  </div>
);
