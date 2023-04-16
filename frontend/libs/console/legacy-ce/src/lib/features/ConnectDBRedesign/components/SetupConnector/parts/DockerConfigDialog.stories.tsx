import { ComponentMeta, ComponentStory } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { DockerConfigDialog } from './DockerConfigDialog';

export default {
  component: DockerConfigDialog,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof DockerConfigDialog>;

export const Primary: ComponentStory<typeof DockerConfigDialog> = args => {
  return (
    <div className="max-w-3xl">
      <DockerConfigDialog {...args} />
    </div>
  );
};

Primary.args = {
  onCancel: () => {},
  onSetupSuccess: () => {},
};
