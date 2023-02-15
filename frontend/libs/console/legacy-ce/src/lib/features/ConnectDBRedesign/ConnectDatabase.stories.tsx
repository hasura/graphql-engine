import { hasuraToast } from '@/new-components/Toasts';
import { useArgs } from '@storybook/client-api';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import { ConnectDatabase } from './ConnectDatabase';

export default {
  component: ConnectDatabase,
  argTypes: {
    onEnableEnterpriseTrial: { action: 'Enable Enterprise Clicked' },
    onContactSales: { action: 'Contact Sales Clicked' },
  },
} as ComponentMeta<typeof ConnectDatabase>;

export const Primary: ComponentStory<typeof ConnectDatabase> = args => {
  const [, updateArgs] = useArgs();
  return (
    <ConnectDatabase
      {...args}
      onEnableEnterpriseTrial={() => {
        hasuraToast({
          message:
            'Missing EE Trial Forms here. Setting ee trial prop to active as a temporary measure.',
          title: 'Sign Up Not Implemented',
          toastOptions: {
            duration: 3000,
          },
        });
        updateArgs({ ...args, eeState: 'active' });
      }}
    />
  );
};

Primary.args = {
  eeState: 'inactive',
  initialDb: 'snowflake',
};
