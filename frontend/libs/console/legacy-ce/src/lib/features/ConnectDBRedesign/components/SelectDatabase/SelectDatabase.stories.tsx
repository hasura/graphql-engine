import { SelectDatabase } from '.';
import { hasuraToast } from '../../../../new-components/Toasts';
import { useArgs } from '@storybook/client-api';
import { ComponentMeta, ComponentStory } from '@storybook/react';

export default {
  component: SelectDatabase,
  argTypes: {
    onEnableEnterpriseTrial: { action: 'Enable Enterprise Clicked' },
    onContactSales: { action: 'Contact Sales Clicked' },
  },
} as ComponentMeta<typeof SelectDatabase>;

export const Primary: ComponentStory<typeof SelectDatabase> = args => {
  const [, updateArgs] = useArgs();
  return (
    <div className="max-w-3xl">
      Note: This container has a max width set. When rendering this component
      keep width in mind to avoid it growing too large.
      <SelectDatabase
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
    </div>
  );
};
Primary.args = {
  eeState: 'inactive',
  initialDb: 'snowflake',
};
