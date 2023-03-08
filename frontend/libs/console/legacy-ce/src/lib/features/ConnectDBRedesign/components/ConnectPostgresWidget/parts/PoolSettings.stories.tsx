import { SimpleForm } from '../../../../../new-components/Form';
import { Button } from '../../../../../new-components/Button';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { z } from 'zod';

import { PoolSettings } from './PoolSettings';
import { poolSettingsSchema } from '../schema';

export default {
  component: PoolSettings,
} as ComponentMeta<typeof PoolSettings>;

export const PoolSettingsDefaultView: ComponentStory<
  typeof PoolSettings
> = () => (
  <SimpleForm
    onSubmit={data => console.log(data)}
    schema={z.object({
      poolSettings: poolSettingsSchema,
    })}
    options={{}}
  >
    <PoolSettings name="poolSetting" />
    <br />
    <Button type="submit" className="my-2">
      Submit
    </Button>
  </SimpleForm>
);
