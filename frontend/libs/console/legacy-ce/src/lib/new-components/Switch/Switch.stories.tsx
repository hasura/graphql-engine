import React from 'react';
import { Meta } from '@storybook/react';
import { Switch } from './Switch';

export default {
  title: 'components/Switch',
  component: Switch,
} as Meta<typeof Switch>;

export const Off = () => <Switch />;

export const On = () => <Switch checked />;

export const Playground = () => {
  const [checked, setChecked] = React.useState(false);
  return <Switch checked={checked} onCheckedChange={setChecked} />;
};
