import React from 'react';
import { Button } from './Button';

export default {
  title: 'Example/Button',
  component: Button,
  parameters: {
    myAddonParameter: `
<MyComponent boolProp scalarProp={1} complexProp={{ foo: 1, bar: '2' }}>
  <SomeOtherComponent funcProp={(a) => a.id} />
</MyComponent>
`,
  },
};

const Template = args => <Button {...args} />;

export const Primary = Template.bind({});
Primary.args = {
  primary: true,
  label: 'Button',
};

export const PrimaryProLite = Template.bind({});
PrimaryProLite.args = {
  primary: true,
  label: 'Button',
};
PrimaryProLite.parameters = {
  consoleType: 'pro-lite',
};

export const PrimaryPro = Template.bind({});
PrimaryPro.args = {
  primary: true,
  label: 'Button',
};
PrimaryPro.parameters = {
  consoleType: 'pro',
};

export const PrimaryCloud = Template.bind({});
PrimaryCloud.args = {
  primary: true,
  label: 'Button',
};
PrimaryCloud.parameters = {
  consoleType: 'cloud',
  adminSecretSet: true,
};

export const PrimaryCloudPro = Template.bind({});
PrimaryCloudPro.args = {
  primary: true,
  label: 'Button',
};
PrimaryCloudPro.parameters = {
  consoleType: 'cloud-pro',
  adminSecretSet: true,
};

export const Secondary = Template.bind({});
Secondary.args = {
  label: 'Button',
};

export const Large = Template.bind({});
Large.args = {
  size: 'large',
  label: 'Button',
};

export const Small = Template.bind({});
Small.args = {
  size: 'small',
  label: 'Button',
};
