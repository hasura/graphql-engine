import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { DialogContainer } from './DialogContainer';
import { dialogHeader, familiaritySurveySubHeader } from '../../constants';

export default {
  title: 'features/Onboarding Wizard/Dialog Container',
  component: DialogContainer,
} as ComponentMeta<typeof DialogContainer>;

const ExampleChildren = () => {
  return <div>Hello world!</div>;
};

export const Base: Story = () => (
  <DialogContainer
    header={dialogHeader}
    subHeader={familiaritySurveySubHeader}
    showStepper
    activeIndex={1}
  >
    <ExampleChildren />
  </DialogContainer>
);
