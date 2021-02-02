import React from 'react';

import { Heading } from './atoms';
import { StyleGuide } from './demo/StyleGuide';
import { UIComponents } from './demo/Components';
import { UIKitWrapperDiv } from './demo/styles';

const UIKit = () => (
  <UIKitWrapperDiv py="lg" px="xl" mb="xl" bg="white" fontFamily="roboto">
    <Heading mb="lg">Design System</Heading>
    <StyleGuide />
    <UIComponents />
  </UIKitWrapperDiv>
);

export default UIKit;
