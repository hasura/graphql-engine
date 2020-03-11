import React from 'react';
import { connect } from 'react-redux';

import { Heading } from './atoms';
import { StyleGuide } from './demo/StyleGuide';
import { UIComponents } from './demo/Components';
import { UIKitWrapperDiv } from './demo/styles';

const UIKit = () => (
  <UIKitWrapperDiv py="lg" px="xl" mb="xl" bg="white">
    <Heading mb="lg">Design System</Heading>
    <StyleGuide />
    <UIComponents />
  </UIKitWrapperDiv>
);

export default connect()(UIKit);
