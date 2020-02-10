import PropTypes from 'prop-types';
import React from 'react';
import { connect } from 'react-redux';

import { Buttons } from './demo/Button';
import { ColorScheme, Shades } from './demo/Colors';
import { TextLinks, Typography } from './demo/Typography';
import { Alerts } from './demo/Alerts';
import { BoxShadows } from './demo/Shadows';

import Button from './atoms/Button';
import AlertBox from './atoms/AlertBox';
import ToolTip from './atoms/Tooltip';
import { Heading, TextLink } from './atoms/Typography';
import RadioButton from './atoms/RadioButton';
import Checkbox from './atoms/Checkbox';

import { Flex, UIKitWrapperDiv } from './demo/styles';

// UI elements created with Styled-System for demo  //

const UIElements = () => (
  <React.Fragment>
    <Heading color="black.text" mt="lg">
      UI Elements
    </Heading>
    <Heading as="h3" color="black.text" my="lg">
      Colors
    </Heading>
    <ColorScheme />
    <Shades />
    <Heading as="h3" color="black.text" my="lg">
      Buttons
    </Heading>
    <Buttons />
    <Heading as="h3" color="black.text" my="lg">
      Shadows
    </Heading>
    <BoxShadows />
    <Heading as="h3" color="black.text" my="lg">
      Alerts
    </Heading>
    <Alerts />
    <Heading as="h3" color="black.text" my="lg">
      Text Links
    </Heading>
    <TextLinks />
    <Heading as="h3" color="black.text" my="lg">
      Typography
    </Heading>
    <Typography />
  </React.Fragment>
);

// UIKit(Parent) Demo component ************* //

const UIKit = () => (
  <UIKitWrapperDiv
    fontFamily="roboto" // ~ theme.fonts.roboto
    p="lg" // ~ padding: theme.space.lg
    mb="xl" // ~ margin-bottom: theme.space.xl
    bg="white" // ~ theme.colors.white
  >
    {/* React UI Components ***************************/}
    <Heading mb="lg">React Componets</Heading>
    {/* Buttons ~ large size */}
    <Heading mb="md" as="h2">
      {'<Button />'}
    </Heading>
    <Flex mb="lg">
      <Button type="primary" size="large" mr="lg">
        Primary Button
      </Button>
      <Button type="secondary" size="large" mr="lg">
        Secondary Button
      </Button>
      <Button type="success" size="large" mr="lg">
        Success Button
      </Button>
      <Button type="danger" size="large" mr="lg">
        Danger Button
      </Button>
      <Button type="warning" size="large" mr="lg">
        Warning Button
      </Button>
      <Button type="info" size="large" mr="lg">
        Info Button
      </Button>
    </Flex>
    {/* Buttons ~ small size */}
    <Flex mb="lg">
      <Button type="primary" mr="lg">
        Primary Button
      </Button>
      <Button type="outOfRange" mr="lg">
        Default Type
      </Button>
    </Flex>
    {/* Disabled State */}
    <Flex mb="lg">
      <Button type="primary" mr="lg" disabled>
        Primary Button
      </Button>
      <Button type="secondary" mr="lg" disabled>
        Secondary Button
      </Button>
      <Button type="success" mr="lg" disabled>
        Success Button
      </Button>
      <Button type="danger" mr="lg" disabled>
        Danger Button
      </Button>
      <Button type="warning" mr="lg" disabled>
        Warning Button
      </Button>
      <Button type="info" mr="lg" disabled>
        Info Button
      </Button>
    </Flex>
    {/* AlertBox  *******************************/}
    <Heading mb="md" as="h2">
      {'<Alertbox />'}
    </Heading>
    <AlertBox type="success" my="lg">
      Hello Testing!
    </AlertBox>
    <AlertBox type="info" my="lg" />
    <AlertBox type="warning" my="lg" />
    <AlertBox type="error" my="lg" />
    <AlertBox type="default" my="lg">
      dummy alert!
    </AlertBox>
    {/* ToolTip ********************************/}
    <Heading mb="md" as="h2">
      {'<Tooltip />'}
    </Heading>
    <Flex mb="lg">
      <ToolTip message="Dummy Text" placement="top" mr="lg">
        Hover me!!
      </ToolTip>
      <ToolTip message="Primary Button" mr="lg">
        <Button type="primary" size="small">
          Hover me!
        </Button>
      </ToolTip>
      <ToolTip message="Dummy Text" placement="bottom" mr="lg">
        Bottom
      </ToolTip>
      <ToolTip message="Dummy Text" placement="left">
        Left
      </ToolTip>
    </Flex>
    {/* Typography ******************************/}
    <Heading my="lg" as="h2">
      Typography
    </Heading>
    <Heading mb="md">{'<Heading />'}</Heading>
    <Heading mb="md" as="h2">
      {'<Heading as="h2" />'}
    </Heading>
    <Heading mb="md" as="h3">
      {'<Heading as="h3" />'}
    </Heading>
    <Heading mb="md" as="h4">
      {'<Heading as="h4" />'}
    </Heading>
    <Heading mb="md" as="h2">
      {'<TextLink />'}
    </Heading>
    <TextLink>Check it out</TextLink>
    <Heading my="md" as="h2">
      {'<TextLink underline />'}
    </Heading>
    <TextLink underline>Check it out</TextLink>
    {/* RadioButton ******************************/}
    <Heading my="md" as="h2">
      {'<RadioButton />'}
    </Heading>
    <RadioButton mb="md">Choice 1</RadioButton>
    <RadioButton mb="md">Choice 2</RadioButton>
    {/* Checkbox ******************************/}
    <Heading my="md" as="h2">
      {'<Checkbox />'}
    </Heading>
    <Checkbox mb="md">Option 1</Checkbox>
    <Checkbox mb="md">Option 2</Checkbox>
    {/* UI Elements *****************************/}
    <UIElements />
  </UIKitWrapperDiv>
);

// ************************************ //

UIKit.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

// ************************************ //

export default connect()(UIKit);
