import PropTypes from 'prop-types';
import React from 'react';
import { connect } from 'react-redux';

import { Buttons } from './demo/Button';
import { ColorScheme, Shades } from './demo/Colors';
import { TextLinks, Typography } from './demo/Typography';
import { Alerts } from './demo/Alerts';
import { BoxShadows } from './demo/Shadows';

// Design-System Components ************** //

import {
  Button,
  AlertBox,
  ToolTip,
  Heading,
  TextLink,
  Text,
  RadioButton,
  Checkbox,
  SwitchButton,
  Tabs,
  Spinner,
} from './atoms';

// Icon
import { Icon } from './Icons';

import { Flex, UIKitWrapperDiv } from './demo/styles';

// UI elements created with Styled-System for demo //

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

// Dummy data for Tabs ********************** //

const dummytabsData = [
  {
    title: 'Title 1',
    tabContent: 'Content 1',
  },
  {
    title: 'Title 2',
    tabContent: 'Content 2',
  },
  {
    title: 'Title 3',
    tabContent: 'Content 3',
  },
  {
    title: 'Title 4',
    tabContent: 'Content 4',
  },
  {
    title: 'Title 5',
    tabContent: 'Content 5',
  },
];

// UIKit(Parent) Demo component ************* //

const UIKit = () => (
  <UIKitWrapperDiv
    fontFamily="roboto" // ~ theme.fonts.roboto
    p="lg" // ~ padding: theme.space.lg
    mb="xl" // ~ margin-bottom: theme.space.xl
    bg="white" // ~ theme.colors.white
  >
    {/* React UI Components ********************/}
    <Heading mb="lg">React Components</Heading>
    {/* Buttons ~ large size */}

    <Heading mb="lg" as="h2">
      Buttons
    </Heading>

    <Heading as="h3">{'<Button />'}</Heading>
    <Button m="lg">Default Button</Button>

    <Heading as="h3">{'<Button type="primary" />'}</Heading>
    <Button type="primary" m="lg">
      Primary Button
    </Button>

    <Heading as="h3">{'<Button type="primary" size="large" />'}</Heading>
    <Button type="primary" size="large" m="lg">
      Primary Button
    </Button>

    <Heading as="h3">{'<Button type="secondary" size="large" />'}</Heading>
    <Button type="secondary" size="large" m="lg">
      Secondary Button
    </Button>

    <Heading as="h3">{'<Button type="success" size="large" />'}</Heading>
    <Button type="success" size="large" m="lg">
      Success Button
    </Button>

    <Heading as="h3">{'<Button type="danger" size="large" />'}</Heading>
    <Button type="danger" size="large" m="lg">
      Danger Button
    </Button>

    <Heading as="h3">{'<Button type="warning" size="large" />'}</Heading>
    <Button type="warning" size="large" m="lg">
      Warning Button
    </Button>

    <Heading as="h3">{'<Button type="info" size="large" />'}</Heading>
    <Button type="info" size="large" m="lg">
      Info Button
    </Button>

    {/* Disabled State */}
    <Heading as="h3">{'<Button type="whatever" disabled  />'}</Heading>
    <Flex m="lg">
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
    {/* Spinner  *******************************/}
    <Heading mb="md" as="h2">
      {'<Spinner />'}
    </Heading>
    <Spinner />
    {/* AlertBox  *******************************/}
    <Heading mb="md" mt="lg" as="h2">
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
    {/* Tabs */}
    <Heading mb="lg" as="h2">
      {'<Tabs tabsData={array} />'}
    </Heading>
    <Tabs tabsData={dummytabsData} />
    {/* Icon ********************************/}
    <Heading my="md" as="h2">
      Icons
    </Heading>
    <Heading my="md" as="h3">
      {'<Icon type="success" />'}
    </Heading>
    <Icon type="success" ml="md" />
    <Heading my="md" as="h3">
      {'<Icon type="success" color="yellow.primary />'}
    </Heading>
    <Icon type="success" color="yellow.primary" ml="md" />
    <Heading my="md" as="h3">
      {'<Icon type="info" />'}
    </Heading>
    <Icon type="info" ml="md" />
    <Heading my="md" as="h3">
      {'<Icon type="warning" />'}
    </Heading>
    <Icon type="warning" ml="md" />
    <Heading my="md" as="h3">
      {'<Icon type="error" />'}
    </Heading>
    <Icon type="error" ml="md" />
    <Heading my="md" as="h3">
      {'<Icon type="graphiql" />'}
    </Heading>
    <Icon type="graphiql" ml="md" />
    <Heading my="md" as="h3">
      {'<Icon type="database" />'}
    </Heading>
    <Icon type="database" ml="md" />
    <Heading my="md" as="h3">
      {'<Icon type="schema" />'}
    </Heading>
    <Icon type="schema" ml="md" />
    <Heading my="md" as="h3">
      {'<Icon type="event" />'}
    </Heading>
    <Icon type="event" ml="md" />
    <Heading my="md" as="h3">
      {'<Icon type="settings" />'}
    </Heading>
    <Icon type="settings" ml="md" />
    <Heading my="md" as="h3">
      {'<Icon type="question" />'}
    </Heading>
    <Icon type="question" ml="md" />
    <Heading my="md" as="h3">
      {'<Icon /> ~ default'}
    </Heading>
    <Icon ml="md" />
    {/* ToolTip ********************************/}
    <Heading my="lg" as="h2">
      {'<ToolTip />'}
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
    {/* Heading */}
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
    {/* TextLink */}
    <Heading mb="md" as="h2">
      {'<TextLink />'}
    </Heading>
    <TextLink>Check it out</TextLink>
    <Heading my="md" as="h2">
      {'<TextLink underline />'}
    </Heading>
    <TextLink underline>Check it out</TextLink>
    {/* Text */}
    <Heading my="md" as="h2">
      {'<Text />'}
    </Heading>
    <Text>
      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua. Semper quis lectus
      nulla at volutpat diam ut venenatis. Sed viverra tellus in hac habitasse
      platea dictumst. Id porta nibh venenatis cras. Velit dignissim sodales ut
      eu sem. Turpis cursus in hac habitasse platea dictumst quisque. Integer
      feugiat scelerisque varius morbi enim. Dui accumsan sit amet nulla. Donec
      et odio pellentesque diam volutpat commodo sed. Augue eget arcu dictum
      varius duis at. Nullam vehicula ipsum a arcu cursus vitae. Sapien et
      ligula ullamcorper malesuada proin libero nunc. Nunc congue nisi vitae
      suscipit tellus mauris a diam maecenas.
    </Text>
    <Heading my="md" as="h2">
      {"<Text type='explain' />"}
    </Heading>
    {/* Explainer text */}
    <Text type="explain">
      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua. Semper quis lectus
      nulla at volutpat diam ut venenatis. Sed viverra tellus in hac habitasse
      platea dictumst. Id porta nibh venenatis cras. Velit dignissim sodales ut
      eu sem. Turpis cursus in hac habitasse platea dictumst quisque. Integer
      feugiat scelerisque varius morbi enim. Dui accumsan sit amet nulla. Donec
      et odio pellentesque diam volutpat commodo sed. Augue eget arcu dictum
      varius duis at. Nullam vehicula ipsum a arcu cursus vitae. Sapien et
      ligula ullamcorper malesuada proin libero nunc. Nunc congue nisi vitae
      suscipit tellus mauris a diam maecenas.
    </Text>
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
    {/* Switch Button */}
    <Heading my="md" as="h2">
      {'<SwitchButton />'}
    </Heading>
    <SwitchButton />
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
