import React from 'react';
import { connect } from 'react-redux';

import { Buttons } from './demo/Button';
import { ColorScheme, Shades } from './demo/Colors';
import { TextLinks, Typography } from './demo/Typography';
import { Alerts } from './demo/Alerts';
import { BoxShadows } from './demo/Shadows';

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
  Icon,
} from './atoms';
import {
  Flex,
  UIKitWrapperDiv,
  ColorSchemeDiv,
  BoxShadowDiv,
} from './demo/styles';

// UIKit(Parent) Demo component ************* //

const UIKit = () => (
  <UIKitWrapperDiv fontFamily="roboto" py="lg" px="xl" mb="xl" bg="white">
    <Heading mb="lg">Design System</Heading>
    <StyleGuide />
    <UIComponents />
  </UIKitWrapperDiv>
);

// UI elements created with Styled-System for demo //

export const UIElements = () => (
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

const StyleGuide = () => (
  <React.Fragment>
    <Heading mb="lg" as="h3">
      Color Scheme
    </Heading>
    <Flex m="lg">
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="red.original"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          red.original
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="red.primary"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          red.primary
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="red.hover"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          red.hover
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="red.light"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          red.light
        </Text>
      </Flex>
    </Flex>

    <Flex m="lg">
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="green.original"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          green.original
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="green.primary"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          green.primary
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="green.hover"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          green.hover
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="green.light"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          green.light
        </Text>
      </Flex>
    </Flex>

    <Flex m="lg">
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="blue.original"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          blue.original
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="blue.primary"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          blue.primary
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="blue.hover"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          blue.hover
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="blue.light"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          blue.light
        </Text>
      </Flex>
    </Flex>

    <Flex m="lg">
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="orange.original"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          orange.original
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="orange.primary"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          orange.primary
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="orange.hover"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          orange.hover
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="orange.light"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          orange.light
        </Text>
      </Flex>
    </Flex>

    <Flex m="lg">
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="yellow.original"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          yellow.original
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="yellow.primary"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          yellow.primary
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="yellow.hover"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          yellow.hover
        </Text>
      </Flex>
    </Flex>

    <Flex m="lg">
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="grey.original"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          grey.original
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="grey.tab"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          grey.tab
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="grey.border"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          grey.border
        </Text>
      </Flex>
    </Flex>

    <Flex m="lg">
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="black.original"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          black.original
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="black.text"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          black.text
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="black.secondary"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          black.secondary
        </Text>
      </Flex>
      <Flex flexDirection="column" mr="40px">
        <ColorSchemeDiv
          bg="black.hover"
          borderRadius="circle"
          width={90}
          height={90}
        />
        <Text mt="md" fontSize="button">
          black.hover
        </Text>
      </Flex>
    </Flex>

    <Flex
      flexDirection="column"
      m="lg"
      alignItems="flex-start"
      justifyContent="center"
    >
      <ColorSchemeDiv bg="tab" borderRadius="circle" width={90} height={90} />
      <Text mt="md" fontSize="button" pl="lg">
        tab
      </Text>
    </Flex>

    <Flex
      flexDirection="column"
      m="lg"
      alignItems="flex-start"
      justifyContent="center"
    >
      <ColorSchemeDiv
        bg="white"
        borderRadius="circle"
        width={90}
        height={90}
        boxShadow={3}
      />
      <Text mt="md" fontSize="button" pl="lg">
        white
      </Text>
    </Flex>

    {/* Shadows */}

    <Heading mb="lg" mt="xl" as="h3">
      Shadows
    </Heading>
    <Flex justifyContent="flex-start">
      <Flex flexDirection="column" mr="xl">
        <BoxShadowDiv
          width={225}
          height={125}
          boxShadow={1}
          borderRadius="xs"
          bg="white"
        />
        <Text mt="md" fontSize="button">
          {'boxShadow={1}'}
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="xl">
        <BoxShadowDiv
          width={225}
          height={125}
          boxShadow={2}
          borderRadius="xs"
          bg="white"
        />
        <Text mt="md" fontSize="button">
          2
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="xl">
        <BoxShadowDiv
          width={225}
          height={125}
          boxShadow={3}
          borderRadius="xs"
          bg="white"
        />
        <Text mt="md" fontSize="button">
          3
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="xl">
        <BoxShadowDiv
          width={225}
          height={125}
          boxShadow={4}
          borderRadius="xs"
          bg="white"
        />
        <Text mt="md" fontSize="button">
          4
        </Text>
      </Flex>
    </Flex>

    {/* Border */}

    <Heading mb="lg" mt="xl" as="h3">
      Border
    </Heading>
    <Flex justifyContent="flex-start">
      <Flex flexDirection="column" mr="lg">
        <BoxShadowDiv
          width={180}
          height={120}
          boxShadow={1}
          border={0}
          bg="white"
        />
        <Text mt="md" fontSize="button" fontWeight="bold">
          {'border={0} ~ 0'}
        </Text>
        <Text mt="sm" fontSize="button">
          No border
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="lg">
        <BoxShadowDiv
          width={180}
          height={120}
          border={1}
          borderRadius="xs"
          bg="white"
        />
        <Text mt="md" fontSize="button" fontWeight="bold">
          1
        </Text>
        <Text mt="sm" fontSize="button">
          1px solid
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="lg">
        <BoxShadowDiv
          width={180}
          height={120}
          border={2}
          borderRadius="xs"
          bg="white"
        />
        <Text mt="md" fontSize="button" fontWeight="bold">
          2
        </Text>
        <Text mt="sm" fontSize="button">
          2px solid
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="lg">
        <BoxShadowDiv width={180} height={120} border={3} bg="white" />
        <Text mt="md" fontSize="button" fontWeight="bold">
          3
        </Text>
        <Text mt="sm" fontSize="button">
          3px solid
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="lg">
        <BoxShadowDiv
          width={180}
          height={120}
          border={4}
          borderRadius="xs"
          bg="white"
        />
        <Text mt="md" fontSize="button" fontWeight="bold">
          4
        </Text>
        <Text mt="sm" fontSize="button">
          4px solid
        </Text>
      </Flex>

      <Flex flexDirection="column">
        <BoxShadowDiv width={180} height={120} border={5} bg="white" />
        <Text mt="md" fontSize="button" fontWeight="bold">
          5
        </Text>
        <Text mt="sm" fontSize="button">
          5px solid
        </Text>
      </Flex>
    </Flex>

    {/* Border Radius */}

    <Heading mb="lg" mt="xl" as="h3" fontWeight="bold">
      Border Radius
    </Heading>
    <Flex justifyContent="flex-start">
      <Flex flexDirection="column" mr="xl">
        <BoxShadowDiv
          width={125}
          height={125}
          boxShadow={1}
          borderRadius="xs"
          bg="white"
        />
        <Text mt="md" fontSize="button" fontWeight="bold">
          {"borderRadius='xs'"}
        </Text>
        <Text mt="sm" fontSize="button">
          2px
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="xl">
        <BoxShadowDiv
          width={125}
          height={125}
          boxShadow={2}
          borderRadius="sm"
          bg="white"
        />
        <Text mt="md" fontSize="button" fontWeight="bold">
          sm
        </Text>
        <Text mt="sm" fontSize="button">
          4px
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="xl">
        <BoxShadowDiv
          width={125}
          height={125}
          boxShadow={2}
          borderRadius="md"
          bg="white"
        />
        <Text mt="md" fontSize="button" fontWeight="bold">
          md
        </Text>
        <Text mt="sm" fontSize="button">
          8px
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="xl">
        <BoxShadowDiv
          width={125}
          height={125}
          boxShadow={2}
          borderRadius="lg"
          bg="white"
        />
        <Text mt="md" fontSize="button" fontWeight="bold">
          lg
        </Text>
        <Text mt="sm" fontSize="button">
          12px
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="xl">
        <BoxShadowDiv
          width={125}
          height={125}
          boxShadow={2}
          borderRadius="xl"
          bg="white"
        />
        <Text mt="md" fontSize="button" fontWeight="bold">
          xl
        </Text>
        <Text mt="sm" fontSize="button">
          16px
        </Text>
      </Flex>

      <Flex flexDirection="column">
        <BoxShadowDiv
          width={125}
          height={125}
          boxShadow={2}
          borderRadius="circle"
          bg="white"
        />
        <Text mt="md" fontSize="button" fontWeight="bold">
          circle
        </Text>
        <Text mt="sm" fontSize="button">
          1000px
        </Text>
      </Flex>
    </Flex>

    {/* Font Weight */}

    <Heading mb="lg" mt="xl" as="h3">
      Font Weight
    </Heading>

    <Flex justifyContent="flex-start">
      <Flex flexDirection="column" mr="xl">
        <Heading fontWeight="normal">Aa</Heading>
        <Text mt="md" fontSize="button">
          {"fontWeight='normal'"}
        </Text>
        <Text mt="sm" fontSize="button" fontWeight="bold">
          400
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="xl">
        <Heading fontWeight="medium">Aa</Heading>
        <Text mt="md" fontSize="button">
          medium
        </Text>
        <Text mt="sm" fontSize="button" fontWeight="bold">
          500
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="xl">
        <Heading fontWeight="bold">Aa</Heading>
        <Text mt="md" fontSize="button">
          bold
        </Text>
        <Text mt="sm" fontSize="button" fontWeight="bold">
          700
        </Text>
      </Flex>
    </Flex>

    {/* Font sizes */}

    <Heading mb="lg" mt="xl" as="h3">
      Font Size
    </Heading>

    <Flex justifyContent="flex-start">
      <Flex flexDirection="column" mr="xl">
        <Heading fontWeight="normal">Aa</Heading>
        <Text mt="md" fontSize="button">
          {"fontSize='h1'"}
        </Text>
        <Text mt="sm" fontSize="button" fontWeight="bold">
          30px
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="xl">
        <Heading fontWeight="normal" as="h2">
          Aa
        </Heading>
        <Text mt="md" fontSize="button">
          h2
        </Text>
        <Text mt="sm" fontSize="button" fontWeight="bold">
          24px
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="xl">
        <Heading fontWeight="normal" as="h3">
          Aa
        </Heading>
        <Text mt="md" fontSize="button">
          h3
        </Text>
        <Text mt="sm" fontSize="button" fontWeight="bold">
          20px
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="xl">
        <Heading fontWeight="normal" as="h4">
          Aa
        </Heading>
        <Text mt="md" fontSize="button">
          h4
        </Text>
        <Text mt="sm" fontSize="button" fontWeight="bold">
          18px
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="xl">
        <Heading fontWeight="normal" fontSize="p">
          Aa
        </Heading>
        <Text mt="md" fontSize="button">
          p
        </Text>
        <Text mt="sm" fontSize="button" fontWeight="bold">
          16px
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="xl">
        <Heading fontWeight="normal" fontSize="button">
          Aa
        </Heading>
        <Text mt="md" fontSize="button">
          button
        </Text>
        <Text mt="sm" fontSize="button" fontWeight="bold">
          14px
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="xl">
        <Heading fontWeight="normal" fontSize="explain">
          Aa
        </Heading>
        <Text mt="md" fontSize="button">
          explain
        </Text>
        <Text mt="sm" fontSize="button" fontWeight="bold">
          12px
        </Text>
      </Flex>

      <Flex flexDirection="column" mr="xl">
        <Heading fontWeight="normal" fontSize="icon">
          Aa
        </Heading>
        <Text mt="md" fontSize="icon">
          icon
        </Text>
        <Text mt="sm" fontSize="button" fontWeight="bold">
          20px
        </Text>
      </Flex>
    </Flex>
  </React.Fragment>
);

const UIComponents = () => (
  <React.Fragment>
    {/* Buttons ~ large size */}

    <Heading as="h2" mt="xl" mb="lg">
      Buttons
    </Heading>

    <Heading as="h3">{'<Button />'}</Heading>
    <Button m="lg">Default Button</Button>

    <Heading as="h3">{'<Button type="primary" />'}</Heading>
    <Button type="primary" m="lg">
      Primary Button
    </Button>

    <Heading as="h3">{'<Button type="primary" isLoading="true" />'}</Heading>
    <Button type="primary" m="lg" isLoading>
      Primary Button
    </Button>

    <Heading as="h3">{'<Button type="primary" size="large" />'}</Heading>
    <Button type="primary" size="large" m="lg">
      Primary Button
    </Button>

    <Heading as="h3">
      {'<Button type="primary" size="large" isLoading="true" />'}
    </Heading>
    <Button type="primary" size="large" m="lg" isLoading="true">
      Primary Button
    </Button>

    <Heading as="h3">{'<Button type="secondary" size="large" />'}</Heading>
    <Button type="secondary" size="large" m="lg">
      Secondary Button
    </Button>

    <Heading as="h3">
      {'<Button type="secondary" size="large" isLoading="true" />'}
    </Heading>
    <Button type="secondary" size="large" m="lg" isLoading="true">
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

    <Heading my="md" as="h3">
      {'<Spinner size="small" />'}
    </Heading>
    <Spinner size="small" m="lg" />

    <Heading mb="md" as="h3">
      {'<Spinner size="large" />'}
    </Heading>
    <Spinner size="large" m="lg" />

    {/* AlertBox  *******************************/}

    <Heading mb="lg" mt="xl" as="h2">
      Alertbox
    </Heading>

    <Heading as="h3">{'<AlertBox />'}</Heading>
    <AlertBox m="lg">Dummy Text!!</AlertBox>

    <Heading as="h3">{'<AlertBox type="success" />'}</Heading>
    <AlertBox type="success" m="lg">
      Hello Testing!
    </AlertBox>

    <Heading as="h3">{'<AlertBox type="info" />'}</Heading>
    <AlertBox type="info" m="lg" />

    <Heading as="h3">{'<AlertBox type="warning" />'}</Heading>
    <AlertBox type="warning" m="lg" />

    <Heading as="h3">{'<AlertBox type="error" />'}</Heading>
    <AlertBox type="error" m="lg" />

    {/* Tabs *********************************/}

    <Heading mt="xl" mb="lg" as="h2">
      React Tab Component
    </Heading>

    <Heading mb="lg" as="h3">
      {'<Tabs tabsData={array} />'}
    </Heading>
    <Tabs tabsData={dummytabsData} />

    {/* Icons *********************************/}

    <Heading as="h2" mt="xl" mb="lg">
      Icon Component
    </Heading>

    <Heading my="lg" as="h3">
      {'<Icon type="success" />'}
    </Heading>
    <Icon type="success" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="success" color="yellow.primary />'}
    </Heading>
    <Icon type="success" color="yellow.primary" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="info" />'}
    </Heading>
    <Icon type="info" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="warning" />'}
    </Heading>
    <Icon type="warning" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="error" />'}
    </Heading>
    <Icon type="error" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="graphiql" />'}
    </Heading>
    <Icon type="graphiql" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="database" />'}
    </Heading>
    <Icon type="database" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="schema" />'}
    </Heading>
    <Icon type="schema" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="event" />'}
    </Heading>
    <Icon type="event" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="settings" />'}
    </Heading>
    <Icon type="settings" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="question" />'}
    </Heading>
    <Icon type="question" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon /> ~ default'}
    </Heading>
    <Icon ml="xl" />

    {/* ToolTip ********************************/}
    <Heading mb="lg" mt="xl" as="h2">
      ToolTip Component
    </Heading>

    <Heading mb="lg" mt="xl" as="h3">
      {'<ToolTip />'}
    </Heading>
    <ToolTip message="Dummy Text" ml="xl">
      Hover me!!
    </ToolTip>

    <Heading mb="lg" mt="xl" as="h3">
      {'<ToolTip placement="right" />'}
    </Heading>
    <ToolTip message="Dummy Text" placement="right" ml="xl">
      Hover me!!
    </ToolTip>

    <Heading my="xl" as="h3">
      {'<ToolTip placement="top" />'}
    </Heading>
    <ToolTip message="Primary Button" ml="xl" placement="top">
      <Button type="primary" size="small">
        Hover me!
      </Button>
    </ToolTip>

    <Heading mb="lg" mt="xl" as="h3">
      {'<ToolTip placement="left" />'}
    </Heading>
    <ToolTip message="Dummy Text" placement="left" ml="xl">
      Hover me!!
    </ToolTip>

    <Heading mb="lg" mt="xl" as="h3">
      {'<ToolTip placement="bottom" />'}
    </Heading>
    <ToolTip message="Dummy Text" placement="bottom" ml="xl">
      Hover me!!
    </ToolTip>

    {/* Typography ******************************/}

    {/* Heading */}
    <Heading mb="lg" mt="xl" as="h2">
      Typography
    </Heading>

    <Heading mb="lg" mt="xl" as="h3">
      {'<Heading />'}
    </Heading>
    <Heading>Main Heading</Heading>

    <Heading mb="lg" mt="xl" as="h3">
      {'<Heading as="h2" />'}
    </Heading>
    <Heading as="h2">Subpage title</Heading>

    <Heading mb="lg" mt="xl" as="h3">
      {'<Heading as="h3" />'}
    </Heading>
    <Heading as="h3">Section Header</Heading>

    <Heading mb="lg" mt="xl" as="h3">
      {'<Heading as="h4" />'}
    </Heading>
    <Heading as="h4">Sub section Heading</Heading>

    {/* TextLink */}
    <Heading as="h2" mb="lg" mt="xl">
      {'<TextLink />'}
    </Heading>
    <TextLink>Check it out</TextLink>

    <Heading as="h2" mb="lg" mt="xl">
      {'<TextLink underline />'}
    </Heading>
    <TextLink underline>Check it out</TextLink>

    {/* Text */}
    <Heading mb="lg" mt="xl" as="h2">
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
    <Heading my="md" as="h2" mb="lg" mt="xl">
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
    <Heading mb="lg" mt="xl" as="h2">
      {'<RadioButton />'}
    </Heading>
    <RadioButton mb="md">Choice 1</RadioButton>
    <RadioButton mb="md">Choice 2</RadioButton>
    {/* Checkbox ******************************/}
    <Heading mb="lg" mt="xl" as="h2">
      {'<Checkbox />'}
    </Heading>
    <Checkbox mb="md">Option 1</Checkbox>
    <Checkbox mb="md">Option 2</Checkbox>
    {/* Switch Button */}
    <Heading mb="lg" mt="xl" as="h2">
      {'<SwitchButton />'}
    </Heading>
    <SwitchButton />
  </React.Fragment>
);

export default connect()(UIKit);
