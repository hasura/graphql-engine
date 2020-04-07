import React from 'react';

import { Heading, Text } from '../atoms';
import { Flex, ColorSchemeDiv, BoxShadowDiv } from './styles';

export const StyleGuide = () => (
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
