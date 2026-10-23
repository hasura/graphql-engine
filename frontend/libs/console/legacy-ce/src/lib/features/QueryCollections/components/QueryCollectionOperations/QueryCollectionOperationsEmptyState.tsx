import React from 'react';
import { Box, Card, Flex, Text } from '@radix-ui/themes';
import { HasuraIcon } from '@hasura/shared/ui';
import {
  FaBolt,
  FaCogs,
  FaDatabase,
  FaLink,
  FaLock,
  FaMobileAlt,
  FaPlug,
  FaProjectDiagram,
  FaServer,
} from 'react-icons/fa';

// Radix color tokens flip with the console appearance, so the diagram follows
// light/dark mode without any `dark:` overrides.
const lineColor = 'border-(--gray-a8)';
const mutedText = 'text-(--gray-a10)';

const NodeBox = ({ icon, label }: { icon: React.ReactNode; label: string }) => (
  <Flex
    align="center"
    gap="2"
    className={`w-48 rounded-(--radius-2) border p-2 ${lineColor} ${mutedText}`}
  >
    {icon}
    <Text size="2">{label}</Text>
  </Flex>
);

const ArrowHead = () => (
  <svg
    className={`w-4 h-full -ml-1.5 ${mutedText}`}
    fill="currentColor"
    viewBox="0 0 16 16"
    height="1em"
    width="1em"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path d="M5.56 14L5 13.587V2.393L5.54 2 11 7.627v.827L5.56 14z" />
  </svg>
);

const Line = () => <div className={`w-4 border-t ${lineColor}`} />;

const EdgeLabel = ({
  icon,
  label,
}: {
  icon: React.ReactNode;
  label: string;
}) => (
  <Flex align="center" gap="2" px="2" className={mutedText}>
    {icon}
    <Text size="2">{label}</Text>
  </Flex>
);

/** A titled column of nodes with a bracket on the side facing Hasura. */
const NodeGroup = ({
  title,
  side,
  children,
}: {
  title: string;
  side: 'left' | 'right';
  children: React.ReactNode;
}) => {
  const edge = side === 'left' ? 'right-0' : 'left-0';
  return (
    <Flex
      align="center"
      className={`relative ${side === 'left' ? 'pr-6' : 'pl-6'}`}
    >
      <Text
        size="2"
        weight="bold"
        className={`absolute -top-3.5 uppercase tracking-wider ${mutedText} ${
          side === 'left' ? 'left-0' : 'right-0'
        }`}
      >
        {title}
      </Text>

      <div className={`absolute h-full border-r ${edge} ${lineColor}`} />
      <div className={`absolute w-4 border-t top-0 ${edge} ${lineColor}`} />
      <div className={`absolute w-4 border-b bottom-0 ${edge} ${lineColor}`} />

      <Flex direction="column" gap="2" py="4">
        {children}
      </Flex>
    </Flex>
  );
};

export const QueryCollectionOperationsEmptyState = () => {
  return (
    <Box my="2" className="relative">
      <Flex
        align="center"
        pt="6"
        px="4"
        pb="4"
        className="w-max rounded-(--radius-3) border bg-(--gray-a3) border-(--gray-a6)"
      >
        <NodeGroup title="Sources" side="left">
          <NodeBox icon={<FaCogs />} label="REST Endpoints" />
          <NodeBox icon={<FaDatabase />} label="Database" />
          <NodeBox icon={<FaPlug />} label="GraphQL Services" />
        </NodeGroup>

        <Flex align="center">
          <Line />
          <EdgeLabel icon={<FaLink />} label="Connected" />
          <Line />
          <ArrowHead />
        </Flex>

        <Card size="1" className="w-48 p-0! overflow-hidden hover:shadow-md">
          <Flex align="center" gap="2" p="2" className="relative">
            <div className="absolute mt-[-8px] top-1/2 right-[-6px] animate-ping rounded-full w-6 h-6 bg-(--amber-9) z-10" />
            <Flex
              align="center"
              justify="center"
              className="absolute mt-[-8px] top-1/2 right-[-6px] rounded-full w-6 h-6 bg-(--amber-9) z-10"
            >
              <FaLock color="white" className="w-2 h-2" />
            </Flex>
            <HasuraIcon className="w-3 h-3 text-(--gray-12)" />
            <Text size="2" weight="bold">
              Hasura
            </Text>
          </Flex>
          <Flex
            align="center"
            gap="2"
            py="1"
            px="2"
            className="border-t border-(--gray-a5) bg-(--gray-a2) text-(--gray-11)"
          >
            <FaLock />
            <Text size="2">Allow List Operations</Text>
          </Flex>
        </Card>

        <Flex align="center">
          <Line />
          <ArrowHead />
          <EdgeLabel icon={<FaBolt />} label="Powering" />
          <Line />
        </Flex>

        <NodeGroup title="Consumers" side="right">
          <NodeBox icon={<FaMobileAlt />} label="Apps" />
          <NodeBox icon={<FaServer />} label="Data Platforms" />
          <NodeBox icon={<FaProjectDiagram />} label="Other Services" />
        </NodeGroup>
      </Flex>
    </Box>
  );
};
