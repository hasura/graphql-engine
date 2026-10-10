import { Button, Card, IconButton, Link, Text } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
import clsx from 'clsx';
import { FaTimes, FaNetworkWired } from 'react-icons/fa';

import type { JSX } from 'react';

export default function VPCBanner({
  className,
  onClose,
}: {
  className?: string;
  onClose?: VoidFunction;
}): JSX.Element {
  return (
    <Card size="1" className={clsx('mb-4', className)}>
      <Flex align="center" gap="4">
        <FaNetworkWired className="fill-current self-start" />
        <div>
          <Text as="p" weight="medium" color="gray">
            Want to connect to a private database?
          </Text>
          <Text as="p">
            Explore our Dedicated VPC and VPC PrivateLink offerings.
          </Text>
        </div>
        <Link
          href="https://hasura.io/docs/latest/graphql/cloud/dedicated-vpc.html"
          target="__blank"
        >
          <Button>Learn More</Button>
        </Link>
        {onClose && (
          <IconButton
            color="indigo"
            radius="full"
            variant="ghost"
            onClick={onClose}
          >
            <FaTimes />
          </IconButton>
        )}
      </Flex>
    </Card>
  );
}
