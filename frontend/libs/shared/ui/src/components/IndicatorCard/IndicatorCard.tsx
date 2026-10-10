import React from 'react';
import {
  FaCheck,
  FaExclamationCircle,
  FaExclamationTriangle,
  FaFlask,
  FaInfo,
  FaTimes,
} from 'react-icons/fa';
import { Callout, Flex } from '@radix-ui/themes';
import { IconType } from 'react-icons';
import { Text } from '../typography';
import { IconButton } from '../Button';
import { Collapsible } from '../Collapsible';

type indicatorCardStatus =
  'info' | 'positive' | 'negative' | 'experimental' | 'warning';

export type IndicatorCardProps = Callout.RootProps & {
  status?: indicatorCardStatus;
  headline?: React.ReactNode;
  children?: React.ReactNode;
  showIcon?: boolean;
  className?: string;
  customIcon?: IconType;
  onDismiss?: () => void;
  id?: string;
  collapsible?: boolean;
};

const calloutProps: Record<indicatorCardStatus, Callout.RootProps> = {
  info: {
    color: 'blue',
  },
  negative: {
    color: 'red',
  },
  positive: {
    color: 'green',
  },
  experimental: {
    color: 'purple',
  },
  warning: {
    color: 'amber',
  },
};

const IconPerStatus: Record<indicatorCardStatus, IconType> = {
  info: FaInfo,
  negative: FaExclamationCircle,
  positive: FaCheck,
  experimental: FaFlask,
  warning: FaExclamationTriangle,
};

export const IndicatorCard = ({
  status = 'info',
  headline,
  showIcon,
  children,
  customIcon,
  onDismiss,
  size,
  collapsible,
  ...rest
}: IndicatorCardProps) => {
  const Icon = customIcon ?? IconPerStatus[status];

  const renderHeadline = (className?: string) => {
    return headline ? (
      <Flex className={className} gap="2">
        {showIcon ? (
          <Callout.Icon>
            <Icon />
          </Callout.Icon>
        ) : null}
        <Text as="div" weight="bold" data-testid={headline}>
          {headline}
        </Text>
      </Flex>
    ) : null;
  };

  const nonCollapsibleContent = () => (
    <div>
      {renderHeadline('mb-2')}
      <Text as="div">{children}</Text>
    </div>
  );

  const collapsibleContent = () => (
    <Collapsible disableContentStyles triggerChildren={renderHeadline()}>
      <Text as="div">{children}</Text>
    </Collapsible>
  );

  return (
    <Callout.Root {...calloutProps[status]} size={size} {...rest}>
      {showIcon && !headline ? (
        <Callout.Icon>
          <Icon />
        </Callout.Icon>
      ) : null}
      <Callout.Text size={size}>
        <Flex justify="between">
          {collapsible ? collapsibleContent() : nonCollapsibleContent()}
          {onDismiss ? (
            <IconButton {...calloutProps} size={size} variant="ghost">
              <FaTimes />
            </IconButton>
          ) : null}
        </Flex>
      </Callout.Text>
    </Callout.Root>
  );
};
