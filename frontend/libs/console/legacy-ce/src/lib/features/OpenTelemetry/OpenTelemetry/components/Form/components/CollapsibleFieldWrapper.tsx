import * as React from 'react';

import {
  IconTooltip,
  Collapsible,
  LearnMoreLink,
  Text,
} from '@hasura/shared/ui';
import { Flex, Skeleton, Strong } from '@radix-ui/themes';

interface CollapsibleFieldWrapperProps {
  inputFieldName: string;
  label: string;
  tooltip: string;
  loading?: boolean;
  learnMoreLink?: string;
  children?: React.ReactNode;
}

/**
 * At the time of writing:
 * 1. the FieldWrapper allows adding the label, and the tooltip in a consistent way
 * 2. The Collapsible component allows collapsing a section
 * but none of them allow having a collapsible FieldWrapper. Hence this component brings some parts
 * of the FieldWrapper and mix them with the Collapsible component.
 *
 * TODO: If this pattern will be used more often, we should uniform this behavior.
 * TODO: Fix the a11y issue for which a button cannot be child of another button (speaking about the
 * tooltip trigger being a child of the collapsible trigger)
 */
export const CollapsibleFieldWrapper: React.FC<CollapsibleFieldWrapperProps> = (
  props,
) => {
  const { inputFieldName, label, tooltip, children, loading, learnMoreLink } =
    props;

  if (loading) return <Skeleton height="30px" width="200px" />;

  return (
    <Collapsible
      triggerChildren={
        <Text as="label" htmlFor={inputFieldName}>
          <Flex align="center" gap="2">
            <Strong>{label}</Strong>
            <span>(Optional)</span>

            <IconTooltip message={tooltip} />
            {!!learnMoreLink && <LearnMoreLink href={learnMoreLink} />}
          </Flex>
        </Text>
      }
    >
      {children}
    </Collapsible>
  );
};
