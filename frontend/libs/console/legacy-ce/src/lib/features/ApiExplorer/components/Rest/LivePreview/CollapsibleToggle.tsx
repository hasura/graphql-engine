import React, { Fragment, useState } from 'react';
import { Card, Flex, Strong } from '@radix-ui/themes';
import { Collapsible, Text } from '@hasura/shared/ui';

type CollapsibleToggleProps = {
  state: Record<string, any>[];
  properties: string[];
  title: string;
  children?: React.ReactNode;
};

const CollapsibleToggle: React.FC<CollapsibleToggleProps> = ({
  title,
  state,
  properties,
  children,
}) => {
  const [isOpen, setIsOpen] = useState(true);

  return (
    <Card>
      <Collapsible
        open={isOpen}
        onOpenChange={setIsOpen}
        disableContentStyles
        triggerChildren={
          <div>
            <Text as="p" align="left">
              <Strong>{title}</Strong>
            </Text>
            {isOpen || !state.length ? null : (
              <Flex
                gap="2"
                align="center"
                wrap="wrap"
                className="wrap-break-word"
              >
                {state.map((stateVar, index) => (
                  <Fragment key={index}>
                    <Text className="overflow-hidden text-ellipsis max-w-[300] text-base">
                      {`${stateVar[properties[0]]}  :  ${stateVar[properties[1]]}`}
                    </Text>
                    {index !== state?.length - 1 ? <span>|</span> : null}
                  </Fragment>
                ))}
              </Flex>
            )}
          </div>
        }
      >
        {children}
      </Collapsible>
    </Card>
  );
};

export default CollapsibleToggle;
