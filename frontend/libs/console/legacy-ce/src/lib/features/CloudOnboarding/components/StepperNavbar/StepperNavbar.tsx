import React from 'react';
import { Avatar, Flex, Strong } from '@radix-ui/themes';
import { CustomRightChevron } from './components/CustomRightChevron';
import { Text } from '@hasura/shared/ui';

export type StepperNavbarStep = {
  step: string;
  text: string;
};

type StepperNavbarProps = {
  steps: StepperNavbarStep[];
  /**
   * step which is currently active, assumes 1-based indexing
   */
  activeIndex?: number;
};

export function StepperNavbar(props: StepperNavbarProps) {
  const { steps, activeIndex } = props;
  const lastStep = steps.length - 1;
  // for using 1-based indexing, if no activeIndex prop then set it as -1
  const currentActiveIndex = activeIndex ? activeIndex - 1 : -1;

  return (
    <nav>
      <ol className="font-sans border-t border-l border-r border-gray-500 rounded-t divide-y mb-0 divide-gray-300 md:flex md:divide-y-0">
        {steps.map((stepDetails, index) => (
          <li key={stepDetails.text} className="relative grow md:flex">
            <Flex align="center" className="group w-full">
              <Flex align="center" className="px-4 py-2" gap="2">
                <Avatar
                  radius="full"
                  fallback={<Strong>{stepDetails.step}</Strong>}
                  color={index === currentActiveIndex ? 'indigo' : 'gray'}
                />
                <Text
                  size="2"
                  weight="bold"
                  color={currentActiveIndex ? 'indigo' : 'gray'}
                  className={'ml-2'}
                >
                  {stepDetails.text}
                </Text>
              </Flex>
            </Flex>
            <div
              className="md:block absolute top-0 right-0 h-full w-5"
              aria-hidden="true"
            >
              {index !== lastStep && (
                <CustomRightChevron className="h-full w-full" />
              )}
            </div>
          </li>
        ))}
      </ol>
    </nav>
  );
}
