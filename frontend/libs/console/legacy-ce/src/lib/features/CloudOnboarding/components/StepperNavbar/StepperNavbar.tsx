import React from 'react';
import clsx from 'clsx';
import { CustomRightChevron } from './components/CustomRightChevron';

const commmonListItemStyle =
  'flex-shrink-0 w-10 h-10 flex items-center justify-center border-2 rounded-full';

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
      <ol className="font-sans border-t border-l border-r border-gray-300 rounded-t divide-y mb-0 divide-gray-300 md:flex md:divide-y-0 bg-white">
        {steps.map((stepDetails, index) => (
          <li key={stepDetails.text} className="relative flex-grow md:flex">
            <div className="group flex items-center w-full">
              <span className="px-md py-sm flex items-center">
                <span
                  className={clsx(
                    `font-bold text-sm`,
                    commmonListItemStyle,
                    index === currentActiveIndex
                      ? `bg-[#f9c548] border-amber-500 text-slate-900`
                      : `border-gray-300 text-muted-dark`
                  )}
                >
                  {stepDetails.step}
                </span>
                <span
                  className={clsx(
                    'ml-sm font-bold ',
                    index === currentActiveIndex
                      ? 'text-slate-900'
                      : 'text-muted-dark'
                  )}
                >
                  {stepDetails.text}
                </span>
              </span>
            </div>
            <div
              className="md:block absolute top-0 right-0 h-full w-5"
              aria-hidden="true"
            >
              {index !== lastStep && (
                <CustomRightChevron className="h-full w-full text-gray-300" />
              )}
            </div>
          </li>
        ))}
      </ol>
    </nav>
  );
}
