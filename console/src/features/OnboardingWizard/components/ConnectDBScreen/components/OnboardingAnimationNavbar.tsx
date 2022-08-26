import React from 'react';
import { CustomRightChevron } from './CustomRightChevron';

const commmonListItemStyle =
  'flex-shrink-0 w-10 h-10 flex items-center justify-center border-2 rounded-full';

export function OnboardingAnimationNavbar() {
  return (
    <nav>
      <ol className="border-t border-l border-r border-gray-300 rounded-t divide-y mb-0 divide-gray-300 md:flex md:divide-y-0 bg-white">
        <li className="relative flex-grow md:flex">
          <div className="group flex items-center w-full">
            <span className="px-md py-sm flex items-center">
              <span
                className={`${commmonListItemStyle} border-amber-500 bg-[#f9c548] font-semibold`}
              >
                <span className="text-sm text-gray-800">01</span>
              </span>
              <span className="ml-sm text-gray-900">Getting Started</span>
            </span>
          </div>
          <div
            className="hidden md:block absolute top-0 right-0 h-full w-5"
            aria-hidden="true"
          >
            <CustomRightChevron className="h-full w-full text-gray-300" />
          </div>
        </li>

        <li className="relative flex-grow md:flex">
          <div className="px-6 py-4 flex items-center" aria-current="step">
            <span
              className={`${commmonListItemStyle} border-gray-300  group-hover:border-gray-400`}
            >
              <span className="text-sm font-semibold text-gray-500 group-hover:text-gray-900">
                02
              </span>
            </span>
            <span className="ml-sm text-gray-900">Connect Database</span>
          </div>
          <div
            className="hidden md:block absolute top-0 right-0 h-full w-5"
            aria-hidden="true"
          >
            <CustomRightChevron className="h-full w-full text-gray-300" />
          </div>
        </li>

        <li className="relative flex-grow md:flex">
          <div className="group flex items-center">
            <span className="px-6 py-4 flex items-center">
              <span
                className={`${commmonListItemStyle} border-gray-300 group-hover:border-gray-400`}
              >
                <span className="text-sm font-semibold text-gray-500 group-hover:text-gray-900">
                  03
                </span>
              </span>
              <span className="ml-sm text-gray-900">Make Your First Query</span>
            </span>
          </div>
        </li>
      </ol>
    </nav>
  );
}
