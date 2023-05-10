import {
  screen,
  userEvent,
  waitForElementToBeRemoved,
  within,
} from '@storybook/testing-library';
import React from 'react';
import { dismissAllToasts } from '../new-components/Toasts';

export const TemplateStoriesFactory =
  (
    Template: (stories: Record<string, any>) => React.ReactNode,
    classNames = ''
  ) =>
  (stories: Record<string, any>): React.ReactComponentElement<any> =>
    (
      <div className="w-full">
        {Object.entries(stories)
          // Only use objects as function are events handlers injected by storybook
          .filter(
            ([, story]) =>
              typeof story === 'object' && !story.disableSnapshotTesting
          )
          .map(([storyName, story]) => (
            <div key={storyName}>
              <div className="text-black dark:text-white bg-gray-100 dark:bg-gray-700 underline p-2">
                {storyName}
              </div>
              <div className={`py-4 ${classNames}`}>{Template(story)}</div>
            </div>
          ))}
      </div>
    );

export const dismissToast = (delay = 500) => {
  return new Promise<void>(resolve => {
    // waiting a brief delay to ensures toast is available to dismiss after slide in transition
    setTimeout(() => {
      dismissAllToasts();
      resolve();
    }, delay);
  });
};

// confirms a hasuraAlert by button text
export const confirmAlert = async (
  confirmText = 'Remove',
  removalTimout = 3000
) => {
  const alert = await screen.findByRole('alertdialog');
  await userEvent.click(await within(alert).findByText(confirmText));

  // this is important b/c in successfull async workflows, the alert remains on screen to indicate success
  await waitForElementToBeRemoved(() => screen.queryByRole('alertdialog'), {
    timeout: removalTimout,
  });
};
