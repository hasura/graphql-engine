import React from 'react';

export const TemplateStoriesFactory = (
  Template: (stories: Record<string, any>) => React.ReactNode
) => (stories: Record<string, any>): React.ReactComponentElement<any> => (
  <div>
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
          <div className="py-4">{Template(story)}</div>
        </div>
      ))}
  </div>
);
