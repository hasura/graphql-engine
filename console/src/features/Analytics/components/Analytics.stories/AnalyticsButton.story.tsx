import type { ComponentStory } from '@storybook/react';
import React from 'react';
import { Button } from '@/new-components/Button';

import { Analytics } from '../Analytics';

// --------------------------------------------------
// STORY
// --------------------------------------------------
export const AnalyticsButton: ComponentStory<typeof Analytics> = args => {
  return (
    <Analytics {...args}>
      <Button>
        The Analytics component passes the HTML attributes to the Button (please
        inspect the DOM to find the rendered HTML attributes)
      </Button>
    </Analytics>
  );
};
AnalyticsButton.storyName = '⚙️ Passing the Button component';
AnalyticsButton.args = {
  name: 'componentName',
  passHtmlAttributesToChildren: true,
};
