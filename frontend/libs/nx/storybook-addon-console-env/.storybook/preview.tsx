import React from 'react';
import { StoryFn } from '@storybook/react';

declare const window: any;

export const decorators = [
  (Story: StoryFn) => (
    <div>
      <div>
        <Story />
      </div>
      <pre
        style={{
          backgroundColor: 'DarkSlateBlue',
          color: 'white',
          borderRadius: '8px',
          margin: '16px 0',
          padding: '16px',
        }}
      >
        window.__env = {JSON.stringify(window.__env, null, 2)}
      </pre>
    </div>
  ),
];
