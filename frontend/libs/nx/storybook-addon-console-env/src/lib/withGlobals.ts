import { StoryFn, StoryContext } from '@storybook/addons';

export const withGlobals =
  (
    decorated: (
      Story: StoryFn,
      values: Record<string, unknown>,
      context: StoryContext
    ) => StoryFn
  ) =>
  (story: StoryFn, context: StoryContext) => {
    const values = context.globals;
    return decorated(story, values, context);
  };
