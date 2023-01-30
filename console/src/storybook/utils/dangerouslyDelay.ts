// Description: A simple but dangerous delay function to use in storybook
// Warning! Use only as a last resource, since it leads to tests that depend on timing
// Always try to use `waitFor` or `await findBy` instead
export const dangerouslyDelay = (ms: number) =>
  new Promise(res => setTimeout(res, ms));
