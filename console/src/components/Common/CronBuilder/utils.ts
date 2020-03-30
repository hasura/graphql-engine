export const stripNonStandardElements = (cronExpression: string) => {
  const elements = cronExpression.split(' ');
  if (elements.length <= 5) {
    return cronExpression;
  }
  return elements.join(' ');
};
