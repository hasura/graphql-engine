const readableNames: Record<string, string> = {
  athena: 'Amazon Athena',
  snowflake: 'Snowflake',
};

export const makeDriverNameReadable = (name: string): string =>
  readableNames[name] || name;
