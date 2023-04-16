// returns the correct indefinite article based on the first character of the input string
export const indefiniteArticle = (word: string): string => {
  const vowels = ['a', 'e', 'i', 'o', 'u'];
  return vowels.includes(word.charAt(0)) ? 'an' : 'a';
};

export const getDriverNameFromUrlParams = (): string | undefined => {
  const urlParams = new URLSearchParams(window.location.search);

  const driver = urlParams.get('driver');

  return driver ?? undefined;
};

export const transformErrorResponse = (error: unknown) => {
  const err = error as Record<string, any>;

  let message = '';

  if ('internal' in err) message = JSON.stringify(err?.internal, null, '\t');
  else message = err.error;

  return {
    name: `Error code: ${err.code}`,
    message,
  };
};
