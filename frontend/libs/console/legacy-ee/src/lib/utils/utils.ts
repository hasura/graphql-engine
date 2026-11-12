export const constructRedirectUrl = (pathname: string, search: string) => {
  let finalUrl = pathname;
  if (search) {
    finalUrl += search;
  }
  return finalUrl;
};
