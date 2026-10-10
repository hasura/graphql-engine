export const constructRedirectUrl = (pathname: string, search: string) => {
  let finalUrl = pathname;
  if (search) {
    finalUrl += search;
  }
  return finalUrl;
};

export const isJsonString = (str) => {
  try {
    JSON.parse(str);
  } catch (e) {
    return false;
  }
  return true;
};
