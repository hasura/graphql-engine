/**
 *
 *
 * @param {string} query
 * @param {string[]} toSearch
 * @return a boolean indicating if the query was found in any of the string within toSearch
 */
export const anyIncludes = (query: string, toSearch: string[]) => {
  const lowered = query.toLowerCase();
  return toSearch.some((x) => x.toLowerCase().includes(lowered));
};
