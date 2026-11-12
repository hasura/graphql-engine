export const isEmpty = (value) =>
  value === undefined || value === null || value === '';
export const addNumbersSafely = (...args) => {
  let result = 0;
  args.forEach((i) => {
    if (!Number.isNaN(Number(i))) result = Number(i) + Number(result);
  });
  return result;
};
