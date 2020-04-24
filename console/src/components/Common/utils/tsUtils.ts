export const UNSAFE_keys = <T extends object>(source: T) =>
  Object.keys(source) as Array<keyof T>;
