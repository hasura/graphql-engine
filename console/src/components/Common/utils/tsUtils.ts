export const UNSAFE_keys = <T extends object>(source: T) =>
  Object.keys(source) as Array<keyof T>;

export type Json =
  | null
  | boolean
  | number
  | string
  | Json[]
  | { [prop: string]: Json };

export type Nullable<T> = T | null | undefined;
