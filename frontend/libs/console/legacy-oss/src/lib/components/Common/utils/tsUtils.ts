export const UNSAFE_keys = <T extends Record<string, unknown>>(source: T) =>
  Object.keys(source) as Array<keyof T>;

export type Json =
  | null
  | boolean
  | number
  | string
  | Json[]
  | { [prop: string]: Json };

export type Nullable<T> = T | null | undefined;

type PathImpl<T, Key extends keyof T> = Key extends string
  ? T[Key] extends Record<string, unknown>
    ?
        | `${Key}.${PathImpl<T[Key], Exclude<keyof T[Key], keyof unknown[]>> &
            string}`
        | `${Key}.${Exclude<keyof T[Key], keyof unknown[]> & string}`
    : never
  : never;

type PathImpl2<T> = PathImpl<T, keyof T> | keyof T;

export type Path<T> = PathImpl2<T> extends string | keyof T
  ? PathImpl2<T>
  : keyof T;

export type PathValue<
  T,
  P extends Path<T>
> = P extends `${infer Key}.${infer Rest}`
  ? Key extends keyof T
    ? Rest extends Path<T[Key]>
      ? PathValue<T[Key], Rest>
      : never
    : never
  : P extends keyof T
  ? T[P]
  : never;

export function get<T extends Record<string, any>, P extends Path<T>>(
  obj: T,
  path: P
): PathValue<T, P> {
  const keys = (path as string).split('.');
  if (!keys.length) return obj as PathValue<T, P>;
  const new_obj = keys.reduce((acc, key) => {
    return acc[key] || false;
  }, obj);
  return new_obj as PathValue<T, P>;
}
