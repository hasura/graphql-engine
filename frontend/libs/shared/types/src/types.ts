export type NameValue = {
  name: string;
  value: string;
};

export type Nullable<T> = T | null | undefined;

/**
 * Set all keys in an object to Never. Useful for writing custom types.
 * To grasp it:
 * Given { a: string; b: number }
 * called like this MakeNever<MyType>
 * It will output { a: never; b: never }
 */
export type MakeNever<T> = {
  [P in keyof T]: never;
};

/**
 * Makes a discriminated union
 * Useful if you have part of the object where you need
 *
 * To grasp it:
 * Given { buttonLabel: string, buttonIcon?: string; onClick: () => void }
 * Called like this DiscriminatedTypes<MyType, 'buttonLabel'>
 * It will output { buttonLabel?: string; buttonIcon?: never; onClick?: never } | { buttonLabel: string; buttonIcon?: string: onClick: () => void; }
 * This way :
 *   If buttonLabel is not set, buttonIcon and onClick cannot be set
 *   If buttonLabel is set, buttonIcon can be set and onClick is mandatory
 *
 * @example <caption>Here you prevent `labelIcon` and `labelColor` to be passed without `label`,
 * but you can pass just `label` if you want.</caption>
 * type FieldWrapperProps =
 *  | {
 *      id: string;
 *    } & DiscriminatedTypes<
 *      {
 *        label: string;
 *        labelColor: string;
 *        labelIcon?: React.ReactElement;
 *      },
 *      'label'
 *    >;
 */
export type DiscriminatedTypes<T, K extends keyof T> =
  | MakeNever<Partial<Pick<T, K>> & Partial<Omit<T, K>>>
  | (Required<Pick<T, K>> & Omit<T, K>);

type PathImpl<T, Key extends keyof T> = Key extends string
  ? T[Key] extends Record<string, unknown>
    ? | `${Key}.${PathImpl<T[Key], Exclude<keyof T[Key], keyof unknown[]>> &
          string}`
      | `${Key}.${Exclude<keyof T[Key], keyof unknown[]> & string}`
    : never
  : never;

type PathImpl2<T> = PathImpl<T, keyof T> | keyof T;

export type Path<T> =
  PathImpl2<T> extends string | keyof T ? PathImpl2<T> : keyof T;

export type PathValue<
  T,
  P extends Path<T>,
> = P extends `${infer Key}.${infer Rest}`
  ? Key extends keyof T
    ? Rest extends Path<T[Key]>
      ? PathValue<T[Key], Rest>
      : never
    : never
  : P extends keyof T
    ? T[P]
    : never;

export type CreateBooleanMap<T> = {
  [K in keyof T]?: boolean;
};

/**
 * Lets you choose specific properties to make optional on a type
 *
 * For example:
 *
 * interface Person {
 *   name: string;
 *   hometown: string;
 *   nickname: string;
 * }
 *
 * type NicknameOptional = PartialBy<Person, 'nickname'>
 *
 */
export type PartialBy<T, K extends keyof T> = Omit<T, K> & Partial<Pick<T, K>>;

type DeepUtilityBuiltin =
  | Date
  | Error
  | RegExp
  | Promise<unknown>
  | ((...args: any[]) => unknown)
  | string
  | number
  | boolean
  | bigint
  | symbol
  | null
  | undefined;

/**
 * Recursive version of `Partial`: makes every property optional, at every level.
 * Arrays are kept as they are.
 *
 * For example:
 *
 * ```
 * type Config = { a: { b: string; c: number } };
 * type PartialConfig = DeepPartial<Config>;
 * //   ^? { a?: { b?: string; c?: number } }
 * ```
 */
export type DeepPartial<T> = T extends
  DeepUtilityBuiltin | ReadonlyArray<unknown>
  ? T
  : { [K in keyof T]?: DeepPartial<T[K]> };

/**
 * Recursive version of `Required`: makes every property required (and not
 * `undefined`), at every level.
 *
 * For example:
 *
 * ```
 * type Config = { a?: { b?: string } };
 * type FullConfig = DeepRequired<Config>;
 * //   ^? { a: { b: string } }
 * ```
 */
export type DeepRequired<T> = T extends DeepUtilityBuiltin
  ? Exclude<T, undefined>
  : T extends Array<infer Item>
    ? Array<DeepRequired<Item>>
    : T extends ReadonlyArray<infer Item>
      ? ReadonlyArray<DeepRequired<Item>>
      : { [K in keyof T]-?: DeepRequired<T[K]> };

// see: https://stackoverflow.com/questions/57103834/typescript-omit-a-property-from-all-interfaces-in-a-union-but-keep-the-union-s
// TS docs: https://www.typescriptlang.org/docs/handbook/2/conditional-types.html#distributive-conditional-types
export type DistributiveOmit<T, K extends PropertyKey> = T extends any
  ? Omit<T, K>
  : never;

/**
 *
 * Pass in a `Record` type, and this will create a string union of all dot notations into the type
 *
 * For example:
 *
 * ```
 * type SomeObject = { foo: string; bar: { baz: string; bing: string } };
 * type AllPaths = PathInto<x>;
 * ```
 *
 * And `AllPaths` would be equivalent to this:
 *
 * ```
 * type AllPaths = "foo" | "bar.baz" | "bar.bing"
 * ```
 *
 */
export type PathInto<ObjectType extends Record<string, unknown>> = keyof {
  [
    Key in keyof ObjectType as ObjectType[Key] extends string
      ? Key
      : ObjectType[Key] extends Record<string, unknown>
        ? `${Key & string}.${PathInto<ObjectType[Key]> & string}`
        : never
  ]: unknown;
};
/**
 *
 * This allows you to pass in a `Record` type and the 2nd argument is dot notation to extract a type.
 *
 * For example:
 *
 * ```
 * type SomeObject = { foo: { bar: { baz: SomeType } } };
 * type TypeOfBaz = Choose<SomeObject, 'foo.bar.baz'>;
 * ```
 *
 * `TypeOfBaz` would be equal to `SomeType`
 *
 */
export type Choose<
  T extends Record<string, unknown>,
  K extends PathInto<T>,
> = K extends `${infer U}.${infer Rest}`
  ? T[U] extends Record<string, unknown>
    ? Choose<T[U], Rest extends PathInto<T[U]> ? Rest : never>
    : T[K]
  : T[K];
