import { MapStateToProps as ReduxMapStateToProps } from 'react-redux';
import { Store, AnyAction } from 'redux';
import { ThunkAction, ThunkDispatch } from 'redux-thunk';
import { RouterAction } from 'react-router-redux';
import { RAEvents } from './components/Services/Events/types';
import reducer from './reducer';

export type FixMe = any;

export type ApiExplorer = {
  authApiExpanded: string;
  currentTab: number;
  headerFocus: boolean;
  loading: boolean;
  mode: string;
  modalState: Record<string, string>;
  explorerData: Record<string, string>;
  displayedApi: DisplayedApiState;
};

export type DisplayedApiState = {
  details: Record<string, string>;
  id: string;
  request: ApiExplorerRequest;
};

export type ApiExplorerRequest = {
  bodyType: string;
  headers: ApiExplorerHeader[];
  headersInitialised: boolean;
  method: string;
  params: string;
  url: string;
};

export type ApiExplorerHeader = {
  key: string;
  value: string;
  isActive: boolean;
  isNewHeader: boolean;
  isDisabled: boolean;
};

// Redux Utils
export type ReduxState = ReturnType<typeof reducer>;

export type ReduxAction = RAEvents | RouterAction;
export type MapStateToProps<
  StateProps = unknown,
  OwnProps = unknown
> = ReduxMapStateToProps<StateProps, OwnProps, ReduxState>;
export type GetReduxState = () => ReduxState;
export type Thunk<
  ReturnType = void,
  A extends AnyAction = ReduxAction
> = ThunkAction<ReturnType, ReduxState, unknown, A>;
export type Dispatch = ThunkDispatch<ReduxState, unknown, ReduxAction>;
export type ConnectInjectedProps = {
  dispatch: Dispatch;
};
export type ReduxStore = Store<ReduxState, ReduxAction>;

// Router Utils
export type ReplaceRouterState = (route: string) => void;
// HGE common types
export type RunSqlType = {
  type: string;
  version?: number;
  args: {
    cascade?: boolean;
    read_only?: boolean;
    sql: string;
  };
};

export type Entry<O, K extends keyof O> = [K, O[K]];
export type Entries<O> = Entry<O, keyof O>[];

declare global {
  // eslint-disable-next-line no-underscore-dangle
  const __DEVELOPMENT__: boolean;
}
export type DeepPartial<T> = {
  [P in keyof T]?: DeepPartial<T[P]>;
};

export type Nullable<T> = T | null;

export type NullableProps<T> = { [K in keyof T]: T[K] | null };

export type DeepNullableProps<T> = {
  [K in keyof T]: DeepNullableProps<T[K]> | null;
};

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
  [Key in keyof ObjectType as ObjectType[Key] extends string
    ? Key
    : ObjectType[Key] extends Record<string, unknown>
    ? `${Key & string}.${PathInto<ObjectType[Key]> & string}`
    : never]: unknown;
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
  K extends PathInto<T>
> = K extends `${infer U}.${infer Rest}`
  ? T[U] extends Record<string, unknown>
    ? Choose<T[U], Rest extends PathInto<T[U]> ? Rest : never>
    : T[K]
  : T[K];
