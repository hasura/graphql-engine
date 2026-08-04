import { ErrorResponseType, TableName, Target } from "@hasura/dc-api-types";

export const coerceUndefinedToNull = <T>(v: T | undefined): T | null => v === undefined ? null : v;

export const coerceUndefinedOrNullToEmptyArray = <T>(v: T[] | undefined | null): T[] => v == null ? [] : v;

export const coerceUndefinedOrNullToEmptyRecord = <V>(v: Record<string, V> | undefined | null): Record<string, V> => v == null ? {} : v;

export const unreachable = (x: never): never => { throw new Error(`Unreachable code reached! The types lied! 😭 Unexpected value: ${x}`) };

export const zip = <T, U>(arr1: T[], arr2: U[]): [T,U][] => {
  const length = Math.min(arr1.length, arr2.length);
  const newArray = Array(length);
  for (let i = 0; i < length; i++) {
    newArray[i] = [arr1[i], arr2[i]];
  }
  return newArray;
};

export const mapObject = <T, U>(obj: Record<string, T>, fn: (entry: [string, T]) => [string, U]): Record<string, U> => {
  return Object.fromEntries(Object.entries(obj).map(fn));
}

export const mapObjectToArray = <T, U>(obj: Record<string, T>, fn: (entry: [string, T], index: number) => U): Array<U> => {
  return Object.entries(obj).map(fn);
}

export const crossProduct = <T, U>(arr1: T[], arr2: U[]): [T,U][] => {
  return arr1.flatMap(a1 => arr2.map<[T,U]>(a2 => [a1, a2]));
};

export function last<T>(x: T[]): T {
  return x[x.length - 1];
}

export function logDeep(msg: string, myObject: unknown): void {
  const util = require('util');
  console.log(msg, util.inspect(myObject, {showHidden: true, depth: null, colors: true}));
}

export function isEmptyObject(obj: Record<string, unknown>): boolean {
  return Object.keys(obj).length === 0;
}

/**
 * Usage: `await delay(5000)`
 *
 * @param ms
 * @returns
 */
export function delay(ms: number): Promise<void> {
  return new Promise( resolve => setTimeout(resolve, ms) );
}

export const tableNameEquals = (tableName1: TableName) => (target: Target): boolean => {
  if(target.type != 'table') {
    return false;
  }
  return stringArrayEquals(tableName1)(target.name);
}

export const tableToTarget = (tableName: TableName): Target => {
  return {
    type: 'table',
    name: tableName
  }
}

export const stringArrayEquals = (arr1: string[]) => (arr2: string[]): boolean => {
  if (arr1.length !== arr2.length)
    return false;

  return zip(arr1, arr2).every(([n1, n2]) => n1 === n2);
}

export class ErrorWithStatusCode extends Error {
  code: number;
  type: ErrorResponseType;
  details: Record<string, unknown>;
  constructor(message: string, code: number, details: Record<string, unknown>) {
    super(message);
    this.code = code;
    this.type = 'uncaught-error';
    this.details = details;
  }
  public static mutationPermissionCheckFailure(message: string, details: Record<string, unknown>): ErrorWithStatusCode {
    const cls = new ErrorWithStatusCode(message, 400, details);
    cls.type = 'mutation-permission-check-failure';
    return cls;
  }
}

/**
 * @param inputSequence 
 * @param asyncFn 
 * @returns Promise<Array<Result>>
 * 
 * This function exists to sequence promise generating inputs and a matching function.
 * Promise.all executes in parallel which is not always desired behaviour.
 */
export async function asyncSequenceFromInputs<Input, Result>(inputSequence: Input[], asyncFn: (input: Input) => Promise<Result>): Promise<Array<Result>> {
  const results = [];
  for (const input of inputSequence) {
    results.push(await asyncFn(input));
  }
  return results;
}