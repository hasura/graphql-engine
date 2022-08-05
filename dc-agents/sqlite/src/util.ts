import { TableName } from "./types";

export const coerceUndefinedToNull = <T>(v: T | undefined): T | null => v === undefined ? null : v;

export const coerceUndefinedOrNullToEmptyArray = <T>(v: Array<T> | undefined | null): Array<T> => v == null ? [] : v;

export const coerceUndefinedOrNullToEmptyRecord = <K extends string | number | symbol, V>(v: Record<K,V> | undefined | null): Record<K,V> => v == null ? {} as Record<K,V> : v;

export const unreachable = (x: never): never => { throw new Error(`Unreachable code reached! The types lied! 😭 Unexpected value: ${x}`) };

export const zip = <T, U>(arr1: T[], arr2: U[]): [T,U][] => {
  const length = Math.min(arr1.length, arr2.length);
  const newArray = Array(length);
  for (let i = 0; i < length; i++) {
    newArray[i] = [arr1[i], arr2[i]];
  }
  return newArray;
};

export const crossProduct = <T, U>(arr1: T[], arr2: U[]): [T,U][] => {
  return arr1.flatMap(a1 => arr2.map(a2 => [a1, a2]) as [T,U][]);
};

export function omap<V,O>(m: { [x: string]: V; },f: (k: string, v: V) => O) {
  return Object.keys(m).map(k => f(k, m[k]))
}

export function stringToBool(x: string | null | undefined): boolean {
  return (/1|true|t|yes|y/i).test(x || '');
}

export function last<T>(x: Array<T>): T {
  return x[x.length - 1];
}

export function logDeep(msg: string, myObject: any): void {
  const util = require('util');
  console.log(msg, util.inspect(myObject, {showHidden: true, depth: null, colors: true}));
}

export function isEmptyObject(obj: Record<string, any>): boolean {
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

export const tableNameEquals = (tableName1: TableName) => (tableName2: TableName): boolean => {
  if (tableName1.length !== tableName2.length)
    return false;

  return zip(tableName1, tableName2).every(([n1, n2]) => n1 === n2);
}
