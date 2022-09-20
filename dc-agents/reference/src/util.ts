import { TableName } from "@hasura/dc-api-types";

export const coerceUndefinedToNull = <T>(v: T | undefined): T | null => v === undefined ? null : v;

export const unreachable = (x: never): never => { throw new Error(`Unreachable code reached! The types lied! 😭 Unexpected value: ${x}`) };

export const zip = <T, U>(arr1: T[], arr2: U[]): [T, U][] => {
  const length = Math.min(arr1.length, arr2.length);
  const newArray = Array(length);
  for (let i = 0; i < length; i++) {
    newArray[i] = [arr1[i], arr2[i]];
  }
  return newArray;
};

export function* mapIterable<T, U>(iterable: Iterable<T>, fn: (item: T) => U) {
  for (const x of iterable) {
    yield fn(x);
  }
}

export function* filterIterable<T>(iterable: Iterable<T>, fn: (item: T) => boolean) {
  for (const x of iterable) {
    if (fn(x)) yield x;
  }
}

export function* skipIterable<T>(iterable: Iterable<T>, count: number) {
  let currentCount = 0;
  for (const x of iterable) {
    if (currentCount >= count) {
      yield x;
    } else {
      currentCount++;
    }
  }
}

export function* takeIterable<T>(iterable: Iterable<T>, count: number) {
  let currentCount = 0;
  for (const x of iterable) {
    if (currentCount >= count) return;

    yield x;
    currentCount++;
  }
}

export const reduceAndIterable = (iterable: Iterable<boolean>): boolean => {
  for (const x of iterable) {
    if (x === false) return false;
  }
  return true;
}

export const reduceOrIterable = (iterable: Iterable<boolean>): boolean => {
  for (const x of iterable) {
    if (x === true) return true;
  }
  return false;
}

export const tableNameEquals = (tableName1: TableName) => (tableName2: TableName): boolean => {
  if (tableName1.length !== tableName2.length)
    return false;

  return zip(tableName1, tableName2).every(([n1, n2]) => n1 === n2);
}
