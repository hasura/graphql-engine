import { FunctionName, TableName, TargetName } from "@hasura/dc-api-types";

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

export const mapObject = <T, U>(obj: Record<string, T>, fn: (entry: [string, T]) => [string, U]): Record<string, U> => {
  return Object.fromEntries(Object.entries(obj).map(fn));
}

export const mapObjectValues = <T, U>(obj: Record<string, T>, fn: (value: T, propertyName: string) => U): Record<string, U> => {
  return Object.fromEntries(Object.entries(obj).map(([prop, val]) => [prop, fn(val, prop)]));
}

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

export const nameEquals = (name1: TableName | FunctionName) => (name2: TableName | FunctionName): boolean => {
  if (name1.length !== name2.length)
    return false;

  return zip(name1, name2).every(([n1, n2]) => n1 === n2);
}

export const targetNameEquals = (name1: TargetName) => (name2: TargetName): boolean => {
  switch (name1.type) {
    case 'table':
      return name2.type === "table" && nameEquals(name1.table)(name2.table);
    case 'function':
      return name2.type === "function" && nameEquals(name1.function)(name2.function);
    case 'interpolated':
      return name2.type === "interpolated" && name1.interpolated == name2.interpolated;
    default:
      return unreachable(name1["type"]);
  }
}
