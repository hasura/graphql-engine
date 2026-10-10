import { Table } from '@hasura/shared/types';
import { extractTableInfo } from '@hasura/shared/utils';

export const areTablesEqual = (table1: Table, table2: Table) => {
  if (typeof table1 !== typeof table2) {
    return false;
  }

  if (
    table1 === null ||
    table1 === undefined ||
    table2 === null ||
    table2 === undefined
  ) {
    return (table1 ?? null) === (table2 ?? null);
  }

  if (Array.isArray(table1)) {
    if (!Array.isArray(table2) || table1.length !== table2.length) {
      return false;
    }

    return table1.every((t1, i) => t1 === table2[i]);
  }

  if (typeof table1 !== 'object' || typeof table2 !== 'object') {
    return false;
  }

  const keysOfTable1 = Object.keys(table1);
  const keysOfTable2 = Object.keys(table2);

  if (keysOfTable1.length !== keysOfTable2.length) {
    return false;
  }

  return keysOfTable1.every(
    (key) =>
      table1[key as keyof typeof table1] === table2[key as keyof typeof table2],
  );
};

export const areTablesEqualCoalesce = (entityA: unknown, entityB: unknown) => {
  const infoA = extractTableInfo(entityA as Table);
  const infoB = extractTableInfo(entityB as Table);

  return (
    infoA && infoB && infoA.name === infoB.name && infoA.schema === infoB.schema
  );
};
