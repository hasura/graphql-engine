import {
  setLSItem,
  LS_KEYS,
  getParsedLSItem,
} from '../../../../utils/localStorage';

type CollapseEntry = Record<string, boolean>;

export const persistColumnCollapseChange = (
  tableName: string,
  schemaName: string,
  collapsedData: CollapseEntry
) => {
  const currentCollapsed =
    getParsedLSItem(LS_KEYS.dataColumnsCollapsedKey) || {};
  const newCollapsed = {
    ...currentCollapsed,
    [`${schemaName}.${tableName}`]: collapsedData,
  };

  setLSItem(LS_KEYS.dataColumnsCollapsedKey, JSON.stringify(newCollapsed));
};

export const getPersistedCollapsedColumns = (
  tableName: string,
  schemaName: string
) => {
  const collapsedData = getParsedLSItem(LS_KEYS.dataColumnsCollapsedKey) || {};
  return collapsedData[`${schemaName}.${tableName}`];
};

type OrderEntry = { newOrder: number; defaultOrder: number };

const compareReorderItems = (item1: OrderEntry) => (item2: OrderEntry) =>
  item1.newOrder === item2.newOrder &&
  item1.defaultOrder === item2.defaultOrder;

export const persistColumnOrderChange = (
  tableName: string,
  schemaName: string,
  orderData: OrderEntry[]
) => {
  const currentOrders = getParsedLSItem(LS_KEYS.dataColumnsOrderKey) || {};

  // remove duplicates
  const newOrders: OrderEntry[] = [];
  orderData.forEach(item => {
    const sameElements = orderData.filter(compareReorderItems(item));
    const alreadyAdded = newOrders.some(compareReorderItems(item));

    if (sameElements.length % 2 && !alreadyAdded) {
      newOrders.push(item);
    }
  });

  if (!newOrders.length) {
    delete currentOrders[`${schemaName}.${tableName}`];
    setLSItem(LS_KEYS.dataColumnsOrderKey, JSON.stringify(currentOrders));
    return;
  }

  setLSItem(
    LS_KEYS.dataColumnsOrderKey,
    JSON.stringify({
      ...currentOrders,
      [`${schemaName}.${tableName}`]: newOrders,
    })
  );
};

export const getPersistedColumnsOrder = (
  tableName: string,
  schemaName: string
) => {
  const orderData = getParsedLSItem(LS_KEYS.dataColumnsOrderKey);
  return orderData ? orderData[`${schemaName}.${tableName}`] : [];
};

export const persistPageSizeChange = (pageSize: number) => {
  setLSItem(LS_KEYS.dataPageSizeKey, JSON.stringify(pageSize));
};

export const getPersistedPageSize = () => {
  return getParsedLSItem(LS_KEYS.dataPageSizeKey);
};
