import { getLSItem, setLSItem, LS_KEYS } from '../../../../utils/localStorage';

export const parseLSState = (data: any) => {
  try {
    if (data) {
      return JSON.parse(data);
    }
    return null;
  } catch {
    return null;
  }
};

type CollapseEntry = Record<string, boolean>;

export const persistColumnCollapseChange = (
  tableName: string,
  schemaName: string,
  collapsedData: CollapseEntry
) => {
  const state = getLSItem(LS_KEYS.dataColumnsCollapsedKey);
  const currentCollapsed = parseLSState(state) || {};
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
  const collapsedData =
    parseLSState(getLSItem(LS_KEYS.dataColumnsCollapsedKey)) || {};
  return collapsedData[`${schemaName}.${tableName}`];
};

const columnsOrderKey = 'data:order';

type OrderEntry = { newOrder: number; defaultOrder: number };

const compareReorderItems = (item1: OrderEntry) => (item2: OrderEntry) =>
  item1.newOrder === item2.newOrder &&
  item1.defaultOrder === item2.defaultOrder;

export const persistColumnOrderChange = (
  tableName: string,
  schemaName: string,
  orderData: OrderEntry[]
) => {
  const currentOrders = parseLSState(getLSItem(columnsOrderKey)) || {};

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
    setLSItem(columnsOrderKey, JSON.stringify(currentOrders));
    return;
  }

  setLSItem(
    columnsOrderKey,
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
  const orderData = parseLSState(getLSItem(columnsOrderKey));
  return orderData ? orderData[`${schemaName}.${tableName}`] : [];
};

export const persistPageSizeChange = (pageSize: number) => {
  setLSItem(LS_KEYS.dataPageSizeKey, JSON.stringify(pageSize));
};

export const getPersistedPageSize = () => {
  return parseLSState(getLSItem(LS_KEYS.dataPageSizeKey));
};
