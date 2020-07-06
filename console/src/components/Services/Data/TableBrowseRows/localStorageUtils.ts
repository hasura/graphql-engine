import { getLSItem, setLSItem } from '../../../../utils/localstorage';

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

const columnsCollapsedKey = 'data:collapsed';

type CollapseEntry = Record<string, boolean>;

export const persistColumnCollapseChange = (
  tableName: string,
  schemaName: string,
  collapsedData: CollapseEntry
) => {
  const state = getLSItem(columnsCollapsedKey);
  const currentCollapsed = parseLSState(state) || {};
  const newCollapsed = {
    ...currentCollapsed,
    [`${schemaName}.${tableName}`]: collapsedData,
  };

  setLSItem(columnsCollapsedKey, JSON.stringify(newCollapsed));
};

export const getPersistedCollapsedColumns = (
  tableName: string,
  schemaName: string
) => {
  const collapsedData = parseLSState(getLSItem(columnsCollapsedKey)) || {};
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

const pageSizeKey = 'data:pageSize';

export const persistPageSizeChange = (pageSize: number) => {
  setLSItem(pageSizeKey, JSON.stringify(pageSize));
};

export const getPersistedPageSize = () => {
  return parseLSState(getLSItem(pageSizeKey));
};
