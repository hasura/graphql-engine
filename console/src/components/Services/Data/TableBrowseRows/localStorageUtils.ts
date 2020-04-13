export const setLSState = (key: string, data: string) => {
  window.localStorage.setItem(key, data);
};

export const getLSState = (key: string) => {
  return window.localStorage.getItem(key);
};

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

type CollapesEntry = Record<string, boolean>;

export const persistColumnCollapseChange = (
  tableName: string,
  schemaName: string,
  collapsedData: CollapesEntry
) => {
  const state = getLSState(columnsCollapsedKey);
  const currentCollapsed = parseLSState(state) || {};
  const newCollapsed = {
    ...currentCollapsed,
    [`${schemaName}.${tableName}`]: collapsedData,
  };

  setLSState(columnsCollapsedKey, JSON.stringify(newCollapsed));
};

export const getPersistedCollapsedColumns = (
  tableName: string,
  schemaName: string
) => {
  const collapsedData = parseLSState(getLSState(columnsCollapsedKey)) || {};
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
  const currentOrders = parseLSState(getLSState(columnsOrderKey)) || {};

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
    setLSState(columnsOrderKey, JSON.stringify(currentOrders));
    return;
  }

  setLSState(
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
  const orderData = parseLSState(getLSState(columnsOrderKey));
  return orderData ? orderData[`${schemaName}.${tableName}`] : [];
};

const pageSizeKey = 'data:pageSize';

export const persistPageSizeChange = (pageSize: number) => {
  setLSState(pageSizeKey, JSON.stringify(pageSize));
};

export const getPersistedPageSize = () => {
  return parseLSState(getLSState(pageSizeKey));
};
