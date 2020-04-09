const setLSState = (key: string, data: any) => {
  window.localStorage.setItem(key, JSON.stringify(data));
};

const getLSState = (key: string, defaultValue: null | {} = {}) => {
  try {
    const p = window.localStorage.getItem(key);
    if (p) {
      return JSON.parse(p);
    }
    return defaultValue;
  } catch (e) {
    console.error(e);
    setLSState(key, defaultValue);
    return defaultValue;
  }
};

const columnsCollapsedKey = 'data:collapsed';

type CollapesEntry = Record<string, boolean>;
type CollapesState = Record<string, CollapesEntry>;

export const handleCollapseChange = (
  tableName: string,
  schemaName: string,
  collapsedData: CollapesEntry
) => {
  const currentCollapsed = getLSState(columnsCollapsedKey);
  const newCollapsed = {
    ...currentCollapsed,
    [`${schemaName}.${tableName}`]: collapsedData,
  };

  setLSState(columnsCollapsedKey, newCollapsed);
};

export const getCollapsedColumns = (tableName: string, schemaName: string) => {
  const collapsedData = getLSState(columnsCollapsedKey) as CollapesState;
  return collapsedData[`${schemaName}.${tableName}`];
};

const columnsOrderKey = 'data:order';

type OrderEntry = { newOrder: number; defaultOrder: number };
type OrderState = Record<string, OrderEntry[]>;

const compareReorderItems = (item1: OrderEntry) => (item2: OrderEntry) =>
  item1.newOrder === item2.newOrder &&
  item1.defaultOrder === item2.defaultOrder;

export const handleOrderChange = (
  tableName: string,
  schemaName: string,
  orderData: OrderEntry[]
) => {
  const currentOrders = getLSState(columnsOrderKey);

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
    setLSState(columnsOrderKey, currentOrders);
    return;
  }

  setLSState(columnsOrderKey, {
    ...currentOrders,
    [`${schemaName}.${tableName}`]: newOrders,
  });
};

export const getColumnsOrder = (tableName: string, schemaName: string) => {
  const orderData = getLSState(columnsOrderKey) as OrderState;
  return orderData[`${schemaName}.${tableName}`];
};

const pageSizeKey = 'data:pageSize';

export const handlePageSizeStateChange = (pageSize: number) => {
  setLSState(pageSizeKey, pageSize);
};

export const getPageSize = () => {
  return getLSState(pageSizeKey, null);
};
