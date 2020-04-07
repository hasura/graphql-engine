const columnsCollapsedState = 'data:collapsed';
const defaultColumnsCollapsedState = {};

type CollapesEntry = Record<string, boolean>;
type CollapesState = Record<string, CollapesEntry>;

const setColumnsCollapsedState = (data: CollapesState) => {
  window.localStorage.setItem(columnsCollapsedState, JSON.stringify(data));
};

const getColumnsCollapsedState = (): CollapesState => {
  try {
    const p = window.localStorage.getItem(columnsCollapsedState);
    if (p) {
      return JSON.parse(p);
    }
    window.localStorage.setItem(
      columnsCollapsedState,
      JSON.stringify(defaultColumnsCollapsedState)
    );
    return defaultColumnsCollapsedState;
  } catch (e) {
    console.error(e);
    return defaultColumnsCollapsedState;
  }
};

export const handleCollapseChange = (
  tableName: string,
  schemaName: string,
  collapsedData: CollapesEntry
) => {
  const currentCollapsed = getColumnsCollapsedState();
  const newCollapsed = {
    ...currentCollapsed,
    [`${schemaName}.${tableName}`]: collapsedData,
  };

  setColumnsCollapsedState(newCollapsed);
};

export const getCollapsedColumns = (tableName: string, schemaName: string) => {
  const collapsedData = getColumnsCollapsedState();
  return collapsedData[`${schemaName}.${tableName}`];
};

const columnsOrderState = 'data:order';
const defaultColumnsOrderState = {};

type OrderEntry = { newOrder: number; defaultOrder: number };
type OrderState = Record<string, OrderEntry[]>;

const setColumnsOrderState = (data: OrderState) => {
  window.localStorage.setItem(columnsOrderState, JSON.stringify(data));
};

const getColumnsOrderState = (): OrderState => {
  try {
    const p = window.localStorage.getItem(columnsOrderState);
    if (p) {
      return JSON.parse(p);
    }
    window.localStorage.setItem(
      columnsOrderState,
      JSON.stringify(defaultColumnsOrderState)
    );
    return defaultColumnsOrderState;
  } catch (e) {
    console.error(e);
    return defaultColumnsOrderState;
  }
};

const compareReorderItems = (item1: OrderEntry) => (item2: OrderEntry) =>
  item1.newOrder === item2.newOrder &&
  item1.defaultOrder === item2.defaultOrder;

export const handleOrderChange = (
  tableName: string,
  schemaName: string,
  orderData: OrderEntry[]
) => {
  const currentOrders = getColumnsOrderState();

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
    setColumnsOrderState(currentOrders);
    return;
  }

  setColumnsOrderState({
    ...currentOrders,
    [`${schemaName}.${tableName}`]: newOrders,
  });
};

export const getColumnsOrder = (tableName: string, schemaName: string) => {
  const orderData = getColumnsOrderState();
  return orderData[`${schemaName}.${tableName}`];
};

const pageSizeState = 'data:pageSize';
const defaultPageSizeState = {};

const setPageSizeState = (data: Record<string, number>) => {
  window.localStorage.setItem(pageSizeState, JSON.stringify(data));
};

const getPageSizeState = (): Record<string, number> => {
  try {
    const p = window.localStorage.getItem(pageSizeState);
    if (p) {
      return JSON.parse(p);
    }
    window.localStorage.setItem(
      pageSizeState,
      JSON.stringify(defaultPageSizeState)
    );
    return defaultPageSizeState;
  } catch (e) {
    console.error(e);
    return defaultPageSizeState;
  }
};

export const handlePageSizeStateChange = (
  tableName: string,
  schemaName: string,
  pageSize: number
) => {
  const currentState = getPageSizeState();

  setPageSizeState({
    ...currentState,
    [`${schemaName}.${tableName}`]: pageSize,
  });
};

export const getPageSize = (tableName: string, schemaName: string) => {
  const pageSizeData = getPageSizeState();
  return pageSizeData[`${schemaName}.${tableName}`];
};
