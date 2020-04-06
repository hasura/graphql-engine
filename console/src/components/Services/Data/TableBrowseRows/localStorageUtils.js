const columnsCollapsedState = 'data:collapsed';
const defaultColumnsCollapsedState = {};

/**
 * @param {{
 *  [tableName: string]: {[colName: string]: boolean}
 * }} data
 */
const setColumnsCollapsedState = data => {
  window.localStorage.setItem(columnsCollapsedState, JSON.stringify(data));
};

/**
 * @returns {{
 *  [tableName: string]: {[colName: string]: boolean}
 * }}
 */
const getColumnsCollapsedState = () => {
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

/**
 * @param {string} tableName
 * @param {string} schemaNae
 * @param {{[colName: string]: boolean}} newCollapsedData
 *
 * @returns {void}
 */
export const handleCollapseChange = (tableName, schemaName, collapsedData) => {
  const currentCollapsed = getColumnsCollapsedState();
  const newCollapsed = {
    ...currentCollapsed,
    [`${schemaName}.${tableName}`]: collapsedData,
  };

  setColumnsCollapsedState(newCollapsed);
};

/**
 * @param {string} tableName
 * @param {string} schemaName
 *
 * @returns {{[colName: string]: boolean}|void}
 */
export const getCollapsedColumns = (tableName, schemaName) => {
  const collapsedData = getColumnsCollapsedState();
  return collapsedData[`${schemaName}.${tableName}`];
};

const columnsOrderState = 'data:order';
const defaultColumnsOrderState = {};

/**
 * @param {{
 *  [tableName: string]: {newOrder: number, defaultOrder: number}[]
 * }} data
 */
const setColumnsOrderState = data => {
  window.localStorage.setItem(columnsOrderState, JSON.stringify(data));
};

/**
 * @returns {{
 *  [tableName: string]: {newOrder: number, defaultOrder: number}[]
 * }}
 */
const getColumnsOrderState = () => {
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

const compareReorderItems = item1 => item2 =>
  item1.newOrder === item2.newOrder &&
  item1.defaultOrder === item2.defaultOrder;

/**
 * @param {string} tableName
 * @param {string} schemaName
 *
 * @param {{newOrder: number, defaultOrder: number}[]} orderData
 */
export const handleOrderChange = (tableName, schemaName, orderData) => {
  const currentOrders = getColumnsOrderState();

  // remove duplicates
  const newOrders = [];
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

/**
 * @param {string} tableName
 * @param {string} schemaName
 *
 * @returns {{newOrder: number, defaultOrder: number}[]}
 */
export const getColumnsOrder = (tableName, schemaName) => {
  const orderData = getColumnsOrderState();
  return orderData[`${schemaName}.${tableName}`];
};
