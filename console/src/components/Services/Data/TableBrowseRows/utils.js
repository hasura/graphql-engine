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
 *
 * @param {string} tableName
 * @param {{[colName: string]: boolean}} newCollapsedData
 *
 * @returns {void}
 */
export const handleCollapseChange = (tableName, collapsedData) => {
  const currentCollapsed = getColumnsCollapsedState();
  const newCollapsed = {
    ...currentCollapsed,
    [tableName]: collapsedData,
  };

  setColumnsCollapsedState(newCollapsed);
};

/**
 * @param {string} tableName
 *
 * @returns {{[colName: string]: boolean}|void}
 */
export const getCollapsedColumns = tableName => {
  const collapsedData = getColumnsCollapsedState();
  return collapsedData[tableName];
};

const columnsOrderState = 'data:order';
const defaultColumnsOrderState = {};

/**
 * @param {{
 *  [tableName: string]: {[colName: string]: boolean}
 * }} data
 */
const setColumnsOrderState = data => {
  window.localStorage.setItem(columnsOrderState, JSON.stringify(data));
};

/**
 * @param {string} tableName
 *
 * @returns {{
 *  [tableName: string]: {[colName: string]: boolean}
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

/**
 * @param {string} tableName
 * @param {any} orderData
 */
export const handleOrderChange = (tableName, orderData) => {
  const currentOrders = getColumnsOrderState();
  const newOrders = {
    ...currentOrders,
    [tableName]: orderData,
  };

  setColumnsOrderState(newOrders);
};

export const getColumnsOrder = tableName => {
  const orderData = getColumnsOrderState();
  return orderData[tableName];
};
