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

const compareReorderItems = item1 => item2 =>
  item1.newOrder === item2.newOrder &&
  item1.defaultOrder === item2.defaultOrder;

/**
 * @param {string} tableName
 * @param {any} orderData
 */
export const handleOrderChange = (tableName, orderData) => {
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
    delete currentOrders[tableName];
    setColumnsOrderState(currentOrders);
    return;
  }

  setColumnsOrderState({
    ...currentOrders,
    [tableName]: newOrders,
  });
};

export const getColumnsOrder = tableName => {
  const orderData = getColumnsOrderState();
  return orderData[tableName];
};
