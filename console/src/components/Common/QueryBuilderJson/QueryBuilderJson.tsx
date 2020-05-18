/* eslint-disable no-use-before-define */
/* eslint no-underscore-dangle: 0 */
/* eslint no-shadow: 0 */
import React from 'react';

const styles = require('./QueryBuilderJson.scss');

type ElementProp = object | object[];
type UnselectedElementProp = (string | number)[];
type QueryBuilderJsonProps = {
  element: ElementProp;
  unselectedElements: UnselectedElementProp;
};

const QueryBuilderJson = ({
  element,
  unselectedElements,
}: QueryBuilderJsonProps) => {
  const oBrace = '{ ';
  const cBrace = '}';

  const wrapBraces = (value: object[]) => {
    return (
      <span>
        {oBrace}
        <div className={styles.qb_nested}>{value}</div>
        {cBrace}
      </span>
    );
  };

  const wrapSquareBrackets = (value: object[]) => {
    return (
      <span>
        [<div className={styles.qb_nested}>{value}</div>]
      </span>
    );
  };

  const comma = (show = true) => {
    return show ? ' ,' : '';
  };

  const isCustomJsonObject = (object: object[] | object) => {
    // check if it is an array
    if (object instanceof Array) {
      // check if is an empty array
      if (object.length === 0) {
        return false;
      }
      // check each element of array
      for (let i = 0; i < object.length; i + 1) {
        const objectElement = object[i];

        if (!(objectElement instanceof Object)) {
          return false;
        }

        const objectKeys = Object.keys(objectElement).sort();
        // check if element has 2 values
        if (objectKeys.length !== 2) {
          return false;
        }

        const customJsonKeys = ['key', 'value'].sort();
        // check if element keys are key and value
        for (let j = 0; j < objectKeys.length; j + 1) {
          if (objectKeys[j] !== customJsonKeys[j]) {
            return false;
          }
        }
      }

      return true;
    }

    // check if it is an object and not a React element
    return object instanceof Object && !React.isValidElement(object);
  };

  const isCustomJsonArray = (array: object | object[]) => {
    return array instanceof Array && !isCustomJsonObject(array);
  };

  const displayJsonKeyValue = (
    key: string,
    value: any,
    unselected: boolean
  ) => {
    return (
      <span className={styles.qb_row}>
        <span
          className={`${styles.qb_key} ${unselected &&
            styles.qb_unselected_key}`}
        >
          {`" ${key} "`} :
        </span>
        {displayJsonElement(value)}
      </span>
    );
  };

  // object: customJsonObjectArray ie: array of objects containing key, value fields (helps dealing with non string keys)
  // object: object
  const displayJsonObject = (
    object: ElementProp,
    unselectedElements: UnselectedElementProp
  ) => {
    const _jsonObject: object[] = [];

    // if object convert into customJsonObjectArray
    let objectArray: { key: string; value: any }[];
    if (object instanceof Array) {
      objectArray = object as { key: string; value: any }[];
    } else {
      objectArray = [];
      Object.keys(object).forEach((key, i) => {
        objectArray.push({ key, value: (object as any)[key] });
        // replace unselected key with array position
        if (unselectedElements.includes(key)) {
          /* eslint no-param-reassign: ["error", { "props": false }] */
          unselectedElements[unselectedElements.indexOf(key)] = i;
        }
      });
    }

    objectArray.forEach((_object, i) => {
      const unselected = unselectedElements.includes(i);
      _jsonObject.push(
        <div
          key={i}
          className={`${styles.qb_row} ${unselected &&
            styles.qb_unselected_row}`}
        >
          {displayJsonKeyValue(_object.key, _object.value, unselected)}{' '}
          {comma(i < objectArray.length - 1)}
        </div>
      );
    });

    return wrapBraces(_jsonObject);
  };

  const displayJsonArray = (
    elements: object[],
    unselectedElements: number[]
  ) => {
    const _jsonArray: object[] = [];

    elements.forEach((element, i: number) => {
      const unselected = unselectedElements.includes(i);
      _jsonArray.push(
        <div
          key={i}
          className={`${styles.qb_row} ${unselected &&
            styles.qb_unselected_row}`}
        >
          {displayJsonElement(element)}
          {comma(i < elements.length - 1)}
        </div>
      );
    });

    return wrapSquareBrackets(_jsonArray);
  };

  const displayJsonValue = (value: ElementProp) => {
    return value;
  };

  const displayJsonElement = (
    element: ElementProp,
    unselectedElements: UnselectedElementProp = []
  ) => {
    let _jsonElement = null;

    if (isCustomJsonArray(element)) {
      _jsonElement = displayJsonArray(
        element as object[],
        unselectedElements as number[]
      );
    } else if (isCustomJsonObject(element)) {
      _jsonElement = displayJsonObject(element, unselectedElements);
    } else {
      _jsonElement = displayJsonValue(element);
    }

    return _jsonElement;
  };

  const displayCustomJson = () => {
    return displayJsonElement(element, unselectedElements);
  };

  return displayCustomJson();
};

export default QueryBuilderJson;
