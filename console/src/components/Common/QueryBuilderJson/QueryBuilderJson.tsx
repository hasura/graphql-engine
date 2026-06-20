/* eslint-disable @typescript-eslint/no-use-before-define */
import React from 'react';
import styles from './QueryBuilderJson.scss';

type ObjectType = Record<string, unknown>;
type ElementProp = ObjectType | ObjectType[];
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

  const wrapBraces = (value: React.ReactNode) => {
    return (
      <span>
        {oBrace}
        <div className={styles.qb_nested}>{value}</div>
        {cBrace}
      </span>
    );
  };

  const wrapSquareBrackets = (value: React.ReactNode) => {
    return (
      <span>
        [<div className={styles.qb_nested}>{value}</div>]
      </span>
    );
  };

  const comma = (show = true) => {
    return show ? ' ,' : '';
  };

  const isCustomJsonObject = (object: ObjectType | ObjectType[]) => {
    // check if it is an array
    if (object instanceof Array) {
      // check if is an empty array
      if (object.length === 0) {
        return false;
      }
      // check each element of array
      for (let i = 0; i < object.length; i++) {
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
        for (let j = 0; j < objectKeys.length; j++) {
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

  const isCustomJsonArray = (array: ObjectType | ObjectType[]) => {
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
          className={`${styles.qb_key} ${
            unselected && styles.qb_unselected_key
          }`}
        >
          &quot; {key} &quot; :
        </span>
        {displayJsonElement(value)}
      </span>
    );
  };

  // object: customJsonObjectArray ie: array of objects containing key, value fields (helps dealing with non string keys)
  // object: object
  const displayJsonObject = (
    object: ElementProp,
    unselectedElmnts: UnselectedElementProp
  ) => {
    const jsonObject: React.ReactNode[] = [];

    // if object convert into customJsonObjectArray
    let objectArray: { key: string; value: any }[];
    if (object instanceof Array) {
      objectArray = object as { key: string; value: any }[];
    } else {
      objectArray = [];
      Object.keys(object).forEach((key, i) => {
        objectArray.push({ key, value: (object as any)[key] });
        // replace unselected key with array position
        if (unselectedElmnts.includes(key)) {
          /* eslint no-param-reassign: ["error", { "props": false }] */
          unselectedElmnts[unselectedElmnts.indexOf(key)] = i;
        }
      });
    }

    objectArray.forEach((_object, i) => {
      const unselected = unselectedElmnts.includes(i);
      jsonObject.push(
        <div
          key={i}
          className={`${styles.qb_row} ${
            unselected && styles.qb_unselected_row
          }`}
        >
          {displayJsonKeyValue(_object.key, _object.value, unselected)}{' '}
          {comma(i < objectArray.length - 1)}
        </div>
      );
    });

    return wrapBraces(jsonObject);
  };

  const displayJsonArray = (
    elements: ObjectType[],
    unselectedElmnts: number[]
  ) => {
    const jsonArray: React.ReactNode[] = [];

    elements.forEach((elmnt, i: number) => {
      const unselected = unselectedElmnts.includes(i);
      jsonArray.push(
        <div
          key={i}
          className={`${styles.qb_row} ${
            unselected && styles.qb_unselected_row
          }`}
        >
          {displayJsonElement(elmnt)}
          {comma(i < elements.length - 1)}
        </div>
      );
    });

    return wrapSquareBrackets(jsonArray);
  };

  const displayJsonValue = (value: ElementProp) => {
    return value;
  };

  const displayJsonElement = (
    elmnt: ElementProp,
    unselectedElmnts: UnselectedElementProp = []
  ) => {
    let jsonElement: React.ReactNode = null;

    if (isCustomJsonArray(elmnt)) {
      jsonElement = displayJsonArray(
        elmnt as ObjectType[],
        unselectedElmnts as number[]
      );
    } else if (isCustomJsonObject(elmnt)) {
      jsonElement = displayJsonObject(elmnt, unselectedElmnts);
    } else {
      jsonElement = displayJsonValue(elmnt);
    }

    return jsonElement;
  };

  const displayCustomJson = () => {
    return displayJsonElement(element, unselectedElements);
  };

  return displayCustomJson();
};

export default QueryBuilderJson;
