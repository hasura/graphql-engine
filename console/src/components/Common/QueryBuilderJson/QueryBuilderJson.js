import React from 'react';
import PropTypes from 'prop-types';

const styles = require('./QueryBuilderJson.scss');

class QueryBuilderJson extends React.Component {
  static propTypes = {
    element: PropTypes.object,
    unselectedElements: PropTypes.array,
  };

  render() {
    const oBrace = '{ ';
    const cBrace = '}';

    const wrapBraces = value => {
      return (
        <span>
          {oBrace}
          <div className={styles.qb_nested}>{value}</div>
          {cBrace}
        </span>
      );
    };

    const wrapSquareBrackets = value => {
      return (
        <span>
          [<div className={styles.qb_nested}>{value}</div>]
        </span>
      );
    };

    const comma = (show = true) => {
      return show ? ' ,' : '';
    };

    const isCustomJsonObject = object => {
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

    const isCustomJsonArray = array => {
      return array instanceof Array && !isCustomJsonObject(array);
    };

    const displayJsonKeyValue = (key, value, unselected) => {
      /* eslint-disable no-use-before-define */
      return (
        <span className={styles.qb_row}>
          <span
            className={
              styles.qb_key + ' ' + (unselected && styles.qb_unselected_key)
            }
          >
            " {key} " :
          </span>
          {displayJsonElement(value)}
        </span>
      );
      /* eslint-enable no-use-before-define */
    };

    // object: customJsonObjectArray ie: array of objects containing key, value fields (helps dealing with non string keys)
    // object: object
    const displayJsonObject = (object, unselectedElements) => {
      const _jsonObject = [];

      // if object convert into customJsonObjectArray
      let objectArray;
      if (object instanceof Array) {
        objectArray = object;
      } else {
        objectArray = [];
        Object.keys(object).forEach((key, i) => {
          objectArray.push({ key: key, value: object[key] });
          // replace unselected key with array position
          if (unselectedElements.includes(key)) {
            unselectedElements[unselectedElements.indexOf(key)] = i;
          }
        });
      }

      objectArray.forEach((_object, i) => {
        const unselected = unselectedElements.includes(i);
        _jsonObject.push(
          <div
            key={i}
            className={
              styles.qb_row + ' ' + (unselected && styles.qb_unselected_row)
            }
          >
            {displayJsonKeyValue(_object.key, _object.value, unselected)}{' '}
            {comma(i < objectArray.length - 1)}
          </div>
        );
      });

      return wrapBraces(_jsonObject);
    };

    const displayJsonArray = (elements, unselectedElements) => {
      const _jsonArray = [];

      elements.forEach((element, i) => {
        /* eslint-disable no-use-before-define */
        const unselected = unselectedElements.includes(i);
        _jsonArray.push(
          <div
            key={i}
            className={
              styles.qb_row + ' ' + (unselected && styles.qb_unselected_row)
            }
          >
            {displayJsonElement(element)}
            {comma(i < elements.length - 1)}
          </div>
        );
        /* eslint-enable no-use-before-define */
      });

      return wrapSquareBrackets(_jsonArray);
    };

    const displayJsonValue = value => {
      return value;
    };

    const displayJsonElement = (element, unselectedElements = []) => {
      let _jsonElement = null;

      if (isCustomJsonArray(element)) {
        _jsonElement = displayJsonArray(element, unselectedElements);
      } else if (isCustomJsonObject(element)) {
        _jsonElement = displayJsonObject(element, unselectedElements);
      } else {
        _jsonElement = displayJsonValue(element);
      }

      return _jsonElement;
    };

    const displayCustomJson = () => {
      const { element, unselectedElements } = this.props;

      return displayJsonElement(element, unselectedElements);
    };

    return displayCustomJson();
  }
}

export default QueryBuilderJson;
