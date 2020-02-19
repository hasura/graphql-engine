import React from 'react';
import styles from './Headers.scss';
import DropdownButton from '../DropdownButton/DropdownButton';
import { addPlaceholderHeader } from './utils';

const Headers = ({ headers, setHeaders }) => {
  return headers.map(({ name, value, type }, i) => {
    const setHeaderType = e => {
      const newHeaders = JSON.parse(JSON.stringify(headers));
      newHeaders[i].type = e.target.getAttribute('value');
      addPlaceholderHeader(newHeaders);
      setHeaders(newHeaders);
    };

    const setHeaderKey = e => {
      const newHeaders = JSON.parse(JSON.stringify(headers));
      newHeaders[i].name = e.target.value;
      addPlaceholderHeader(newHeaders);
      setHeaders(newHeaders);
    };

    const setHeaderValue = e => {
      const newHeaders = JSON.parse(JSON.stringify(headers));
      newHeaders[i].value = e.target.value;
      addPlaceholderHeader(newHeaders);
      setHeaders(newHeaders);
    };

    const removeHeader = () => {
      const newHeaders = JSON.parse(JSON.stringify(headers));
      setHeaders([...newHeaders.slice(0, i), ...newHeaders.slice(i + 1)]);
    };

    const getHeaderNameInput = () => {
      return (
        <input
          value={name}
          onChange={setHeaderKey}
          placeholder="key"
          className={`form-control ${styles.add_mar_right} ${styles.headerInputWidth}`}
        />
      );
    };

    const getHeaderValueInput = () => {
      return (
        <div className={styles.headerInputWidth}>
          <DropdownButton
            dropdownOptions={[
              { display_text: 'Value', value: 'static' },
              { display_text: 'From env var', value: 'env' },
            ]}
            title={type === 'env' ? 'From env var' : 'Value'}
            dataKey={type === 'env' ? 'env' : 'static'}
            onButtonChange={setHeaderType}
            onInputChange={setHeaderValue}
            required
            bsClass={styles.dropdown_button}
            inputVal={value}
            id={`header-value-${i}`}
            inputPlaceHolder={type === 'env' ? 'HEADER_FROM_ENV' : 'value'}
            testId={`header-value-${i}`}
          />
        </div>
      );
    };

    const getRemoveButton = () => {
      if (i === headers.length - 1) return null;
      return (
        <i
          className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
          onClick={removeHeader}
        />
      );
    };

    return (
      <div className={`${styles.display_flex} ${styles.add_mar_bottom_mid}`}>
        {getHeaderNameInput()}
        {getHeaderValueInput()}
        {getRemoveButton()}
      </div>
    );
  });
};

export default Headers;
