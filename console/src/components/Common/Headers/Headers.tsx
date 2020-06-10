import React from 'react';
import styles from './Headers.scss';
import DropdownButton from '../DropdownButton/DropdownButton';
import { addPlaceholderHeader } from './utils';

export type Header = {
  type: 'static' | 'env';
  name: string;
  value: string;
};

export const defaultHeader: Header = {
  name: '',
  type: 'static',
  value: '',
};

interface HeadersListProps extends React.ComponentProps<'div'> {
  headers: Header[];
  disabled?: boolean;
  setHeaders: (h: Header[]) => void;
}

const Headers: React.FC<HeadersListProps> = ({
  headers,
  setHeaders,
  disabled = false,
}) => {
  return (
    <React.Fragment>
      {headers.map(({ name, value, type }, i) => {
        const setHeaderType = (e: React.BaseSyntheticEvent) => {
          const newHeaders = JSON.parse(JSON.stringify(headers));
          newHeaders[i].type = e.target.getAttribute('value');
          addPlaceholderHeader(newHeaders);
          setHeaders(newHeaders);
        };

        const setHeaderKey = (e: React.ChangeEvent<HTMLInputElement>) => {
          const newHeaders = JSON.parse(JSON.stringify(headers));
          newHeaders[i].name = e.target.value;
          addPlaceholderHeader(newHeaders);
          setHeaders(newHeaders);
        };

        const setHeaderValue = (e: React.ChangeEvent<HTMLInputElement>) => {
          const newHeaders = JSON.parse(JSON.stringify(headers));
          newHeaders[i].value = e.target.value;
          addPlaceholderHeader(newHeaders);
          setHeaders(newHeaders);
        };

        const removeHeader = () => {
          const newHeaders = JSON.parse(JSON.stringify(headers));
          setHeaders([...newHeaders.slice(0, i), ...newHeaders.slice(i + 1)]);
        };

        return (
          <div
            className={`${styles.display_flex} ${styles.add_mar_bottom_mid}`}
            key={i.toString()}
          >
            <input
              value={name}
              onChange={setHeaderKey}
              placeholder="key"
              className={`form-control ${styles.add_mar_right} ${styles.headerInputWidth}`}
              disabled={disabled}
            />
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
                required={false}
                bsClass={styles.dropdown_button}
                inputVal={value}
                id={`header-value-${i}`}
                inputPlaceHolder={type === 'env' ? 'HEADER_FROM_ENV' : 'value'}
                testId={`header-value-${i}`}
                disabled={disabled}
              />
            </div>
            {i < headers.length - 1 ? (
              <i
                className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
                onClick={removeHeader}
              />
            ) : null}
          </div>
        );
      })}
    </React.Fragment>
  );
};

export default Headers;
