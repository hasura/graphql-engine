import React, { useState } from 'react';
import Button from '../../../Common/Button/Button';
import { addInsecureDomain } from '../../../../metadata/actions';

import styles from '../InsercureDomains/AllowInsecureDomain.scss';

type Props = { setToggle: any; dispatch: any };

const AddDomain: React.FC<Props> = props => {
  const { setToggle, dispatch } = props;
  const [domainName, setDomainName] = useState('');

  const handleNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setDomainName(e.target.value);
  };

  const saveWithToggle = () => {
    dispatch(addInsecureDomain(domainName, setToggle));
  };

  return (
    <div className={styles.dialog_box}>
      <div className={styles.add_mar_bottom_mid}>
        <b>Add Domain to Insecure TLS Allow List</b>
      </div>
      <div>
        <label className={styles.heading_style}>
          <b>Domain Name</b>
          <span className={styles.text_red_700}> * </span>
        </label>
        <div className={styles.flex_pad}>
          <span className={`${styles.http_box} ${styles.inline_block}`}>
            https://
          </span>
          <input
            type="text"
            className={`form-control input ${styles.http_input} ${styles.inline_block}`}
            placeholder="www.google.com"
            data-test="domain-name"
            value={domainName}
            onChange={handleNameChange}
          />
        </div>
        <Button
          className={`${styles.add_mar_small}`}
          color="white"
          size="xm"
          data-test="cancel-domain"
          onClick={() => {
            setToggle((toggle: boolean) => !toggle);
          }}
        >
          Cancel
        </Button>
        <Button
          type="submit"
          color="yellow"
          size="sm"
          className={styles.add_mar_right}
          data-test="add-tls-allow-list"
          onClick={saveWithToggle}
        >
          Add to Allow List
        </Button>
      </div>
    </div>
  );
};

export default AddDomain;
