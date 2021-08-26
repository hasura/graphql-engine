import React, { useState } from 'react';
import { connect } from 'react-redux';
import { Dispatch, ReduxState } from '../../../../types';
import Button from '../../../Common/Button/Button';
import AddDomain from './AddDomain';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import { deleteInsecureDomain } from '../../../../metadata/actions';
import { DomainList } from '../../../../metadata/types';

import styles from '../InsercureDomains/AllowInsecureDomain.scss';

type AddDomainProps = {
  dispatch: Dispatch;
  insecureDomains: DomainList[];
};

const InsecureDomains: React.FC<AddDomainProps> = props => {
  const { dispatch, insecureDomains } = props;
  const [toggle, setToggle] = useState(false);

  const handleDeleteDomain = (domain: string) => {
    const confirmMessage = `This will permanently delete the domain.`;
    const isOk = getConfirmation(confirmMessage);
    if (isOk) {
      dispatch(deleteInsecureDomain(domain));
    }
  };

  return (
    <div className={styles.layout}>
      <div className={styles.max_width}>
        <h1 className={styles.header}> Insecure TLS Allow List </h1>
        <p className={styles.sub_heading}>
          Allow your HTTPS integrations (Actions, Event Triggers, Cron
          Triggers,etc) to use self-signed certificates. For more information
          refer to{' '}
          <a
            href="https://hasura.io/docs/latest/graphql/core/api-reference/metadata-api/network.html#metadata-add-host-to-tls-allowlist"
            target="_blank"
            rel="noopener noreferrer"
          >
            docs
          </a>{' '}
          here.
        </p>
        <div className={styles.table_flex}>
          <div className={styles.table_border}>
            <table className={styles.min_width}>
              <thead className={styles.bg_grey}>
                <tr>
                  <th className={`${styles.table_heading} ${styles.padding}`}>
                    DOMAIN
                  </th>
                  <th
                    className={`${styles.table_heading} ${styles.padding} ${styles.align_text_right}`}
                  >
                    MODIFY
                  </th>
                </tr>
              </thead>
              {insecureDomains.length ? (
                insecureDomains.map(domain => (
                  <tbody className={`${styles.table_container}`}>
                    <tr className={styles.upper_border}>
                      <td className={styles.padding} data-test={domain.host}>
                        {domain.host}
                      </td>
                      <td
                        className={`${styles.padding} ${styles.align_text_right}`}
                      >
                        <button
                          className={`${styles.red_text} ${styles.text_button}`}
                          onClick={() => handleDeleteDomain(domain.host)}
                          data-test={`delete-domain-${domain.host}`}
                        >
                          Delete
                        </button>
                      </td>
                    </tr>
                  </tbody>
                ))
              ) : (
                <tbody className={`${styles.table_container}`}>
                  <tr className={styles.upper_border}>
                    <td
                      className={styles.padding}
                      data-test="label-no-domain-found"
                    >
                      No domains added to insecure TLS allow list
                    </td>
                    <td
                      className={`${styles.padding} ${styles.align_text_right}`}
                    />
                  </tr>
                </tbody>
              )}
            </table>
          </div>
        </div>
      </div>
      {!toggle && (
        <Button
          color="white"
          size="xm"
          data-test="add-insecure-domain"
          onClick={() => setToggle(true)}
        >
          Add Domain
        </Button>
      )}

      {toggle && <AddDomain setToggle={setToggle} dispatch={dispatch} />}
    </div>
  );
};

const mapStateToProps = (state: ReduxState) => ({
  insecureDomains: state.metadata.metadataObject?.network?.tls_allowlist ?? [],
});

const mapDispatchToProps = (dispatch: Dispatch) => {
  return {
    dispatch,
  };
};

const connector = connect(mapStateToProps, mapDispatchToProps);

const connectedInsecureDomains = connector(InsecureDomains);

export default connectedInsecureDomains;
