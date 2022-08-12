import React, { useState } from 'react';
import { connect } from 'react-redux';
import { Dispatch, ReduxState } from '../../../../types';
import Button from '../../../Common/Button/Button';
import AddDomain from './AddDomain';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import { deleteInsecureDomain } from '../../../../metadata/actions';
import { DomainList } from '../../../../metadata/types';

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
    <div className="p-lg bg-[#f8fafc]">
      <div className="max-w-[72rem]">
        <h1 className="text-4xl font-bold my-sm"> Insecure TLS Allow List </h1>
        <p>
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
        <div className="mb-md mtsm">
          <div className="overflow-x-auto rounded-sm border-gray-300 border">
            <div className="flex flex-col">
              <div className="bg-[#f2f2f2] flex justify-between p-xs px-sm">
                <div className="text-base font-bold">DOMAIN</div>
                <div className="text-base font-bold">MODIFY</div>
              </div>
              {insecureDomains.length ? (
                insecureDomains.map(domain => (
                  <div className="p-xs px-sm bg-white border-t border-gray-300 flex justify-between">
                    <div data-test={domain.host}>{domain.host}</div>
                    <button
                      className="text-red-700 cursor-pointer"
                      onClick={() => handleDeleteDomain(domain.host)}
                      data-test={`delete-domain-${domain.host}`}
                    >
                      Delete
                    </button>
                  </div>
                ))
              ) : (
                <div className="white border-t border-gray-300">
                  <div className="p-xs" data-test="label-no-domain-found">
                    No domains added to insecure TLS allow list
                  </div>
                </div>
              )}
            </div>
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
