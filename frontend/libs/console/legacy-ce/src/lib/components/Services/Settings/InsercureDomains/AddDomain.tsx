import React, { useState } from 'react';
import { Button } from '../../../../new-components/Button';
import { IconTooltip } from '../../../../new-components/Tooltip';
import { addInsecureDomain } from '../../../../metadata/actions';

import { inputStyles } from '../constants';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';

type Props = { setToggle: any; dispatch: any };

const AddDomain: React.FC<Props> = props => {
  const { setToggle, dispatch } = props;
  const [domainName, setDomainName] = useState('');

  const handleNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setDomainName(e.target.value);
  };

  const saveWithToggle = () => {
    const url = domainName.split(':');
    const host = url[0];
    const port = url[1];
    dispatch(addInsecureDomain(host, port, setToggle));
  };

  return (
    <div className="p-sm border border-gray-200 bg-white rounded-sm w-1/3">
      <div className="mb-sm">
        <b>Add Domain to Insecure TLS Allow List</b>
      </div>
      <div>
        <label className="bg-color=[#4B5563] flex items-center">
          <b>Domain Name</b>
          <IconTooltip message="The domain to be added to the allow list. The format is hostname:port, where port is optional." />
          <LearnMoreLink href="https://hasura.io/docs/latest/deployment/tls-allow-list/" />
        </label>
        <div className="flex pb-md">
          <span className="bg-[#f8fafc] flex items-center px-xs h-10 border border-gray-300 rounded-tl-sm rounded-bl-sm">
            https://
          </span>
          <input
            type="text"
            className={`${inputStyles} rounded-bl-none rounded-tl-none`}
            placeholder="mydomain.com:8080"
            data-test="domain-name"
            value={domainName}
            onChange={handleNameChange}
          />
        </div>
        <Button
          className="mr-sm"
          data-test="cancel-domain"
          onClick={() => {
            setToggle((toggle: boolean) => !toggle);
          }}
        >
          Cancel
        </Button>
        <Button
          type="submit"
          mode="primary"
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
