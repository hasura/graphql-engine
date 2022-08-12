import React, { useState } from 'react';
import Button from '../../../Common/Button/Button';
import { addInsecureDomain } from '../../../../metadata/actions';

import { inputStyles } from '../constants';

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
    <div className="p-sm border border-gray-200 bg-white rounded-sm w-1/3">
      <div className="mb-sm">
        <b>Add Domain to Insecure TLS Allow List</b>
      </div>
      <div>
        <label className="bg-color=[#4B5563]">
          <b>Domain Name</b>
          <span className="text-red-700"> * </span>
        </label>
        <div className="flex pb-md">
          <span className="bg-[#f8fafc] flex items-center px-xs h-10 border border-gray-300 rounded-tl-sm rounded-bl-sm">
            https://
          </span>
          <input
            type="text"
            className={`${inputStyles} rounded-bl-none rounded-tl-none`}
            placeholder="www.google.com"
            data-test="domain-name"
            value={domainName}
            onChange={handleNameChange}
          />
        </div>
        <Button
          className="mr-sm"
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
