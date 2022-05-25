import React from 'react';
import AceEditor from '../../../Common/AceEditor/BaseEditor';
import Tooltip from '../Common/components/Tooltip';

const DerivedFrom = ({ shouldDerive, parentMutation, toggleDerivation }) => {
  if (!parentMutation) return null;

  const tooltip =
    'This code is generated based on the assumption that operation was derived from another operation. If the assumption is wrong, you can disable the derivation.';
  return (
    <div>
      <h2 className="text-sm font-bold mb-md pb-5 mt-0">
        Derived operation
        <Tooltip id="action-name" text={tooltip} className="ml-5" />
      </h2>
      <div className="mb-5">
        <label className="cursor-pointer" onClick={toggleDerivation}>
          <input
            type="checkbox"
            checked={shouldDerive}
            className="cursor-pointer mr-md legacy-input-fix"
          />
          Generate code with delegation to the derived mutation
        </label>
      </div>
      <AceEditor
        mode="graphql"
        value={parentMutation}
        width={'600px'}
        height={'200px'}
        readOnly
      />
    </div>
  );
};

export default DerivedFrom;
