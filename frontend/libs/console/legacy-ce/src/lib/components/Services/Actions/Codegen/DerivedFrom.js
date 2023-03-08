import React from 'react';
import AceEditor from '../../../Common/AceEditor/BaseEditor';
import { IconTooltip } from '../../../../new-components/Tooltip';

const DerivedFrom = ({ shouldDerive, parentMutation, toggleDerivation }) => {
  if (!parentMutation) return null;

  const tooltip =
    'This code is generated based on the assumption that operation was derived from another operation. If the assumption is wrong, you can disable the derivation.';
  return (
    <div>
      <h2 className="text-sm font-bold pb-5 mt-0">
        Derived operation
        <IconTooltip message={tooltip} />
      </h2>
      <div className="mb-5">
        <input
          id="derivedFromInputId"
          type="checkbox"
          checked={shouldDerive}
          className="cursor-pointer mr-md legacy-input-fix"
          // Force margin to mitigate bug introduced by a too much restrictive reset
          style={{ margin: '0' }}
        />
        <label
          htmlFor="derivedFromInputId"
          className="cursor-pointer"
          onClick={toggleDerivation}
          // Force margin to mitigate bug introduced by a too much restrictive reset
          style={{ margin: '0', marginLeft: '0.5rem' }}
        >
          Generate code with delegation to the derived mutation
        </label>
      </div>
      <AceEditor
        mode="graphql"
        value={parentMutation}
        width={'600px'}
        height={'200px'}
        readOnly
        setOptions={{ useWorker: false }}
      />
    </div>
  );
};

export default DerivedFrom;
