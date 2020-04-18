import React from 'react';

import GraphQLEditor from '../../../Common/AceEditor/SDLEditor';
import { ToolTip, Heading, Box } from '../../../UIKit/atoms';
import styles from '../Common/components/Styles.scss';

const DerivedFrom = ({ shouldDerive, parentMutation, toggleDerivation }) => {
  if (!parentMutation) return null;

  const tooltipText =
    'This code is generated based on the assumption that mutation was derived from another mutation. If the assumption is wrong, you can disable the derivation.';

  return (
    <div>
      <Heading as="h2" mb="20px" fontSize="15px" mt="0px">
        Derived mutation
        <ToolTip message={tooltipText} ml="sm" />
      </Heading>
      <Box mb="20px">
        <label className={`${styles.cursorPointer}`} onClick={toggleDerivation}>
          <input
            type="checkbox"
            checked={shouldDerive}
            className={`${styles.cursorPointer} ${styles.add_mar_right_mid}`}
          />
          Generate code with delegation to the derived mutation
        </label>
      </Box>
      <GraphQLEditor
        value={parentMutation}
        width={'600px'}
        height={'200px'}
        readOnly
      />
    </div>
  );
};

export default DerivedFrom;
