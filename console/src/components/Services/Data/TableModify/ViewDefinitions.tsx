import React from 'react';
import styles from './ModifyTable.scss';
import TextAreaWithCopy from '../../../Common/TextAreaWithCopy/TextAreaWithCopy';
import Button from '../../../Common/Button/Button';

export interface ViewDefinitionsProps {
  modifyViewOnClick: () => void;
  sql: string | object;
}

const ViewDefinitions: React.FC<ViewDefinitionsProps> = ({
  modifyViewOnClick,
  sql,
}) => (
  <>
    <h4 className={styles.subheading_text}>
      View Definition:
      <span className={styles.add_mar_left}>
        <Button
          type="submit"
          size="xs"
          className={styles.add_mar_right}
          onClick={modifyViewOnClick}
          data-test="modify-view"
        >
          Modify
        </Button>
      </span>
    </h4>

    <TextAreaWithCopy
      copyText={sql}
      textLanguage="sql"
      id="copyCustomFunctionSQL"
    />
  </>
);

export default ViewDefinitions;
