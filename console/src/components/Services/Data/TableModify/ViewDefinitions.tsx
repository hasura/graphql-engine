import React from 'react';
import styles from './ModifyTable.scss';
import TextAreaWithCopy from '../../../Common/TextAreaWithCopy/TextAreaWithCopy';
import RawSqlButton from '../Common/Components/RawSqlButton';

export interface ViewDefinitionsProps {
  dispatch: () => void;
  sql: string | object;
}

const ViewDefinitions: React.FC<ViewDefinitionsProps> = ({ dispatch, sql }) => (
  <>
    <h4 className={styles.subheading_text}>
      View Definition:
      <span className={styles.add_mar_left}>
        <RawSqlButton
          className={styles.add_mar_right}
          sql={sql}
          dispatch={dispatch}
          data-test="modify-view"
        >
          Modify
        </RawSqlButton>
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
