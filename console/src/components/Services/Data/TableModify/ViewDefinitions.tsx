import React from 'react';
import styles from './ModifyTable.scss';
import TextAreaWithCopy from '../../../Common/TextAreaWithCopy/TextAreaWithCopy';
import RawSqlButton from '../Common/Components/RawSqlButton';

export interface ViewDefinitionsProps {
  dispatch: () => void;
  sql: string | Record<string, unknown>;
  source: string;
}

const ViewDefinitions: React.FC<ViewDefinitionsProps> = ({
  dispatch,
  sql,
  source,
}) => (
  <>
    <h4 className={styles.subheading_text}>
      View Definition:
      <span className={styles.add_mar_left}>
        <RawSqlButton
          className={styles.add_mar_right}
          sql={sql}
          dispatch={dispatch}
          source={source}
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
