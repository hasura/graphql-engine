import React from 'react';
import styles from '../../../../Common/TableCommon/TableStyles.scss';
import RelationshipEditor from './RelationshipEditor';
import { isEmpty } from '../../../../Common/utils/jsUtils';

const AddedRelationships = ({ relationships, dispatch }) => {
  if (isEmpty(relationships)) {
    return null;
  }

  return (
    <div className={styles.tableContainer}>
      <table
        className={`${
          styles.table
        } table table-bordered table-striped table-hover`}
      >
        <thead>
          {/*<tr>*/}
          {/*{['Relationships'].map((s, i) => (*/}
          {/*<th key={i}>{s}</th>*/}
          {/*))}*/}
          {/*</tr>*/}
        </thead>
        <tbody>
          {relationships.map(rel => {
            const column1 = (
              <RelationshipEditor
                dispatch={dispatch}
                key={rel.name}
                relConfig={rel}
              />
            );
            return <tr>{column1}</tr>;
          })}
        </tbody>
      </table>
    </div>
  );
};

export default AddedRelationships;
