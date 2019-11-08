import React from 'react';
import styles from '../Actions.scss';
import TabContainer from '../Containers/TabContainer';
import AddManualRelationship from './Components/AddManualRelationship';
import AddedRelationships from './Components/AddedRelationships';
import { findAction } from '../utils';
import { setTypes } from './reducer';
import { generateTableDef } from '../../../Common/utils/pgUtils';

const Relationships = ({
  params,
  allActions,
  dispatch,
  allTables,
  allTypes,
  schemaList,
}) => {
  const { actionName } = params;

  const action = findAction(allActions, actionName);

  const actionOutputTypeName = action.action_defn.output_type;

  const actionOutputType = allTypes.find(t => t.name === actionOutputTypeName);

  const setRelTypes = (relName, refSchema, refTable, fieldMappings) => {
    console.log('types');
    console.log(relName, refSchema, refTable, fieldMappings);
    const types = [...allTypes];
    console.log(types);

    for (let i = 0; i < types.length; i++) {
      const type = types[i];
      if (type.name === actionOutputTypeName) {
        types[i] = {
          ...type,
          relationships: [
            ...(type.relationships || []),
            {
              remote_table: generateTableDef(refTable, refSchema),
              name: relName,
              field_mapping: fieldMappings.reduce((fm, f) => {
                return f.field ? { ...fm, [f.field]: f.refColumn } : fm;
              }, {}),
            },
          ],
        };

        break;
      }
    }

    dispatch(setTypes(types));
  };

  const relationships = actionOutputType.relationships || [];

  return (
    <TabContainer
      params={params}
      allActions={allActions}
      tabName="relationships"
      dispatch={dispatch}
    >
      <div className={`${styles.padd_left_remove} container-fluid`}>
        <div className={`${styles.padd_left_remove} col-xs-10 col-md-10`}>
          <h4 className={styles.subheading_text}>Relationships</h4>
          <AddedRelationships relationships={relationships} />
          <br />
          <AddManualRelationship
            objectType={actionOutputType}
            allTables={allTables}
            schemaList={schemaList}
            dispatch={dispatch}
            stateCb={setRelTypes}
          />
          <hr />
        </div>
      </div>
      <div className={`${styles.fixed} hidden`}>{alert}</div>
    </TabContainer>
  );
};

export default Relationships;
