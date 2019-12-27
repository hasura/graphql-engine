import React from 'react';
import styles from '../Actions.scss';
import ActionContainer from '../Containers/ActionContainer';
import AddManualRelationship from './Components/AddManualRelationship';
import AddedRelationships from './Components/AddedRelationships';
import { findAction } from '../utils';
import { setTypes } from './reducer';
import { generateTableDef } from '../../../Common/utils/pgUtils';
import { unwrapType } from '../../../../shared/utils/wrappingTypeUtils';

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

  const actionOutputTypeName = unwrapType(action.action_defn.output_type)
    .typename;

  const actionOutputType = allTypes.find(t => t.name === actionOutputTypeName);

  const setRelTypes = (relName, refSchema, refTable, fieldMappings) => {
    const types = [...allTypes];

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
    <ActionContainer
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
    </ActionContainer>
  );
};

export default Relationships;
