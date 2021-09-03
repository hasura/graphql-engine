import React from 'react';
import styles from '../Actions.scss';
import ActionContainer from '../Containers/ActionContainer';
import { findAction } from '../utils';
import { unwrapType } from '../../../../shared/utils/wrappingTypeUtils';
import AllRelationships from './Relationships';

const Relationships = ({
  params,
  allActions,
  dispatch,
  allTables,
  allTypes,
  schemaList,
  readOnlyMode,
  dataSources,
}) => {
  const { actionName } = params;

  const action = findAction(allActions, actionName);

  const actionOutputTypeName = unwrapType(action.definition.output_type)
    .typename;

  const actionOutputType = allTypes.find(t => t.name === actionOutputTypeName);

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
          <AllRelationships
            objectType={actionOutputType}
            allTables={allTables}
            schemaList={schemaList}
            dispatch={dispatch}
            currentAction={action}
            readOnlyMode={readOnlyMode}
            dataSources={dataSources}
          />
          <hr className="my-md" />
        </div>
      </div>
      <div className={`${styles.fixed} hidden`}>{alert}</div>
    </ActionContainer>
  );
};

export default Relationships;
