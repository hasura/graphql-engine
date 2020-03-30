import React from 'react';

import ActionContainer from '../Containers/ActionContainer';
import { findAction } from '../utils';
import { unwrapType } from '../../../../shared/utils/wrappingTypeUtils';
import AllRelationships from './Relationships';
import { Heading } from '../../../UIKit/atoms';
import styles from '../Actions.scss';

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

  return (
    <ActionContainer
      params={params}
      allActions={allActions}
      tabName="relationships"
      dispatch={dispatch}
    >
      <div className={`${styles.padd_left_remove} container-fluid`}>
        <div className={`${styles.padd_left_remove} col-xs-10 col-md-10`}>
          <Heading as="h4" fontSize="15px" pb="20px" mt="0px" mb="0px">
            Relationships
          </Heading>
          <AllRelationships
            objectType={actionOutputType}
            allTables={allTables}
            schemaList={schemaList}
            dispatch={dispatch}
            currentAction={action}
          />
          <hr />
        </div>
      </div>
      <div className={`${styles.fixed} hidden`}>{alert}</div>
    </ActionContainer>
  );
};

export default Relationships;
