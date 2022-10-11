import React, { useReducer } from 'react';
import { useQueryClient } from 'react-query';
import { RemoteDBRelationship } from '@/metadata/types';
import { NormalizedTable } from '@/dataSources/types';
import { Dispatch } from '@/types';
import { ordinalColSort } from '../../utils';
import { addDbToDbRelationship, dropDbToDbRelationship } from '../Actions';
import {
  relResetState,
  dbToDbRelDefaultState,
  dbToDbRelReducer,
} from './state';
import ExpandableEditor from '../../../../Common/Layout/ExpandableEditor/Editor';
import ManualRelationshipSelector from './ManualRelationshipSelector';
import { RemoteRelCollapsedLabel } from './RemoteRelCollapsedLabel';
import { parseDbToDbRemoteRel } from './utils';

const AddManualRelationship = ({
  tableSchema,
  reduxDispatch,
  currentSource,
  relationship,
}: {
  tableSchema: NormalizedTable;
  reduxDispatch: Dispatch;
  currentSource: string;
  relationship: RemoteDBRelationship | null;
}) => {
  const [state, dispatch] = useReducer(
    dbToDbRelReducer,
    relationship ? parseDbToDbRemoteRel(relationship) : dbToDbRelDefaultState
  );
  const columns = tableSchema.columns.sort(ordinalColSort);
  const isNew = relationship === null;
  const queryClient = useQueryClient();

  // columns in the right order with their indices
  const orderedColumns = columns.map((c, i) => ({
    name: c.column_name,
    index: i,
  }));

  const resetManualRel = () => {
    if (!relationship) {
      dispatch(relResetState());
    }
  };

  const removeFunc = relationship
    ? (toggleEditor: () => void) => {
        reduxDispatch(
          dropDbToDbRelationship(state, tableSchema, () => {
            toggleEditor();
            queryClient.refetchQueries(['metadata'], { active: true });
          })
        );
      }
    : null;

  const saveFk = (toggleEditor: unknown) => {
    reduxDispatch(
      addDbToDbRelationship(state, tableSchema, toggleEditor, isNew, () => {
        queryClient.refetchQueries(['metadata'], { active: true });
      })
    );
    // queryClient.refetchQueries(['metadata'], { active: true });
  };

  const expandedContent = () => (
    <ManualRelationshipSelector
      orderedColumns={orderedColumns}
      state={state}
      dispatch={dispatch}
      isNew={isNew}
      currentSource={currentSource}
    />
  );

  const collapsedLabel = () => (
    <RemoteRelCollapsedLabel
      currentSource={currentSource}
      currentSchema={tableSchema?.table_schema}
      currentTable={tableSchema?.table_name}
      relationship={relationship ?? undefined}
    />
  );

  const expandedLabel = () => {
    return <b>Configure relationship</b>;
  };

  return (
    <div key="add_manual_relationship">
      <ExpandableEditor
        editorExpanded={expandedContent}
        expandedLabel={expandedLabel}
        expandButtonText={
          relationship ? 'Edit' : 'Add a remote database relationship'
        }
        collapsedLabel={collapsedLabel}
        collapseCallback={resetManualRel}
        saveFunc={saveFk}
        removeFunc={removeFunc}
        service="create"
        property="manual-remote-db-rel"
      />
    </div>
  );
};

export default AddManualRelationship;
