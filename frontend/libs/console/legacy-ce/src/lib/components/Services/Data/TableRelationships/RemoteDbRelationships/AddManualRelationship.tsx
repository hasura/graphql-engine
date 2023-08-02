import { useReducer } from 'react';
import { NormalizedTable } from '../../../../../dataSources/types';
import { useInvalidateMetadata } from '../../../../../features/hasura-metadata-api';
import { RemoteDBRelationship } from '../../../../../metadata/types';
import { Dispatch } from '../../../../../types';
import ExpandableEditor from '../../../../Common/Layout/ExpandableEditor/Editor';
import { ordinalColSort } from '../../utils';
import { addDbToDbRelationship, dropDbToDbRelationship } from '../Actions';
import ManualRelationshipSelector from './ManualRelationshipSelector';
import { RemoteRelCollapsedLabel } from './RemoteRelCollapsedLabel';
import {
  dbToDbRelDefaultState,
  dbToDbRelReducer,
  relResetState,
} from './state';
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
  const invalidate = useInvalidateMetadata();

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
            invalidate({
              componentName: 'AddManualRelationship',
              reasons: ['Dropped db-to-db relationship'],
            });
          })
        );
      }
    : null;

  const saveFk = (toggleEditor: unknown) => {
    reduxDispatch(
      addDbToDbRelationship(state, tableSchema, toggleEditor, isNew, () => {
        invalidate({
          componentName: 'AddManualRelationship',
          reasons: ['Added db-to-db relationship'],
        });
      })
    );
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
