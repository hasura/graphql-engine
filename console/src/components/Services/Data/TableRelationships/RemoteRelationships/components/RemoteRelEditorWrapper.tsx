import React from 'react';
import ExpandableEditor from '../../../../../Common/Layout/ExpandableEditor/Editor';
import { RemoteRelationshipServer } from '../utils';
import RemoteRelEditor from './RemoteRelEditor';
import RemoteRelCollapsedLabel from './EditorCollapsed';
import { useRemoteRelationship } from '../state';
import { saveRemoteRelationship, dropRemoteRelationship } from '../../Actions';
import { Dispatch } from '../../../../../../types';
import { Table } from '../../../../../../dataSources/types';
import { PGFunction } from '../../../../../../dataSources/services/postgresql/types';

type Props = {
  relationship?: RemoteRelationshipServer;
  table: Table;
  isLast: boolean;
  remoteSchemas: string[];
  reduxDispatch: Dispatch;
  allFunctions: PGFunction[];
};

const EditorWrapper: React.FC<Props> = ({
  relationship,
  table,
  isLast,
  remoteSchemas,
  reduxDispatch,
  allFunctions,
}) => {
  const { state, dispatch, reset } = useRemoteRelationship(table, relationship);

  const expandedContent = () => (
    <RemoteRelEditor
      table={table}
      remoteSchemas={remoteSchemas}
      isLast={isLast}
      state={state}
      allFunctions={allFunctions}
      dispatch={dispatch}
      reduxDispatch={reduxDispatch}
    />
  );

  const collapsedLabel = () => (
    <RemoteRelCollapsedLabel relationship={relationship} />
  );

  const saveFunc = (toggle: VoidFunction) => {
    reduxDispatch(saveRemoteRelationship(state, relationship, toggle));
  };

  const removeFunc = !isLast
    ? () => {
        reduxDispatch(dropRemoteRelationship(state, relationship));
      }
    : null;

  const expandButtonText = isLast ? 'Add a remote schema relationship' : 'Edit';
  const collapseButtonText = isLast ? 'Cancel' : 'Close';
  return (
    <ExpandableEditor
      editorExpanded={expandedContent}
      property={`remote-relationship-${isLast ? 'add' : 'edit'}`}
      service="table-relationship"
      saveFunc={saveFunc}
      expandButtonText={expandButtonText}
      collapseButtonText={collapseButtonText}
      collapsedLabel={collapsedLabel}
      removeFunc={removeFunc}
      collapseCallback={reset}
    />
  );
};

export default EditorWrapper;
