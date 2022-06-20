import React, { useState } from 'react';
import AceEditor from 'react-ace';

import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import Button from '../../../Common/Button/Button';

import { getConfirmation } from '../../../Common/utils/jsUtils';
import {
  updateAllowedQuery,
  deleteAllowedQuery,
  deleteAllowList,
} from '../../../../metadata/actions';
import { AllowedQueriesCollection } from '../../../../metadata/reducer';
import { Dispatch } from '../../../../types';
import { getCollectionNames, checkLastQuery } from './utils';
import Tooltip from '../../../Common/Tooltip/Tooltip';
import { inputStyles } from '../constants';

type AllowedQueriesListProps = {
  dispatch: Dispatch;
  allowedQueries: AllowedQueriesCollection[];
};

type ModifiedQuery = Record<string, AllowedQueriesCollection>;

const AllowedQueriesList: React.FC<AllowedQueriesListProps> = props => {
  const [modifiedQueries, setModifiedQueries] = useState<ModifiedQuery>({});
  const { allowedQueries, dispatch } = props;

  const getQueryList = () => {
    if (allowedQueries.length === 0) {
      return <div>No operations in allow-list yet</div>;
    }

    return allowedQueries.map((query, i) => {
      const queryName = query.name;
      const collectionName = query.collection;
      const queryId = `${queryName}_${collectionName}_${i}`;

      const collapsedLabel = () => (
        <div>
          <b>{queryName} </b>
          <i>- {collectionName}</i>
        </div>
      );

      const expandedLabel = collapsedLabel;

      const queryEditorExpanded = () => {
        const modifiedQuery = modifiedQueries[queryId] || { ...query };

        const handleNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
          const newModifiedQueries = { ...modifiedQueries };
          newModifiedQueries[queryId].name = e.target.value;
          setModifiedQueries(newModifiedQueries);
        };

        const handleQueryChange = (val: string) => {
          const newModifiedQueries = { ...modifiedQueries };
          newModifiedQueries[queryId].query = val;
          setModifiedQueries(newModifiedQueries);
        };

        return (
          <div>
            <div>
              <div className="mb-md">
                <b>Query name:</b>
                <Tooltip
                  message="This is an identifier for the query in the collection. 
                This should be unique in the collection and can be different from the operation name of the query."
                />
              </div>
              <input
                type="text"
                className={`${inputStyles} inline-block`}
                value={modifiedQuery.name}
                placeholder="operation_name"
                onChange={handleNameChange}
              />
            </div>
            <div className="mt-mb">
              <div className="mb-md">
                <b>Operation:</b>
              </div>
              <AceEditor
                data-test="allowed_operation_editor"
                mode="graphql"
                theme="github"
                name="allowed_operation_editor"
                value={modifiedQuery.query}
                minLines={8}
                maxLines={100}
                width="100%"
                showPrintMargin={false}
                onChange={handleQueryChange}
              />
            </div>
          </div>
        );
      };

      const editorExpandCallback = () => {
        const newModifiedQueries = { ...modifiedQueries };
        newModifiedQueries[queryId] = { ...query };
        setModifiedQueries(newModifiedQueries);
      };

      const editorCollapseCallback = () => {
        const newModifiedQueries = { ...modifiedQueries };
        delete newModifiedQueries[queryId];
        setModifiedQueries(newModifiedQueries);
      };

      const onSubmit = () => {
        dispatch(
          updateAllowedQuery(
            queryName,
            modifiedQueries[queryId],
            collectionName
          )
        );
      };

      const onDelete = () => {
        const confirmMessage = `This will delete the operation "${queryName}" from the allow-list`;
        const isOk = getConfirmation(confirmMessage);
        if (isOk) {
          const isLastQuery = checkLastQuery(collectionName, allowedQueries);
          dispatch(deleteAllowedQuery(queryName, isLastQuery, collectionName));
        }
      };

      return (
        <div key={queryId}>
          <ExpandableEditor
            editorExpanded={queryEditorExpanded}
            property={`query-${i}`}
            service="modify-allowed-operation"
            saveFunc={onSubmit}
            removeFunc={onDelete}
            collapsedClass="flex"
            expandedLabel={expandedLabel}
            collapsedLabel={collapsedLabel}
            expandCallback={editorExpandCallback}
            collapseCallback={editorCollapseCallback}
          />
        </div>
      );
    });
  };

  const handleDeleteAll = () => {
    const confirmMessage =
      'This will delete all operations from the allow-list';
    const isOk = getConfirmation(confirmMessage, true);
    const collectionNames = getCollectionNames(allowedQueries);
    if (isOk) {
      dispatch(deleteAllowList(collectionNames));
    }
  };

  return (
    <div>
      <h4 className="text-lg font-bold pb-md">
        Allow List
        <span className="pl-md">
          <Button
            size="xs"
            onClick={handleDeleteAll}
            disabled={allowedQueries.length === 0}
          >
            Delete all
          </Button>
        </span>
      </h4>

      <div className="pl-md">{getQueryList()}</div>
    </div>
  );
};

export default AllowedQueriesList;
