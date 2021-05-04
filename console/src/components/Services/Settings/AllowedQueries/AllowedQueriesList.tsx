import React, { useState } from 'react';
import AceEditor from 'react-ace';

import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import styles from './AllowedQueries.scss';
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

      const collapsedLabel = () => (
        <div>
          <b>{queryName} </b>
          <i>- {collectionName}</i>
        </div>
      );

      const expandedLabel = collapsedLabel;

      const queryEditorExpanded = () => {
        const modifiedQuery = modifiedQueries[queryName] || { ...query };

        const handleNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
          const newModifiedQueries = { ...modifiedQueries };
          newModifiedQueries[queryName].name = e.target.value;
          setModifiedQueries(newModifiedQueries);
        };

        const handleQueryChange = (val: string) => {
          const newModifiedQueries = { ...modifiedQueries };
          newModifiedQueries[queryName].query = val;
          setModifiedQueries(newModifiedQueries);
        };

        return (
          <div>
            <div>
              <div className={styles.add_mar_bottom_mid}>
                <b>Operation name:</b>
              </div>
              <input
                type="text"
                className={`form-control input-sm ${styles.inline_block}`}
                value={modifiedQuery.name}
                placeholder="operation_name"
                onChange={handleNameChange}
              />
            </div>
            <div className={styles.add_mar_top}>
              <div className={styles.add_mar_bottom_mid}>
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
        newModifiedQueries[queryName] = { ...query };
        setModifiedQueries(newModifiedQueries);
      };

      const editorCollapseCallback = () => {
        const newModifiedQueries = { ...modifiedQueries };
        delete newModifiedQueries[queryName];
        setModifiedQueries(newModifiedQueries);
      };

      const onSubmit = () => {
        dispatch(
          updateAllowedQuery(
            queryName,
            modifiedQueries[queryName],
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
        <div key={queryName}>
          <ExpandableEditor
            editorExpanded={queryEditorExpanded}
            property={`query-${i}`}
            service="modify-allowed-operation"
            saveFunc={onSubmit}
            removeFunc={onDelete}
            collapsedClass={styles.display_flex}
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
      <h4 className={styles.subheading_text}>
        Allow List
        <span className={styles.add_mar_left}>
          <Button
            size="xs"
            onClick={handleDeleteAll}
            disabled={allowedQueries.length === 0}
          >
            Delete all
          </Button>
        </span>
      </h4>

      <div className={styles.subsection}>{getQueryList()}</div>
    </div>
  );
};

export default AllowedQueriesList;
