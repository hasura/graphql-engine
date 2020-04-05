import React from 'react';
import AceEditor from 'react-ace';

import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import {
  updateAllowedQuery,
  deleteAllowedQuery,
  deleteAllowList,
} from '../Actions';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import Button from '../../../Common/Button/Button';
import { Heading } from '../../../UIKit/atoms';
import styles from './AllowedQueries.scss';

class AllowedQueriesList extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      modifiedQueries: {},
    };
  }

  render() {
    const { allowedQueries, dispatch } = this.props;
    const { modifiedQueries } = this.state;

    const getQueryList = () => {
      if (allowedQueries.length === 0) {
        return <div>No queries in allow-list yet</div>;
      }

      return allowedQueries.map((query, i) => {
        const queryName = query.name;

        const collapsedLabel = () => (
          <div>
            <b>{queryName}</b>
          </div>
        );

        const expandedLabel = collapsedLabel;

        const queryEditorExpanded = () => {
          const modifiedQuery = modifiedQueries[queryName] || { ...query };

          const handleNameChange = e => {
            const newModifiedQueries = { ...modifiedQueries };
            newModifiedQueries[queryName].name = e.target.value;

            this.setState({ modifiedQueries: newModifiedQueries });
          };

          const handleQueryChange = val => {
            const newModifiedQueries = { ...modifiedQueries };
            newModifiedQueries[queryName].query = val;

            this.setState({ modifiedQueries: newModifiedQueries });
          };

          return (
            <div>
              <div>
                <div className={styles.add_mar_bottom_mid}>
                  <b>Query name:</b>
                </div>
                <input
                  type="text"
                  className={'form-control input-sm ' + styles.inline_block}
                  value={modifiedQuery.name}
                  placeholder={'query_name'}
                  onChange={handleNameChange}
                />
              </div>
              <div className={styles.add_mar_top}>
                <div className={styles.add_mar_bottom_mid}>
                  <b>Query:</b>
                </div>
                <AceEditor
                  data-test="allowed_query_editor"
                  mode="graphql"
                  theme="github"
                  name="allowed_query_editor"
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

          this.setState({ modifiedQueries: newModifiedQueries });
        };

        const editorCollapseCallback = () => {
          const newModifiedQueries = { ...modifiedQueries };
          delete newModifiedQueries[queryName];

          this.setState({ modifiedQueries: newModifiedQueries });
        };

        const onSubmit = () => {
          dispatch(updateAllowedQuery(queryName, modifiedQueries[queryName]));
        };

        const onDelete = () => {
          const confirmMessage = `This will delete the query "${queryName}" from the allow-list`;
          const isOk = getConfirmation(confirmMessage);
          if (isOk) {
            const isLastQuery = allowedQueries.length === 1;

            dispatch(deleteAllowedQuery(queryName, isLastQuery));
          }
        };

        return (
          <div key={queryName}>
            <ExpandableEditor
              editorExpanded={queryEditorExpanded}
              property={`query-${i}`}
              service="modify-allowed-query"
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

    const getDeleteAllBtn = () => {
      const handleDeleteAll = () => {
        const confirmMessage =
          'This will delete all queries from the allow-list';
        const isOk = getConfirmation(confirmMessage, true);
        if (isOk) {
          dispatch(deleteAllowList());
        }
      };

      return (
        <Button
          size="xs"
          onClick={handleDeleteAll}
          disabled={allowedQueries.length === 0}
        >
          Delete all
        </Button>
      );
    };

    return (
      <div>
        <Heading type="subHeading">
          Allowed Queries
          <span className={styles.add_mar_left}>{getDeleteAllBtn()}</span>
        </Heading>

        <div className={styles.subsection}>{getQueryList()}</div>
      </div>
    );
  }
}

export default AllowedQueriesList;
