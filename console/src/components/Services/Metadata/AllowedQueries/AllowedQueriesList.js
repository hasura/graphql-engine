import React from 'react';
import AceEditor from 'react-ace';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';

import styles from './AllowedQueries.scss';

import {
  updateAllowedQuery,
  deleteAllowedQuery,
  deleteAllowList,
} from '../Actions';
import Button from '../../../Common/Button/Button';

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
        const collapsedLabel = () => (
          <div>
            <b>{query.name}</b>
          </div>
        );

        const expandedLabel = collapsedLabel;

        const queryEditorExpanded = () => {
          const modifiedQuery = modifiedQueries[query.name] || { ...query };

          const handleNameChange = e => {
            const newModifiedQueries = { ...modifiedQueries };
            newModifiedQueries[query.name].name = e.target.value;

            this.setState({ modifiedQueries: newModifiedQueries });
          };

          const handleQueryChange = val => {
            const newModifiedQueries = { ...modifiedQueries };
            newModifiedQueries[query.name].query = val;

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
          newModifiedQueries[query.name] = { ...query };

          this.setState({ modifiedQueries: newModifiedQueries });
        };

        const editorCollapseCallback = () => {
          const newModifiedQueries = { ...modifiedQueries };
          delete newModifiedQueries[query.name];

          this.setState({ modifiedQueries: newModifiedQueries });
        };

        const onSubmit = () => {
          dispatch(updateAllowedQuery(query.name, modifiedQueries[query.name]));
        };

        const onDelete = () => {
          const isOk = window.confirm('Are you sure?');

          if (isOk) {
            const isLastQuery = allowedQueries.length === 1;

            dispatch(deleteAllowedQuery(query.name, isLastQuery));
          }
        };

        return (
          <div key={query.name}>
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
        const isOk = window.confirm('Are you sure?');

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
        <h4 className={styles.subheading_text}>
          Allowed Queries
          <span className={styles.add_mar_left}>{getDeleteAllBtn()}</span>
        </h4>

        <div className={styles.subsection}>{getQueryList()}</div>
      </div>
    );
  }
}

export default AllowedQueriesList;
