import React from 'react';
import AceEditor from 'react-ace';
import styles from './AllowedQueries.scss';

import { addAllowedQuery } from '../Actions';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';

class AddAllowedQuery extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      newQuery: {},
    };
  }

  render() {
    const { dispatch, isFirstQuery } = this.props;
    const { newQuery } = this.state;

    const handleSubmit = toggle => {
      dispatch(addAllowedQuery(newQuery, isFirstQuery, toggle));
    };

    const handleCollapse = () => {
      this.setState({ newQuery: {} });
    };

    const getManualQueryInput = () => {
      const getNameInput = () => {
        const handleNameChange = e => {
          this.setState({
            newQuery: {
              ...newQuery,
              name: e.target.value,
            },
          });
        };

        return (
          <div>
            <div className={styles.add_mar_bottom_mid}>
              <b>Query name:</b>
            </div>
            <input
              type="text"
              className={'form-control input-sm ' + styles.inline_block}
              placeholder={'query_name'}
              value={newQuery.name}
              onChange={handleNameChange}
            />
          </div>
        );
      };

      const getQueryInput = () => {
        const handleQueryChange = val => {
          this.setState({
            newQuery: {
              ...newQuery,
              query: val,
            },
          });
        };

        return (
          <div>
            <div className={styles.add_mar_bottom_mid}>
              <b>Query:</b>
            </div>
            <AceEditor
              data-test="allowed_query_add"
              mode="graphql"
              theme="github"
              name="allowed_query_add"
              value={newQuery.query}
              minLines={8}
              maxLines={100}
              width="100%"
              showPrintMargin={false}
              onChange={handleQueryChange}
            />
          </div>
        );
      };

      return (
        <div>
          <div>{getNameInput()}</div>
          <div className={styles.add_mar_top}>{getQueryInput()}</div>
        </div>
      );
    };

    return (
      <div>
        <h4 className={styles.subheading_text}>
          Add a new query to allow-list
        </h4>
        <div className={styles.subsection}>
          <ExpandableEditor
            expandButtonText="Add query"
            editorExpanded={getManualQueryInput}
            collapseCallback={handleCollapse}
            property="add-allowed-query"
            service="add-allowed-query"
            saveFunc={handleSubmit}
          />
        </div>
      </div>
    );
  }
}

export default AddAllowedQuery;
