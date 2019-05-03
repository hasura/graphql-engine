import React from 'react';
import AceEditor from 'react-ace';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';

class WhitelistQueriesList extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      modifiedQueries: {},
    };
  }

  render() {
    const { whitelistQueries } = this.props;
    const { modifiedQueries } = this.state;

    const styles = require('../Metadata.scss');

    const getQueryList = () => {
      if (whitelistQueries.length === 0) {
        return <b>No queries whitelisted yet</b>;
      }

      return whitelistQueries.map((query, i) => {
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
                  data-test="whitelist_query_editor"
                  mode="graphql"
                  theme="github"
                  name="whitelist_query_editor"
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
          // dispatch();
        };

        const onDelete = () => {
          // dispatch();
        };

        return (
          <div key={i}>
            <ExpandableEditor
              editorExpanded={queryEditorExpanded}
              property={`query-${i}`}
              service="modify-whitelist-query"
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

    return (
      <div>
        <h4 className={styles.subheading_text}>Whitelisted Queries</h4>
        <div className={styles.subsection}>{getQueryList()}</div>
      </div>
    );
  }
}

export default WhitelistQueriesList;
