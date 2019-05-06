import React from 'react';
import AceEditor from 'react-ace';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';

const WhitelistQueriesList = props => {
  const { whitelistQueries } = props;

  const styles = require('../Metadata.scss');

  const getQueryList = () => {
    if (whitelistQueries.length === 0) {
      return <b>No queries whitelisted yet</b>;
    }

    return whitelistQueries.map((query, i) => {
      const queryEditorExpanded = () => (
        <div>
          <div>
            <div className={styles.add_mar_bottom_mid}>
              <b>Query name:</b>
            </div>
            <input
              type="text"
              className={'form-control input-sm ' + styles.inline_block}
              value={query.name}
              placeholder={'query_name'}
              onChange={() => {}}
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
              value={query.query}
              minLines={8}
              maxLines={100}
              width="100%"
              showPrintMargin={false}
              onChange={() => {}}
            />
          </div>
        </div>
      );

      const collapsedLabel = () => (
        <div>
          <b>{query.name}</b>
        </div>
      );

      const expandedLabel = collapsedLabel;

      const onSubmit = () => {};
      const onDelete = () => {};
      const editorExpandCallback = () => {};
      const editorCollapseCallback = () => {};

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
};

export default WhitelistQueriesList;
