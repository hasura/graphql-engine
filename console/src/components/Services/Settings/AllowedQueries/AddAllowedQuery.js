import React from 'react';
import AceEditor from 'react-ace';
import styles from './AllowedQueries.scss';

import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import Tooltip from '../../../Common/Tooltip/Tooltip';

import { readFile, parseQueryString } from './utils';
import { showErrorNotification } from '../../Common/Notification';
import { addAllowedQueries } from '../../../../metadata/actions';

class AddAllowedQuery extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      manualQuery: {},
      graphqlFile: null,
    };
  }

  render() {
    const { dispatch, isEmptyList } = this.props;
    const { manualQuery, graphqlFile } = this.state;

    const handleManualCollapse = () => {
      this.setState({ manualQuery: {} });
    };

    const handleManualSubmit = toggle => {
      dispatch(addAllowedQueries([manualQuery], isEmptyList, toggle));
    };

    const handleFileUploadCollapse = () => {};

    function handleFileUploadSubmit(toggle) {
      const addFileQueries = content => {
        try {
          const fileQueries = parseQueryString(content);
          dispatch(addAllowedQueries(fileQueries, isEmptyList, toggle));
        } catch (error) {
          dispatch(
            showErrorNotification('Uploading operations failed', error.message)
          );
        }
      };

      readFile(graphqlFile, addFileQueries);
    }

    const getManualQueryInput = () => {
      const getNameInput = () => {
        const handleNameChange = e => {
          this.setState({
            manualQuery: {
              ...manualQuery,
              name: e.target.value,
            },
          });
        };

        return (
          <div>
            <div className={styles.add_mar_bottom_mid}>
              <b>Operation name:</b>
            </div>
            <input
              type="text"
              className={'form-control input-sm ' + styles.inline_block}
              placeholder={'operation_name'}
              value={manualQuery.name}
              onChange={handleNameChange}
            />
          </div>
        );
      };

      const getQueryInput = () => {
        const handleQueryChange = val => {
          this.setState({
            manualQuery: {
              ...manualQuery,
              query: val,
            },
          });
        };

        return (
          <div>
            <div className={styles.add_mar_bottom_mid}>
              <b>Operation:</b>
            </div>
            <AceEditor
              data-test="allowed_operation_add"
              mode="graphql"
              theme="github"
              name="allowed_operation_add"
              value={manualQuery.query}
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

    const getFileUploadInput = () => {
      const handleFileUpload = e => {
        const files = e.target.files;
        this.setState({ graphqlFile: files[0] });
      };

      return (
        <div>
          <div className={styles.add_mar_bottom_mid}>
            <b>Graphql File:</b>
            <Tooltip message={'.graphql file with operations'} />
          </div>
          <input
            type="file"
            className={'form-control input-sm ' + styles.inline_block}
            onChange={handleFileUpload}
          />
        </div>
      );
    };

    return (
      <div>
        <h4 className={styles.subheading_text}>
          Add new operations to allow-list
        </h4>
        <div className={styles.subsection}>
          <div>
            <ExpandableEditor
              expandButtonText="Add operation manually"
              editorExpanded={getManualQueryInput}
              collapseCallback={handleManualCollapse}
              property="add-allowed-operation"
              service="add-allowed-operation"
              saveButtonText="Add"
              saveFunc={handleManualSubmit}
            />
          </div>
          <div className={styles.add_mar_top}>OR</div>
          <div className={styles.add_mar_top}>
            <ExpandableEditor
              expandButtonText="Upload graphql file"
              editorExpanded={getFileUploadInput}
              collapseCallback={handleFileUploadCollapse}
              property="upload-allowed-operations"
              service="upload-allowed-operations"
              saveButtonText="Upload"
              saveFunc={handleFileUploadSubmit}
            />
          </div>
        </div>
      </div>
    );
  }
}

export default AddAllowedQuery;
