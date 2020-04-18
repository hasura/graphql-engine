import React from 'react';
import AceEditor from 'react-ace';

import { addAllowedQueries } from '../Actions';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import { readFile, parseQueryString } from './utils';
import { showErrorNotification } from '../../Common/Notification';
import { ToolTip, Heading, Text, Box } from '../../../UIKit/atoms';
import styles from './AllowedQueries.scss';

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

    // eslint-disable-next-line @typescript-eslint/no-empty-function
    const handleFileUploadCollapse = () => {};

    function handleFileUploadSubmit(toggle) {
      const addFileQueries = content => {
        try {
          const fileQueries = parseQueryString(content);
          dispatch(addAllowedQueries(fileQueries, isEmptyList, toggle));
        } catch (error) {
          dispatch(
            showErrorNotification('Uploading queries failed', error.message)
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
            <Text fontWeight="bold" mb="sm">
              Query name:
            </Text>

            <input
              type="text"
              className={'form-control input-sm ' + styles.inline_block}
              placeholder={'query_name'}
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
            <Text fontWeight="bold" mb="sm">
              Query:
            </Text>
            <AceEditor
              data-test="allowed_query_add"
              mode="graphql"
              theme="github"
              name="allowed_query_add"
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
          <Box mt="20px">{getQueryInput()}</Box>
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
          <Text fontWeight="bold" mb="sm">
            Graphql File:
            <ToolTip message={'.graphql file with queries'} ml="sm" />
          </Text>
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
        <Heading type="subHeading">Add new queries to allow-list</Heading>
        <div className={styles.subsection}>
          <div>
            <ExpandableEditor
              expandButtonText="Add query manually"
              editorExpanded={getManualQueryInput}
              collapseCallback={handleManualCollapse}
              property="add-allowed-query"
              service="add-allowed-query"
              saveButtonText="Add"
              saveFunc={handleManualSubmit}
            />
          </div>
          <Box my="20px">OR</Box>
          <ExpandableEditor
            expandButtonText="Upload graphql file"
            editorExpanded={getFileUploadInput}
            collapseCallback={handleFileUploadCollapse}
            property="upload-allowed-queries"
            service="upload-allowed-queries"
            saveButtonText="Upload"
            saveFunc={handleFileUploadSubmit}
          />
        </div>
      </div>
    );
  }
}

export default AddAllowedQuery;
