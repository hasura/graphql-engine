import React from 'react';
import PropTypes from 'prop-types';
import AceEditor from 'react-ace';
import { Tab, Tabs, TabList, TabPanel } from 'react-tabs';

import CopyToClipboard from 'react-copy-to-clipboard';

import generateSuggestionBox from './generateSuggestionBox';

import suggestionFunctions from './suggestionFunctions';

class ApiResponse extends React.Component {
  constructor() {
    super();
    this.state = {
      helpCopied: false,
      tabIndex: 0,
    };
  }

  render() {
    const {
      categoryType,
      showHelpBulb,
      enableResponseSection,
      response,
      url,
    } = this.props;

    const styles = require('../ApiExplorer.scss');

    const suggestionFunction = suggestionFunctions[categoryType];
    const isResponseError =
      'statusCode' in response ? response.statusCode !== 200 : false;

    const responseHtml =
      isResponseError && suggestionFunction
        ? generateSuggestionBox(response, suggestionFunction)
        : '';

    const imgHTMLTag = `<img src='${url}' />`;

    let formattedResponse = JSON.stringify(response.response, null, 4);
    let responseMode = 'json';
    let showGutter = true;
    if (response.statusCode === 500) {
      responseMode = 'text';
      formattedResponse = 'Could not get any response';
      formattedResponse += '\n\nThere was an error connecting to ' + url;
      formattedResponse +=
        '\n\nCheck if the URL is valid or the server is timing out';
      showGutter = false;
    }

    const getHeaders = responseHeader => {
      const currHeaders = [];

      if (responseHeader.responseHeaders) {
        responseHeader.responseHeaders.forEach((value, name) => {
          currHeaders.push(
            <tr key={name}>
              <td
                className={
                  styles.headerPadd +
                  ' ' +
                  styles.wd48 +
                  ' ' +
                  styles.border_right
                }
              >
                {name}{' '}
              </td>
              <td className={styles.headerPadd + ' ' + styles.wd48}>{value}</td>
            </tr>
          );
        });
      }

      return currHeaders.length > 0 ? currHeaders : '';
    };

    return (
      <div className={styles.apiResponseWrapper}>
        <div
          id="apiResponseBlock"
          className={styles.fixed_header_internal_link}
        />
        <div className={styles.apiResponseheaderWrapper + ' ' + styles.wd100}>
          <div
            className={
              styles.apiResponseheader + ' col-xs-6 ' + styles.padd_remove
            }
          >
            Response
          </div>
          {enableResponseSection ? (
            <div className={'col-xs-6 ' + styles.padd_remove}>
              <div className={styles.statusDetails}>
                Time:{' '}
                <span className={styles.statusView}>
                  {response.timeElapsed} ms
                </span>
              </div>
              <div className={styles.statusDetails}>
                Status:{' '}
                <span className={styles.statusView}>{response.statusCode}</span>
              </div>
            </div>
          ) : (
            ''
          )}
        </div>

        {responseHtml}

        {showHelpBulb ? (
          <div className={styles.helpTextWrapper}>
            <i className="fa fa-lightbulb-o" aria-hidden="true" />
            Embed in your HTML as follows
            <div className="input-group">
              <pre>{imgHTMLTag}</pre>
              <span className="input-group-btn">
                <CopyToClipboard
                  text={imgHTMLTag}
                  onCopy={() => {
                    this.setState({ helpCopied: true });
                    const timer = setInterval(() => {
                      this.setState({ helpCopied: false });
                      clearInterval(timer);
                    }, 3000);
                  }}
                >
                  <button className={styles.copyBtn + ' btn'} type="button">
                    {this.state.helpCopied ? 'Copied' : 'Copy'}
                  </button>
                </CopyToClipboard>
              </span>
            </div>
          </div>
        ) : (
          ''
        )}
        {enableResponseSection ? (
          <Tabs
            className={styles.apiResponseTab}
            selectedIndex={this.state.tabIndex}
            onSelect={tabIndex => this.setState({ tabIndex })}
          >
            <TabList className={styles.apiResponseTabUl}>
              <Tab
                className={
                  this.state.tabIndex === 0
                    ? ' ' +
                      styles.activeApiResponseTab +
                      ' ' +
                      styles.apiResponseTabList
                    : styles.apiResponseTabList
                }
              >
                Body
              </Tab>
              <Tab
                className={
                  this.state.tabIndex === 1
                    ? ' ' +
                      styles.activeApiResponseTab +
                      ' ' +
                      styles.apiResponseTabList
                    : styles.apiResponseTabList
                }
              >
                Headers
              </Tab>
            </TabList>
            <TabPanel className={styles.apiResponseTabPanel}>
              <div className={styles.AceEditorWrapper}>
                {response.isImage ? (
                  <img
                    src={response.response}
                    style={{ width: '100%', height: '100%' }}
                  />
                ) : (
                  <AceEditor
                    readOnly
                    showPrintMargin={false}
                    mode={responseMode}
                    showGutter={showGutter}
                    theme="github"
                    name="api-explorer-request"
                    value={formattedResponse}
                    minLines={10}
                    maxLines={50}
                    width="100%"
                  />
                )}
              </div>
            </TabPanel>
            <TabPanel className={styles.apiResponseTabPanel}>
              <div className={styles.responseHeader + ' hide'}>
                Header
                <span className={styles.viewDetails + ' hide'}>
                  View Details
                </span>
              </div>
              <div className={styles.responseTable}>
                <table className={'table ' + styles.tableBorder}>
                  <tbody>{getHeaders(response)}</tbody>
                </table>
              </div>
            </TabPanel>
          </Tabs>
        ) : (
          <div className={styles.noResponseWrapper}>
            <div className={styles.noResponseContainer}>
              <div className={styles.noResponseHeader}>
                Hit the Send button to get a response
              </div>
              <div className={styles.barWrapper}>
                <div className={styles.bigBar} />
                <div className={styles.mediumBar} />
                <div className={styles.smallBar} />
              </div>
            </div>
          </div>
        )}
      </div>
    );
  }
}

ApiResponse.propTypes = {
  enableResponseSection: PropTypes.bool.isRequired,
  response: PropTypes.object.isRequired,
  showHelpBulb: PropTypes.bool,
  url: PropTypes.string,
  categoryType: PropTypes.string,
};
export default ApiResponse;
