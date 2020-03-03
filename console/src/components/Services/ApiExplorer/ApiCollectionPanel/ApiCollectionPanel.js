/* eslint-disable */
import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Tab, Tabs, TabList, TabPanel } from 'react-tabs';
import Globals from 'Globals';
import { getConfirmation } from '../../../Common/utils/jsUtils';

class ApiCollectionPanel extends Component {
  onClearHistoryClicked = () => {
    const isOk = getConfirmation();
    if (isOk) {
      this.props.clearHistoryCallback();
    }
  };

  onTabSelectionChanged = e => {
    this.props.tabSelectionCallback(e);
  };

  getTabs(tabs, currentTab, styles) {
    return tabs.map((tab, i) => {
      const style =
        styles.apiCollectionTabList +
        (currentTab === i ? ' ' + styles.activeApiCollectionTab : '');
      return (
        <Tab key={tab.title + i} className={style}>
          {tab.title}
        </Tab>
      );
    });
  }

  getTabPanelApiList(
    apiList,
    selectedApiKey,
    styles,
    isCategorised,
    authApiExpanded
  ) {
    return apiList.map((apiDetails, outerIndex) => {
      const rightArrowImage = require('./chevron.svg');
      if (isCategorised) {
        // form accordion
        const finalCategorisedArray = [];
        const currentContentLength = apiDetails.content.length;
        let categorisedArray = [];
        let prevTitle = '';
        // form accordion items
        const categories = apiDetails.content.map(
          (categorisedApiDetails, index) => {
            const key = categorisedApiDetails.id;
            const method = categorisedApiDetails.request.method;
            const methodStyle =
              method === 'GET'
                ? styles.apiCollectionGet
                : styles.apiCollectionPost;
            const apiDetailsStyle =
              method === 'GET'
                ? styles.apiCollectionGetDetails
                : styles.apiCollectionPostDetails;
            const wrapperStyle =
              method === 'GET'
                ? styles.apiCollectionGetWrapper
                : styles.apiCollectionPostWrapper;
            const style =
              wrapperStyle +
              (selectedApiKey === key
                ? ' ' + styles.activeApiCollectionGetWrapper
                : '') +
              ' ' +
              styles.wd100;
            const rightArrow =
              selectedApiKey === key ? (
                <img
                  className={
                    'img-responsive ' + styles.activeApiCollectionGetWrapperIcon
                  }
                  src={rightArrowImage}
                  alt={'Right arrow'}
                />
              ) : null;
            categorisedArray.push(
              <div
                onClick={() => {
                  // eslint-disable-line no-loop-func
                  this.props.apiSelectionCallback(
                    categorisedApiDetails,
                    authApiExpanded
                  );
                }}
                key={key}
                className={style}
              >
                <div className={'col-xs-3 ' + styles.padd_remove}>
                  <div className={methodStyle}>{method}</div>
                </div>
                <div className={'col-xs-9  ' + styles.padd_remove}>
                  <div className={styles.apiCollectionGetDetailsWrapper}>
                    <div
                      className={
                        apiDetailsStyle + ' col-xs-11 ' + styles.padd_remove
                      }
                    >
                      {categorisedApiDetails.details.title}
                    </div>
                    <div
                      className={
                        styles.apiRightArrowWrapper +
                        ' col-xs-1 ' +
                        styles.padd_remove
                      }
                    >
                      {rightArrow}
                    </div>
                  </div>
                </div>
              </div>
            );
            if (index + 1 === currentContentLength) {
              let circleClass = 'fa fa-chevron-down';
              if (apiDetails.title !== authApiExpanded) {
                finalCategorisedArray.push(
                  <div
                    key={index + 1 + apiDetails.title}
                    className={
                      styles.collectionButtonWrapper +
                      ' ' +
                      styles.border_bottom +
                      ' ' +
                      styles.wd100
                    }
                  >
                    <button
                      className={'btn ' + styles.collectionButtonLess}
                      onClick={() => {
                        this.props.authApiExpandCallback(apiDetails.title);
                      }}
                    >
                      <span>{apiDetails.title}</span>
                      <i className={circleClass} aria-hidden="true" />
                    </button>
                  </div>
                );
              } else {
                circleClass = 'fa fa-chevron-up';
                finalCategorisedArray.push(
                  <div
                    key={index + 1 + apiDetails.title}
                    className={
                      styles.collectionButtonWrapper +
                      ' ' +
                      styles.border_bottom +
                      ' ' +
                      styles.wd100
                    }
                  >
                    <button
                      className={'btn ' + styles.collectionButtonLess}
                      onClick={() => {
                        this.props.authApiExpandCallback('None');
                      }}
                    >
                      <span className={styles.padd_bottom}>
                        {apiDetails.title}
                      </span>
                      <i className={circleClass} aria-hidden="true" />
                    </button>
                    {categorisedArray}
                  </div>
                );
              }
              categorisedArray = [];
            }
          }
        );
        return <div>{finalCategorisedArray}</div>;
      } else {
        const key = apiDetails.id;
        const method = apiDetails.request.method;
        const methodStyle =
          method === 'GET' ? styles.apiCollectionGet : styles.apiCollectionPost;
        const apiDetailsStyle =
          method === 'GET'
            ? styles.apiCollectionGetDetails
            : styles.apiCollectionPostDetails;
        const wrapperStyle =
          method === 'GET'
            ? styles.apiCollectionGetWrapper
            : styles.apiCollectionPostWrapper;
        const style =
          wrapperStyle +
          (selectedApiKey === key
            ? ' ' + styles.activeApiCollectionGetWrapper
            : '') +
          ' ' +
          styles.wd100;
        const rightArrow =
          selectedApiKey === key ? (
            <img
              className={
                'img-responsive ' + styles.activeApiCollectionGetWrapperIcon
              }
              src={rightArrowImage}
              alt={'Right arrow'}
            />
          ) : null;
        return (
          <div
            onClick={() => {
              this.props.apiSelectionCallback(apiDetails, authApiExpanded);
            }}
            key={key}
            className={style}
          >
            <div className={'col-xs-3 ' + styles.padd_remove}>
              <div className={methodStyle}>{method}</div>
            </div>
            <div className={'col-xs-9  ' + styles.padd_remove}>
              <div className={styles.apiCollectionGetDetailsWrapper}>
                <div
                  className={
                    apiDetailsStyle + ' col-xs-11 ' + styles.padd_remove
                  }
                >
                  {apiDetails.details.title}
                </div>
                <div
                  className={
                    styles.apiRightArrowWrapper +
                    ' col-xs-1 ' +
                    styles.padd_remove
                  }
                >
                  {rightArrow}
                </div>
              </div>
            </div>
          </div>
        );
      }
    });
  }

  getTabPanelContent(
    tabContentList,
    emptyText,
    selectedApi,
    styles,
    authApiExpanded,
    index
  ) {
    if (tabContentList.length === 0) {
      return (
        <div
          key={'noTabContentList' + index}
          className={
            styles.apiCollectionTabListDetails +
            ' ' +
            styles.wd100 +
            ' ' +
            styles.apiPaddTop
          }
        >
          <div className={styles.apiCollectionTabListHead}>
            <span className={styles.serviceBaseDomain}>{emptyText}</span>
          </div>
        </div>
      );
    }
    return tabContentList.map((tabContent, index) => {
      let paddingClassname = '';
      if (index === 0) {
        paddingClassname = ' ' + styles.apiPaddTop;
      }
      if (tabContent.title === 'Clear History') {
        return (
          <div
            key={tabContent.title + index}
            className={
              styles.apiCollectionClearHistory +
              ' ' +
              styles.wd100 +
              paddingClassname
            }
          >
            <div
              onClick={this.onClearHistoryClicked}
              className={styles.apiCollectionClearHistoryButton}
            >
              <i className="fa fa-trash-o" aria-hidden="true" />
              Clear History
            </div>
          </div>
        );
      }
      const tabPanelList = this.getTabPanelApiList(
        tabContent.content,
        selectedApi,
        styles,
        tabContent.isCategorised,
        authApiExpanded
      );
      let html = tabPanelList;
      return (
        <div
          key={tabContent.title + index}
          className={
            styles.apiCollectionTabListDetails +
            ' ' +
            styles.wd100 +
            paddingClassname
          }
        >
          {tabContent.title ? (
            <div
              className={
                styles.apiCollectionTabListHead + ' ' + styles.add_ellipsis
              }
            >
              {tabContent.title}
              <span className={styles.serviceBaseDomain + ' hide'}>
                {Globals.projectDomain}
              </span>
            </div>
          ) : null}
          <div className={styles.apiCollectionGetPost + ' ' + styles.wd100}>
            {html}
          </div>
        </div>
      );
    });
  }

  getTabPanels(tabs, selectedApi, styles, authApiExpanded) {
    const selectedApiKey = selectedApi.id;
    return tabs.map((tab, index) => {
      return (
        <TabPanel key={tab.title}>
          {this.getTabPanelContent(
            tab.content,
            tab.emptyText,
            selectedApiKey,
            styles,
            authApiExpanded,
            index
          )}
        </TabPanel>
      );
    });
  }

  render() {
    const styles = require('../ApiExplorer.scss');
    const {
      selectedApi,
      currentTab,
      tabs,
      panelStyles,
      authApiExpanded,
    } = this.props;
    return (
      <div
        className={
          styles.padd_remove +
          ' ' +
          styles.apiCollectionWrapper +
          ' ' +
          styles.wd20 +
          ' ' +
          panelStyles
        }
      >
        <Tabs
          className={styles.apiCollectionTabWrapper + ' ' + styles.wd100}
          selectedIndex={currentTab}
          onSelect={this.onTabSelectionChanged}
        >
          <TabList className={styles.apiCollectionTab}>
            {this.getTabs(tabs, currentTab, styles)}
          </TabList>
          {this.getTabPanels(tabs, selectedApi, styles, authApiExpanded)}
        </Tabs>
      </div>
    );
  }
}

ApiCollectionPanel.propTypes = {
  selectedApi: PropTypes.object.isRequired,
  currentTab: PropTypes.number.isRequired,
  tabs: PropTypes.array.isRequired,
  tabSelectionCallback: PropTypes.func.isRequired,
  apiSelectionCallback: PropTypes.func.isRequired,
  clearHistoryCallback: PropTypes.func.isRequired,
  panelStyles: PropTypes.string,
  authApiExpandCallback: PropTypes.func.isRequired,
  authApiExpanded: PropTypes.string.isRequired,
};

export default ApiCollectionPanel;
