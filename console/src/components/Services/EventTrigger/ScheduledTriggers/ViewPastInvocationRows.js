import React from 'react';

// import { useQuery } from '@apollo/react-hooks';
import useHasuraQuery from './useHasuraQuery';
import Button from '../../../Common/Button/Button';

import Tabs from 'react-bootstrap/lib/Tabs';
import AceEditor from 'react-ace';
import Tab from 'react-bootstrap/lib/Tab';
import 'brace/mode/json';

import { fetchPastInvocations } from './Actions';

import { verifySuccessStatus } from '../utils';

import { allowedColumns } from './utils';
import { convertDateTimeToLocale } from '../utils';

import DragFoldTable from '../../../Common/TableCommon/DragFoldTable';

const tableScss = require('../../../Common/TableCommon/TableStyles.scss');

/*
const LIMIT = 5;

const defaultState = {
  limit: LIMIT,
  offset: 0,
  order_by: '',
};
*/
const ViewPastInvocationRows = props => {
  const { dispatch } = props;

  const { loading, error, data, refetch } = useHasuraQuery({
    query: fetchPastInvocations(),
    dispatcher: dispatch,
  });

  if (loading) {
    return <span>Fetching past invocations...</span>;
  }
  if (error) {
    return (
      <span>
        Error fetching
        <code>{JSON.stringify(error)}</code>
      </span>
    );
  }

  const showDataNotAvailableMessage = () => {
    return (
      <div>
        There are no invocations yet! &nbsp;&nbsp;&nbsp;
        <Button
          type="submit"
          color="yellow"
          size="sm"
          data-test="reload-past-invocations"
          onClick={() => refetch()}
        >
          Reload
        </Button>
      </div>
    );
  };

  if (!data || (data && 'result' in data && data.result.length === 1)) {
    return showDataNotAvailableMessage();
  }

  /*
  const renderRefetchButtonText = () => {
    if (loading && checkObjectValidity(data)) {
      return 'Reloading...';
    }
    return 'Reload current operations';
  };
  */

  const info = data.result;

  const getHeaders = () => {
    if (info.length > 1) {
      const getColWidth = (header, contentRows = []) => {
        const MAX_WIDTH = 200;
        const HEADER_PADDING = 24;
        const CONTENT_PADDING = 24;
        const HEADER_FONT = 'bold 16px Gudea';
        const CONTENT_FONT = '14px Gudea';

        const getTextWidth = (text, font) => {
          // Doesn't work well with non-monospace fonts
          // const CHAR_WIDTH = 8;
          // return text.length * CHAR_WIDTH;

          // if given, use cached canvas for better performance
          // else, create new canvas
          const canvas =
            getTextWidth.canvas ||
            (getTextWidth.canvas = document.createElement('canvas'));

          const context = canvas.getContext('2d');
          context.font = font;

          const metrics = context.measureText(text);
          return metrics.width;
        };

        let maxContentWidth = 0;
        for (let i = 0; i < contentRows.length; i++) {
          if (contentRows[i] !== undefined && contentRows[i] !== null) {
            const content = contentRows[i];

            let contentString;
            if (content === null || content === undefined) {
              contentString = 'NULL';
            } else if (typeof content === 'object') {
              contentString = JSON.stringify(content, null, 4);
            } else if (header === 'created_at') {
              contentString = convertDateTimeToLocale(content);
            } else {
              contentString = content.toString();
            }

            const currLength = getTextWidth(contentString, CONTENT_FONT);

            if (currLength > maxContentWidth) {
              maxContentWidth = currLength;
            }
          }
        }

        const maxContentCellWidth = maxContentWidth + CONTENT_PADDING;

        const headerWithUnit = () => {
          return header;
        };

        const headerCellWidth =
          getTextWidth(headerWithUnit(), HEADER_FONT) + HEADER_PADDING;

        return Math.min(
          MAX_WIDTH,
          Math.max(maxContentCellWidth, headerCellWidth)
        );
      };
      const columns = info[0].filter(i => allowedColumns.indexOf(i) !== -1);
      const headerRows = columns.map((c, key) => {
        return {
          Header: (
            <div key={key} className="ellipsis" title="Click to sort">
              {c}
            </div>
          ),
          accessor: c,
          id: c,
          foldable: true,
          width: getColWidth(c, info[1]),
        };
      });
      /*
      const actionRow = {
        Header: <div key={'action_operation_header'} />,
        accessor: 'tableRowActionButtons',
        id: 'tableRowActionButtons',
        width: 100,
      };
      */
      return [...headerRows];
    }
    return [];
  };

  const styles = require('../TableCommon/EventTable.scss');
  const scheduledStyles = require('./ScheduledTrigger.scss');

  const successIcon = (
    <i className={styles.invocationSuccess + ' fa fa-check'} />
  );

  const failureIcon = (
    <i className={styles.invocationFailure + ' fa fa-times'} />
  );

  const getRows = () => {
    if (info.length > 1) {
      const dat = info.slice(1);
      return dat.map(d => {
        const newRow = {};
        // newRow.tableRowActionButtons = null;
        d.forEach((elem, key) => {
          const getElement = () => {
            if (info[0][key] === 'status') {
              return verifySuccessStatus(elem) ? successIcon : failureIcon;
            }
            if (info[0][key] === 'created_at') {
              return convertDateTimeToLocale(elem);
            }
            return elem;
          };
          newRow[info[0][key]] = (
            <div key={key} title={elem}>
              {getElement()}
            </div>
          );
        });
        return newRow;
      });
    }
    return [];
  };

  const _rows = getRows();
  const _columns = getHeaders();

  const renderOperationTable = () => {
    if (info.length > 1) {
      /*
      const renderImportAllowList = () => {
        return (
          <ImportAllowList
            name={collectionName}
            projectId={projectId}
          />
        );
      };
      */
      return (
        <div
          className={`${tableScss.tableContainer} ${
            scheduledStyles.pastInvocationsContainer
          }`}
        >
          <DragFoldTable
            className="-highlight -fit-content"
            data={_rows}
            columns={_columns}
            resizable
            manual
            showPagination={false}
            sortable={false}
            minRows={0}
            SubComponent={row => {
              if (info && info.length >= row.index + 1) {
                const currentRow = info[row.index + 1];
                return (
                  <div style={{ padding: '20px' }}>
                    <Tabs
                      animation={false}
                      defaultActiveKey={1}
                      id="requestResponseTab"
                    >
                      <Tab eventKey={1} title="Request">
                        <AceEditor
                          mode="json"
                          theme="github"
                          name="request"
                          value={JSON.stringify(
                            JSON.parse((currentRow[4] && currentRow[4]) || {}),
                            null,
                            4
                          )}
                          minLines={4}
                          maxLines={100}
                          width="100%"
                          showPrintMargin={false}
                          showGutter={false}
                        />
                      </Tab>
                      <Tab eventKey={2} title="Response">
                        <AceEditor
                          mode="json"
                          theme="github"
                          name="response"
                          value={JSON.stringify(
                            JSON.parse((currentRow[5] && currentRow[5]) || {}),
                            null,
                            4
                          )}
                          minLines={4}
                          maxLines={100}
                          width="100%"
                          showPrintMargin={false}
                          showGutter={false}
                        />
                      </Tab>
                    </Tabs>
                  </div>
                );
              }
              return <span>Something went wrong while processing!</span>;
            }}
          />
        </div>
      );
    }
    return showDataNotAvailableMessage();
  };

  return (
    <div className={`row ${tableScss.add_mar_top}`}>
      <div className="col-xs-12">
        <div>{renderOperationTable()}</div>
      </div>
    </div>
  );
};

export default ViewPastInvocationRows;
