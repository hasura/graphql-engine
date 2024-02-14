import React, { useState, useEffect, useRef } from 'react';

// import { useQuery } from '@apollo/react-hooks';
import { Link } from 'react-router';
import { getAllowList } from './utils';

// import { fetchOperations } from './graphql.queries';

import { Tooltip } from '@hasura/console-legacy-ce';

import { createFilter } from '../Error/utils';

import { excludedColumns } from './constants';

import FilterCheckboxComponent from '../StatsPanel/Filter/FilterCheckboxComponent';

import useSelectable from './useSelectable';

import DeleteOperationsFromAllowList from './DeleteOperationsFromAllowList';
import ExportAllowListAsJson from './ExportAllowListAsJson';
import SelectProjectAndImport from './SelectProjectAndImport';

import { ActionsPanel } from '../Common/ActionsPanel';

// import ImportAllowList from './ImportAllowList';

/*
import AddToOperationGroup from '../Operations/AddToOperationGroup';
*/
// import DeleteFromOperationGroup from '../Operations/DeleteFromOperationGroup';

import {
  stripUnderScore,
  capitalize,
  transformedVals,
  transformedHeaderVal,
} from '../Operations/utils';

import { OPERATION_NAME_SYMBOL, relativeModulePath } from '../constants';

import { DragFoldTable, tableScss } from '@hasura/console-legacy-ce';

import inspectRow from '../images/usage.svg';
import styles from '../Metrics.module.scss';
import failure from '../images/failure.svg';
import success from '../images/success.svg';

const LIMIT = 5;

const defaultState = {
  limit: LIMIT,
  offset: 0,
  order_by: '',
};

const BrowseAllowLists = props => {
  const [browseState] = useState(defaultState);
  const [selectData, update] = useSelectable();
  const {
    dispatch,
    label,
    projectId,
    RenderLink,
    collectionName,
    update: updateRefetches,
    changeToNewTab,
    updateMetadata,
    metadata,
    refetchMetadata: refetch,
  } = props;

  /* Get the list of filter types by filtering the type key of the filters list itself */

  const { limit, offset } = browseState;
  useEffect(() => {
    if (metadata?.query_collections) {
      const allowList = getAllowList(metadata, collectionName);
      const queries = allowList?.definition?.queries || [];
      updateMetadata(queries.length || 'N/A');
    }
  }, [metadata]);
  const firstLoad = useRef(true);
  useEffect(() => {
    if (firstLoad.current) {
      refetch();
    }
    firstLoad.current = false;
  }, [metadata]);
  const data = metadata?.allowlist;

  useEffect(() => {
    updateRefetches({
      key: 'BrowseAllowLists',
      value: refetch,
    });
    return () => updateMetadata(null);
  }, []);

  const allowList = getAllowList(metadata, collectionName);
  const info = allowList?.definition?.queries || [];

  if (metadata?.loading) {
    return <span>Fetching current operations...</span>;
  }
  const showDataNotAvailableMessage = () => {
    return (
      <div>
        There are no operations in this allow list. <br /> <br />
        <ul className={styles.ul_pad_remove}>
          <li>
            Import allow list from{' '}
            <SelectProjectAndImport
              refetch={refetch}
              currentProjectId={projectId}
              groupName={collectionName}
              dispatch={dispatch}
            />
          </li>
          <br />
          <li>
            Add operations{' '}
            <a href="#" onClick={changeToNewTab}>
              manually
            </a>
          </li>
        </ul>
      </div>
    );
  };

  if (!data || (data && data.length === 0)) {
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

  const getHeaders = () => {
    if (info.length > 0) {
      const getColWidth = (header, contentRows = []) => {
        const MAX_WIDTH = 600;
        const HEADER_PADDING = 62;
        const CONTENT_PADDING = 36;
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
          if (contentRows[i] !== undefined && contentRows[i][header] !== null) {
            const content = contentRows[i][header];

            let contentString;
            if (content === null || content === undefined) {
              contentString = 'NULL';
            } else if (typeof content === 'object') {
              contentString = JSON.stringify(content, null, 4);
            } else {
              if (header in transformedVals) {
                contentString = transformedVals[header](content);
              } else {
                contentString = content.toString();
              }
            }

            const currLength = getTextWidth(contentString, CONTENT_FONT);

            if (currLength > maxContentWidth) {
              maxContentWidth = currLength;
            }
          }
        }

        const maxContentCellWidth = maxContentWidth + CONTENT_PADDING + 12;

        const headerWithUnit = () => {
          if (header in transformedHeaderVal) {
            return transformedHeaderVal[header](header);
          }
          return header;
        };

        const headerCellWidth =
          getTextWidth(headerWithUnit(), HEADER_FONT) + HEADER_PADDING;

        return Math.min(
          MAX_WIDTH,
          Math.max(maxContentCellWidth, headerCellWidth)
        );
      };
      const columns = info[0];
      const headerRows = Object.keys(columns)
        .filter(c1 => excludedColumns.indexOf(c1) === -1)
        .map((c, key) => {
          const unitIfThereIs = () => {
            return '';
          };

          const getWithUnitHeaderTitle = () => {
            return `${capitalize(stripUnderScore(c))}${unitIfThereIs()}${' '}`;
          };
          return {
            Header: (
              <div
                key={key}
                className={`${styles.columnHeader} ellipsis`}
                title="Click to sort"
              >
                {getWithUnitHeaderTitle()}
              </div>
            ),
            accessor: c,
            id: c,
            foldable: true,
            width: getColWidth(c, info),
          };
        });
      const actionRow = {
        Header: (
          <div
            key={'action_operation_header'}
            className={`${styles.columnHeader}`}
          >
            Actions
          </div>
        ),
        accessor: 'tableRowActionButtons',
        id: 'tableRowActionButtons',
        width: 100,
      };
      const onChangeAllSelection = () => {
        const getOperationNames = () => {
          return info.map(o => o.name);
        };
        const names = (info.length > 0 && getOperationNames()) || [];
        if (selectData.length !== info.length) {
          update(names);
        } else {
          update([]);
        }
      };
      const selectRowAction = {
        Header: (
          <div
            key={'action_operation_header'}
            className={`${styles.columnHeader} ${styles.textCenter}`}
          >
            <FilterCheckboxComponent
              title={''}
              id={'all'}
              onChange={onChangeAllSelection}
              checked={selectData.length === info.length}
              bsClass={styles.bsCheckBoxClass}
            />
          </div>
        ),
        accessor: 'tableRowSelectAction',
        id: 'tableRowSelectAction',
        width: 80,
      };
      return [selectRowAction, actionRow, ...headerRows];
    }
    return [];
  };

  const renderIcon = name => {
    const getOperationUrl = () => {
      const filters = [createFilter(OPERATION_NAME_SYMBOL, name)];
      return {
        pathname: `${relativeModulePath}/operations`,
        search: `?filters=${window.encodeURI(JSON.stringify(filters))}`,
      };
    };
    const renderLink = () => {
      const linkParam = getOperationUrl();
      const { search } = linkParam;
      const icon = (
        <img
          className={styles.actionImg}
          src={inspectRow}
          alt={'Show operations'}
        />
      );
      if (RenderLink) {
        return (
          <RenderLink pageName="operations" search={search}>
            {icon}
          </RenderLink>
        );
      }
      return <Link to={linkParam}>{icon}</Link>;
    };
    return (
      <div className={styles.iconPadding}>
        <Tooltip side="right" tooltipContentChildren="View operations">
          {renderLink()}
        </Tooltip>
      </div>
    );
  };

  /*
  const renderActionButtonForGroups = name => {
    return (
      <DeleteFromOperationGroup
        projectId={projectId}
        operationGroupName={collectionName}
        operationName={name}
        refetch={refetch}
        dispatch={dispatch}
      />
    );
  };
  */

  const renderActionButtonForGroups = name => {
    return (
      <FilterCheckboxComponent
        title={''}
        id={name}
        onChange={update}
        checked={selectData.indexOf(name) !== -1}
        bsClass={styles.bsCheckBoxClass}
      />
    );
  };

  const getRows = () => {
    if (info.length > 0) {
      return info.map(d => {
        const newRow = {};
        newRow.tableRowActionButtons = (
          <div className={styles.textCenter}>{renderIcon(d.name)}</div>
        );
        newRow.tableRowSelectAction = (
          <div className={styles.textCenter}>
            {renderActionButtonForGroups(d.name)}
          </div>
        );
        Object.keys(d)
          .filter(c1 => excludedColumns.indexOf(c1) === -1)
          .forEach((elem, key) => {
            const renderElement = () => {
              if (elem === 'success') {
                if (!d[elem]) {
                  return <img src={success} alt={'Success'} />;
                }
                return <img src={failure} alt={'Failure'} />;
              }
              if (elem in transformedVals) {
                return transformedVals[elem](d[elem]);
              }
              /*
            if (elem === 'time') {
              return moment(new Date(d[elem])).fromNow();
            }
            */
              return d[elem];
            };
            const getTitle = () => {
              if (typeof d[elem] === 'boolean') {
                return '';
              }
              return d[elem];
            };
            newRow[elem] = (
              <div key={key} className={styles.columnRow} title={getTitle()}>
                {renderElement()}
              </div>
            );
          });
        return newRow;
      });
    }
    return [];
  };

  const getCount = () => {
    if ('aggregate' in info) {
      return info.aggregate.count;
    }
    return 0;
  };

  const _rows = getRows();
  const _columns = getHeaders();

  const renderHeaderSection = () => {
    return (
      label && (
        <div className={styles.displayFlex + ' ' + styles.addPaddBottom}>
          <div className={styles.subHeader}>{label}</div>
          {/*
        <div className={styles.operationBtn}>
          <button onClick={() => refetch()}>{renderRefetchButtonText()}</button>
        </div>
        */}
        </div>
      )
    );
  };

  const renderOperationTable = () => {
    if (_rows.length > 0) {
      const renderDeleteIcon = () => {
        return (
          <DeleteOperationsFromAllowList
            projectId={projectId}
            operationGroupName={collectionName}
            operationNames={selectData}
            refetch={refetch}
            dispatch={dispatch}
            onCompleted={() => update([])}
          />
        );
      };
      const renderExportAllowList = () => {
        return (
          <ExportAllowListAsJson name={collectionName} projectId={projectId} />
        );
      };
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
        <div className={tableScss.tableContainer + ' ' + styles.tableFullWidth}>
          <ActionsPanel>
            {renderDeleteIcon()}
            {renderExportAllowList()}
            {/* renderImportAllowList() */}
          </ActionsPanel>
          <DragFoldTable
            className="-highlight -fit-content"
            data={_rows}
            columns={_columns}
            resizable
            manual
            showPagination={false}
            sortable={false}
            minRows={0}
            pageSize={limit}
            pages={Math.ceil(getCount() / limit)}
            page={Math.floor(offset / limit)}
          />
        </div>
      );
    }
    return showDataNotAvailableMessage();
  };

  return (
    <div className={`row ${tableScss.add_mar_top}`}>
      <div className="col-xs-12">
        <div className={styles.clearBoth}>
          {renderHeaderSection()}
          {renderOperationTable()}
        </div>
      </div>
    </div>
  );
};

export default BrowseAllowLists;
