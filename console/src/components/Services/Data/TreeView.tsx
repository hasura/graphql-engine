import React, { useState, useEffect } from 'react';
import { Link } from 'react-router';
import styles from '../../Common/Layout/LeftSubSidebar/LeftSubSidebar.scss';
import {
  getFunctionModifyRoute,
  getTableBrowseRoute,
} from '../../Common/utils/routesUtils';
import GqlCompatibilityWarning from '../../Common/GqlCompatibilityWarning/GqlCompatibilityWarning';

type SourceItemsTypes =
  | 'database'
  | 'schema'
  | 'view'
  | 'enum'
  | 'function'
  | 'table';

type SourceItem = {
  name: string;
  type: SourceItemsTypes;
  children?: SourceItem[];
};

const activeStyle = {
  color: '#fd9540',
};

type LeafItemsViewProps = {
  item: SourceItem;
  currentSource: string;
  currentSchema: string;
  pathname: string;
};
const LeafItemsView: React.FC<LeafItemsViewProps> = ({
  item,
  currentSource,
  currentSchema,
  pathname,
}) => {
  const [isOpen, setIsOpen] = useState(false);
  const regex = new RegExp(
    `\\/data\\/${currentSource}\\/schema\\/${currentSchema}\\/(tables|functions|views)\\/${item.name}\\/`
  );
  const isActive = regex.test(pathname);

  const isView = item.type === 'view';

  const iconStyle = {
    marginRight: '5px',
    fontSize: '12px',
    width: '12px',
  };

  const activeIcon =
    'data:image/svg+xml;base64,PHN2ZyBpZD0iU3ZnanNTdmcxMDAxIiB3aWR0aD0iMjg4IiBoZWlnaHQ9IjI4OCIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB2ZXJzaW9uPSIxLjEiIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB4bWxuczpzdmdqcz0iaHR0cDovL3N2Z2pzLmNvbS9zdmdqcyI+PGRlZnMgaWQ9IlN2Z2pzRGVmczEwMDIiPjwvZGVmcz48ZyBpZD0iU3ZnanNHMTAwOCIgdHJhbnNmb3JtPSJtYXRyaXgoMC45MTY3LDAsMCwwLjkxNjcsMTEuOTk1MTk4NDExMTc4NTkyLDExLjk5NTIwMDAwMDAwMDAxMSkiPjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB3aWR0aD0iMjg4IiBoZWlnaHQ9IjI4OCIgZW5hYmxlLWJhY2tncm91bmQ9Im5ldyAwIDAgMTQyLjUxNCAxNDIuNTE0IiB2aWV3Qm94PSIwIDAgMTQyLjUxNCAxNDIuNTE0Ij48cGF0aCBmaWxsPSIjZmQ5NTQwIiBkPSJNMzQuMzY3IDE0Mi41MTRjMTEuNjQ1IDAgMTcuODI3LTEwLjQgMTkuNjQ1LTE2LjU0NC4wMjktLjA5Ny4wNTYtLjE5Ni4wODEtLjI5NyA0LjIzNi0xNy41NDUgMTAuOTg0LTQ1LjM1MyAxNS45ODMtNjUuNThoMTcuODg2YzMuMzYzIDAgNi4wOS0yLjcyNiA2LjA5LTYuMDkgMC0zLjM2NC0yLjcyNy02LjA5LTYuMDktNi4wOUg3My4xMDNjMS42LTYuMzczIDIuNzcxLTEwLjkxMiAzLjIzMi0xMi40NjFsLjUxMi0xLjczNGMxLjg4OC02LjQ0MyA2LjMwOS0yMS41MzUgMTMuMTQ2LTIxLjUzNSA2LjM0IDAgNy4yODUgOS43NjQgNy4zMjggMTAuMjM2LjI3IDMuMzQzIDMuMTg2IDUuODY4IDYuNTM3IDUuNTc5IDMuMzU0LS4yNTYgNS44NjQtMy4xODcgNS42MDUtNi41MzlDMTA4Ljg5NCAxNC4wMzYgMTA0LjA4NyAwIDg5Ljk5MSAwIDc0LjAzIDAgNjguMDM4IDIwLjQ1OCA2NS4xNTkgMzAuMjkybC0uNDkgMS42NTljLS41ODUgMS45NDYtMi4xMiA3Ljk0Mi00LjEyMiAxNS45NjJIMzkuMjM5Yy0zLjM2NCAwLTYuMDkgMi43MjYtNi4wOSA2LjA5IDAgMy4zNjQgMi43MjYgNi4wOSA2LjA5IDYuMDlINTcuNTNjLTYuMjUzIDI1LjM2Mi0xNC4zMzQgNTguODE1LTE1LjIyMyA2Mi40OTgtLjMzMi45NjUtMi44MjkgNy43NDItNy45MzcgNy43NDItNy44IDAtMTEuMTc3LTEwLjk0OC0xMS4yMDQtMTEuMDMtLjkzNi0zLjIyOS00LjMwNS01LjA5OC03LjU0NC00LjE1Ni0zLjIzLjkzNy01LjA5MiA0LjMxNC00LjE1NiA3LjU0NUMxMy41OTcgMTMwLjA1MyAyMC44MTYgMTQyLjUxNCAzNC4zNjcgMTQyLjUxNHpNMTI0LjY4NSAxMjYuODA5YzMuNTg5IDAgNi42MDUtMi41NDkgNi42MDUtNi42MDcgMC0xLjg4NS0uNzU0LTMuNTg2LTIuMzU5LTUuNDc0bC0xMi42NDYtMTQuNTM0IDEyLjI3MS0xNC4zNDZjMS4xMzItMS40MTYgMS45OC0yLjkyNiAxLjk4LTQuOTA4IDAtMy41OS0yLjkyNy02LjIzMS02LjcwMy02LjIzMS0yLjU0NyAwLTQuNTI3IDEuNjA0LTYuMjI5IDMuNjg0bC05LjUzMSAxMi40NTRMOTguNzMgNzguMzkxYy0xLjg5LTIuMzU3LTMuODY5LTMuNjgyLTYuNy0zLjY4Mi0zLjU5IDAtNi42MDcgMi41NTEtNi42MDcgNi42MDkgMCAxLjg4NS43NTYgMy41ODYgMi4zNTcgNS40NzFsMTEuNzk5IDEzLjU5Mkw4Ni42NDcgMTE1LjY3Yy0xLjIyNyAxLjQxNi0xLjk4IDIuOTI2LTEuOTggNC45MDggMCAzLjU4OSAyLjkyNiA2LjIyOSA2LjY5OSA2LjIyOSAyLjU0OSAwIDQuNTMtMS42MDQgNi4yMjktMy42ODJsMTAuMTktMTMuNCAxMC4xOTMgMTMuNEMxMTkuODcyIDEyNS40ODggMTIxLjg1NCAxMjYuODA5IDEyNC42ODUgMTI2LjgwOXoiIGNsYXNzPSJjb2xvcjc2N0U5MyBzdmdTaGFwZSI+PC9wYXRoPjwvc3ZnPjwvZz48L3N2Zz4=';
  const nonActiveIcon =
    'data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iaXNvLTg4NTktMSI/Pg0KPCEtLSBHZW5lcmF0b3I6IEFkb2JlIElsbHVzdHJhdG9yIDE2LjAuMCwgU1ZHIEV4cG9ydCBQbHVnLUluIC4gU1ZHIFZlcnNpb246IDYuMDAgQnVpbGQgMCkgIC0tPg0KPCFET0NUWVBFIHN2ZyBQVUJMSUMgIi0vL1czQy8vRFREIFNWRyAxLjEvL0VOIiAiaHR0cDovL3d3dy53My5vcmcvR3JhcGhpY3MvU1ZHLzEuMS9EVEQvc3ZnMTEuZHRkIj4NCjxzdmcgdmVyc2lvbj0iMS4xIiBpZD0iQ2FwYV8xIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB4PSIwcHgiIHk9IjBweCINCgkgd2lkdGg9IjE0Mi41MTRweCIgaGVpZ2h0PSIxNDIuNTE0cHgiIHZpZXdCb3g9IjAgMCAxNDIuNTE0IDE0Mi41MTQiIHN0eWxlPSJlbmFibGUtYmFja2dyb3VuZDpuZXcgMCAwIDE0Mi41MTQgMTQyLjUxNDsiDQoJIHhtbDpzcGFjZT0icHJlc2VydmUiPg0KPGc+DQoJPGc+DQoJCTxwYXRoIGQ9Ik0zNC4zNjcsMTQyLjUxNGMxMS42NDUsMCwxNy44MjctMTAuNCwxOS42NDUtMTYuNTQ0YzAuMDI5LTAuMDk3LDAuMDU2LTAuMTk2LDAuMDgxLTAuMjk3DQoJCQljNC4yMzYtMTcuNTQ1LDEwLjk4NC00NS4zNTMsMTUuOTgzLTY1LjU4aDE3Ljg4NmMzLjM2MywwLDYuMDktMi43MjYsNi4wOS02LjA5YzAtMy4zNjQtMi43MjctNi4wOS02LjA5LTYuMDlINzMuMTAzDQoJCQljMS42LTYuMzczLDIuNzcxLTEwLjkxMiwzLjIzMi0xMi40NjFsMC41MTItMS43MzRjMS44ODgtNi40NDMsNi4zMDktMjEuNTM1LDEzLjE0Ni0yMS41MzVjNi4zNCwwLDcuMjg1LDkuNzY0LDcuMzI4LDEwLjIzNg0KCQkJYzAuMjcsMy4zNDMsMy4xODYsNS44NjgsNi41MzcsNS41NzljMy4zNTQtMC4yNTYsNS44NjQtMy4xODcsNS42MDUtNi41MzlDMTA4Ljg5NCwxNC4wMzYsMTA0LjA4NywwLDg5Ljk5MSwwDQoJCQlDNzQuMDMsMCw2OC4wMzgsMjAuNDU4LDY1LjE1OSwzMC4yOTJsLTAuNDksMS42NTljLTAuNTg1LDEuOTQ2LTIuMTIsNy45NDItNC4xMjIsMTUuOTYySDM5LjIzOWMtMy4zNjQsMC02LjA5LDIuNzI2LTYuMDksNi4wOQ0KCQkJYzAsMy4zNjQsMi43MjYsNi4wOSw2LjA5LDYuMDlINTcuNTNjLTYuMjUzLDI1LjM2Mi0xNC4zMzQsNTguODE1LTE1LjIyMyw2Mi40OThjLTAuMzMyLDAuOTY1LTIuODI5LDcuNzQyLTcuOTM3LDcuNzQyDQoJCQljLTcuOCwwLTExLjE3Ny0xMC45NDgtMTEuMjA0LTExLjAzYy0wLjkzNi0zLjIyOS00LjMwNS01LjA5OC03LjU0NC00LjE1NmMtMy4yMywwLjkzNy01LjA5Miw0LjMxNC00LjE1Niw3LjU0NQ0KCQkJQzEzLjU5NywxMzAuMDUzLDIwLjgxNiwxNDIuNTE0LDM0LjM2NywxNDIuNTE0eiIgZmlsbD0iIzc2N0U5MyIvPg0KCQk8cGF0aCBkPSJNMTI0LjY4NSwxMjYuODA5YzMuNTg5LDAsNi42MDUtMi41NDksNi42MDUtNi42MDdjMC0xLjg4NS0wLjc1NC0zLjU4Ni0yLjM1OS01LjQ3NGwtMTIuNjQ2LTE0LjUzNGwxMi4yNzEtMTQuMzQ2DQoJCQljMS4xMzItMS40MTYsMS45OC0yLjkyNiwxLjk4LTQuOTA4YzAtMy41OS0yLjkyNy02LjIzMS02LjcwMy02LjIzMWMtMi41NDcsMC00LjUyNywxLjYwNC02LjIyOSwzLjY4NGwtOS41MzEsMTIuNDU0TDk4LjczLDc4LjM5MQ0KCQkJYy0xLjg5LTIuMzU3LTMuODY5LTMuNjgyLTYuNy0zLjY4MmMtMy41OSwwLTYuNjA3LDIuNTUxLTYuNjA3LDYuNjA5YzAsMS44ODUsMC43NTYsMy41ODYsMi4zNTcsNS40NzFsMTEuNzk5LDEzLjU5Mg0KCQkJTDg2LjY0NywxMTUuNjdjLTEuMjI3LDEuNDE2LTEuOTgsMi45MjYtMS45OCw0LjkwOGMwLDMuNTg5LDIuOTI2LDYuMjI5LDYuNjk5LDYuMjI5YzIuNTQ5LDAsNC41My0xLjYwNCw2LjIyOS0zLjY4MmwxMC4xOS0xMy40DQoJCQlsMTAuMTkzLDEzLjRDMTE5Ljg3MiwxMjUuNDg4LDEyMS44NTQsMTI2LjgwOSwxMjQuNjg1LDEyNi44MDl6IiBmaWxsPSIjNzY3RTkzIi8+DQoJPC9nPg0KPC9nPg0KPGc+DQo8L2c+DQo8Zz4NCjwvZz4NCjxnPg0KPC9nPg0KPGc+DQo8L2c+DQo8Zz4NCjwvZz4NCjxnPg0KPC9nPg0KPGc+DQo8L2c+DQo8Zz4NCjwvZz4NCjxnPg0KPC9nPg0KPGc+DQo8L2c+DQo8Zz4NCjwvZz4NCjxnPg0KPC9nPg0KPGc+DQo8L2c+DQo8Zz4NCjwvZz4NCjxnPg0KPC9nPg0KPC9zdmc+DQo=';

  return (
    <>
      <div
        className={styles.sidebarTablePadding}
        onClick={() => {
          setIsOpen(prev => !prev);
        }}
        onKeyDown={() => {
          setIsOpen(prev => !prev);
        }}
        role="button"
      >
        <span
          className={
            item.children &&
            `${styles.title} ${isOpen ? '' : styles.titleClosed}`
          }
        >
          {item.type === 'function' ? (
            <Link
              to={getFunctionModifyRoute(
                currentSchema,
                currentSource,
                item.name
              )}
              data-test={item.name}
              style={isActive ? activeStyle : {}}
            >
              <img
                style={iconStyle}
                src={isActive ? activeIcon : nonActiveIcon}
                alt="function icon"
              />
              {item.name}
            </Link>
          ) : (
            <>
              <Link
                to={getTableBrowseRoute(
                  currentSchema,
                  currentSource,
                  item.name,
                  !isView
                )}
                data-test={item.name}
                style={isActive ? activeStyle : {}}
              >
                {item.type === 'enum' ? (
                  <i className="fa fa-list-ul" />
                ) : (
                  <i className="fa fa-table" />
                )}
                {item.type === 'view' ? <i>{item.name}</i> : item.name}
              </Link>
              <GqlCompatibilityWarning
                identifier={item.name}
                className={styles.add_mar_left_mid}
                ifWarningCanBeFixed
              />
            </>
          )}
        </span>
      </div>
    </>
  );
};

type SchemaItemsViewProps = {
  item: SourceItem;
  currentSource: string;
  isActive: boolean;
  setActiveSchema: (value: string) => void;
  pathname: string;
  databaseLoading: boolean;
};
const SchemaItemsView: React.FC<SchemaItemsViewProps> = ({
  item,
  currentSource,
  isActive,
  setActiveSchema,
  pathname,
  databaseLoading,
}) => {
  const [isOpen, setIsOpen] = useState(false);
  const showActiveStyle =
    pathname === `/data/${currentSource}/schema/${item.name}`;
  useEffect(() => {
    setIsOpen(isActive);
  }, [isActive]);

  return (
    <>
      <div
        onClick={() => {
          setActiveSchema(encodeURIComponent(item.name));
        }}
        onKeyDown={() => {
          setActiveSchema(encodeURIComponent(item.name));
        }}
        role="button"
        className={styles.padd_bottom_small}
        style={showActiveStyle ? activeStyle : {}}
      >
        <span
          className={
            item.children &&
            `${styles.title} ${isOpen ? '' : styles.titleClosed}`
          }
        >
          <i className={`${isOpen ? 'fa fa-folder-open' : 'fa fa-folder'}`} />{' '}
          {item.name}
        </span>
      </div>
      <ul className={styles.reducedChildPadding}>
        {isOpen && item.children ? (
          !databaseLoading ? (
            item.children.map((child, key) => (
              <li key={key}>
                <LeafItemsView
                  item={child}
                  currentSource={currentSource}
                  currentSchema={item.name}
                  key={key}
                  pathname={pathname}
                />
              </li>
            ))
          ) : (
            <li>
              <span
                className={`${styles.sidebarTablePadding} ${styles.padd_bottom_small}`}
              >
                <i className="fa fa-table" />
                <span className={styles.loaderBar} />
              </span>
            </li>
          )
        ) : null}
      </ul>
    </>
  );
};

type DatabaseItemsViewProps = {
  item: SourceItem;
  isActive: boolean;
  setActiveDataSource: (activeSource: string) => void;
  onSchemaChange: (value: string) => void;
  currentSchema: string;
  pathname: string;
  databaseLoading: boolean;
};
const DatabaseItemsView: React.FC<DatabaseItemsViewProps> = ({
  item,
  isActive,
  setActiveDataSource,
  onSchemaChange,
  currentSchema,
  pathname,
  databaseLoading,
}) => {
  const [isOpen, setIsOpen] = useState(false);
  const showActiveStyle = [
    `/data/${item.name}/`,
    `/data/${item.name}`,
    `/data/${item.name}/display`,
    `/data/${item.name}/gallery`,
  ].includes(pathname);

  useEffect(() => {
    setIsOpen(isActive);
  }, [isActive]);
  const handleSelectSchema = (value: string) => {
    onSchemaChange(value);
  };

  return (
    <div className={styles.padd_bottom_small}>
      <div
        onClick={() => {
          setActiveDataSource(item.name);
        }}
        onKeyDown={() => {
          setActiveDataSource(item.name);
        }}
        role="button"
        className={styles.padd_bottom_small}
      >
        <span
          className={
            item.children &&
            `${styles.title} ${isOpen ? '' : styles.titleClosed}`
          }
          style={showActiveStyle ? activeStyle : {}}
        >
          <i className={`fa fa-${item.type}`} /> {item.name}
        </span>
      </div>
      {isOpen && item.children
        ? item.children.map((child, key) => (
            <li key={key}>
              <SchemaItemsView
                item={child}
                currentSource={item.name}
                isActive={child.name === currentSchema}
                setActiveSchema={handleSelectSchema}
                key={key}
                pathname={pathname}
                databaseLoading={databaseLoading}
              />
            </li>
          ))
        : null}
      {databaseLoading && isActive ? (
        <li>
          <span
            className={`${styles.title} ${styles.titleClosed} ${styles.padd_bottom_small}`}
          >
            <i className="fa fa-folder" />
            <span className={styles.loaderBar} />
          </span>
        </li>
      ) : null}
    </div>
  );
};

type TreeViewProps = {
  items: SourceItem[];
  onDatabaseChange: (value: string) => void;
  onSchemaChange: (value: string) => void;
  currentDataSource: string;
  currentSchema: string;
  pathname: string;
  databaseLoading: boolean;
  preLoadState: boolean;
};
const TreeView: React.FC<TreeViewProps> = ({
  items,
  onDatabaseChange,
  currentDataSource,
  onSchemaChange,
  currentSchema,
  pathname,
  databaseLoading,
  preLoadState,
}) => {
  const handleSelectDataSource = (dataSource: string) => {
    onDatabaseChange(dataSource);
  };

  if (items.length === 0) {
    return preLoadState ? (
      <div className={styles.treeNav}>
        <span className={`${styles.title} ${styles.padd_bottom_small}`}>
          <i className="fa fa-database" />
        </span>
        <span className={styles.loaderBar} />
        <li>
          <span className={`${styles.title} ${styles.padd_bottom_small}`}>
            <i className="fa fa-folder" />
            <span className={styles.loaderBar} />
            <ul className={styles.reducedChildPadding}>
              <li
                className={`${styles.sidebarTablePadding} ${styles.add_mar_left_mid}`}
              >
                <i className="fa fa-table" />
                <span className={styles.loaderBar} />
              </li>
            </ul>
          </span>
        </li>
      </div>
    ) : (
      <li className={styles.noChildren} data-test="sidebar-no-services">
        <i>No data available</i>
      </li>
    );
  }

  return (
    <div className={styles.treeNav}>
      {items.map((item, key) => (
        <DatabaseItemsView
          item={item}
          onSchemaChange={onSchemaChange}
          key={key}
          isActive={currentDataSource === item.name}
          setActiveDataSource={handleSelectDataSource}
          currentSchema={currentSchema}
          pathname={pathname}
          databaseLoading={databaseLoading}
        />
      ))}
    </div>
  );
};

export default TreeView;
