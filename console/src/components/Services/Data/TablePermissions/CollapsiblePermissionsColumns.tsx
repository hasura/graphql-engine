import React, { FC } from 'react';
import { Tooltip } from 'react-bootstrap';

import CollapsibleToggle from '../../../Common/CollapsibleToggle/CollapsibleToggle';
import PermissionSectionHeader from './PermissionSectionHeader';
import { getEdForm } from '../utils';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import Button from '../../../Common/Button/Button';
import {
  noPermissionsMsg,
  getAccessText,
  hasPermission,
  BaseQueryType,
} from './utils';
import PermissionCheckbox from './PermissionCheckbox';
import {
  ComputedField,
  TableRelationship,
  TableColumn,
} from '../../../Common/utils/pgUtils';

interface CollapsiblePermissionsColumnsProps {
  query: BaseQueryType;
  status: string;
  isOpen: boolean;
  className?: string;
  title: string;
  permissionsState: any;
  noPermissions?: boolean;
  columns: TableColumn[];
  relationships: TableRelationship[];
  groupedComputedFields: {
    scalar: ComputedField[];
    table: ComputedField[];
  };
  onTogglePermission: (value: { name: string; type: string }) => void;
  onToggleAllPermissions: () => void;
}

export const CollapsiblePermissionsColumns: FC<CollapsiblePermissionsColumnsProps> = props => {
  const {
    query,
    status,
    isOpen,
    className,
    title,
    permissionsState,
    noPermissions,
    columns,
    relationships,
    groupedComputedFields,
    onTogglePermission,
    onToggleAllPermissions,
  } = props;

  const { role } = permissionsState;

  const externalObjects = [];
  if (relationships.length) {
    externalObjects.push('relationships');
  }
  if (query === 'select' && groupedComputedFields.table.length) {
    externalObjects.push('table computed fields');
  }

  return (
    <CollapsibleToggle
      title={
        <PermissionSectionHeader
          title={`Column ${query} permissions`}
          tooltip={
            <Tooltip id="tooltip-row-permissions">
              {`Choose columns allowed to be ${getEdForm(query)}`}
            </Tooltip>
          }
          sectionStatus={status}
        />
      }
      useDefaultTitleStyle
      testId="toggle-col-permission"
      isOpen={isOpen}
    >
      <div className={className} title={title}>
        <div>
          <span
            dangerouslySetInnerHTML={{
              __html: `Allow role <b>${role}</b> ${getAccessText(
                query
              )} <b>columns</b>:`,
            }}
            className={styles.add_mar_right}
          />

          <Button
            size="xs"
            onClick={onToggleAllPermissions}
            disabled={noPermissions}
            title={noPermissions ? noPermissionsMsg : ''}
            data-test="toggle-all-col-btn"
          >
            Toggle All
          </Button>
        </div>

        <div className={styles.permissionColumns}>
          {columns.map((column, index) => {
            const { column_name: fieldName } = column;
            return (
              <div
                key={`${column.column_name}_${index}`}
                className={styles.permissionColumn}
              >
                <PermissionCheckbox
                  labelClassName={styles.permissionColumnText}
                  checked={hasPermission({
                    permissionsState,
                    query,
                    fieldType: 'columns',
                    fieldName,
                  })}
                  onToggle={() =>
                    onTogglePermission({
                      name: fieldName,
                      type: 'columns',
                    })
                  }
                  fieldName={fieldName}
                />

                {query === 'select' &&
                  groupedComputedFields.scalar.map(scalarComputedField => {
                    return (
                      <PermissionCheckbox
                        checked={false}
                        onToggle={() =>
                          onTogglePermission({
                            name: scalarComputedField.computed_field_name,
                            type: 'computed_fields',
                          })
                        }
                        fieldName={scalarComputedField.computed_field_name}
                      />
                    );
                  })}
              </div>
            );
          })}
        </div>

        <div className={styles.add_mar_top_small}>
          For <b>{externalObjects.join(', ')}</b>, set permissions for the
          corresponding tables/views.
        </div>
      </div>
    </CollapsibleToggle>
  );
};

export default CollapsiblePermissionsColumns;
