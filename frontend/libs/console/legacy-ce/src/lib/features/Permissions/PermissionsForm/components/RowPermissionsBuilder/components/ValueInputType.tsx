import { areTablesEqual } from '../../../../../hasura-metadata-api';
import { useContext, useEffect } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { tableContext } from './TableProvider';
import { Table } from '../../../../../hasura-metadata-types';
import { getTableDisplayName } from '../../../../../DatabaseRelationships';
import { isEmpty } from 'lodash';
import { Button } from '../../../../../../new-components/Button';
import { graphQLTypeToJsType } from './utils';

export const ValueInputType = ({
  jsType,
  componentLevelId,
  path,
  noValue,
  comparatorName,
  value,
  comparatorType,
}: {
  jsType: string;
  componentLevelId: string;
  path: string[];
  noValue?: boolean;
  comparatorName: string;
  value: any;
  comparatorType: any;
}) => {
  const { setValue } = useContext(rowPermissionsContext);
  const { table } = useContext(tableContext);

  const componentLevelInputId = `${path.join('.')}-input${
    noValue ? '-no-value' : ''
  }`;
  const inputType =
    jsType === 'boolean' ? 'checkbox' : jsType === 'string' ? 'text' : 'number';

  switch (jsType) {
    case 'boolean':
      return (
        <div className="flex">
          <select
            data-testid={componentLevelId}
            className="border border-gray-200 rounded-md"
            value={JSON.stringify(value)}
            onChange={e => {
              setValue(path, JSON.parse(e.target.value) as Table);
            }}
          >
            <option key="true" value="true">
              True
            </option>
            <option key="false" value="false">
              False
            </option>
            );
          </select>
        </div>
      );

    default:
      return (
        <input
          data-testid={`${componentLevelInputId}`}
          disabled={comparatorName === '_where' && isEmpty(table)}
          className="border border-gray-200 rounded-md p-2 !mr-4"
          type={inputType}
          value={value}
          onChange={e => {
            setValue(path, graphQLTypeToJsType(e.target.value, comparatorType));
          }}
        />
      );
  }
};
