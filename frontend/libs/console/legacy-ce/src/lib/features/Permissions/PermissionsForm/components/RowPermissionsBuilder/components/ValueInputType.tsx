import { useContext } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { tableContext } from './TableProvider';
import { Table } from '../../../../../hasura-metadata-types';
import { isEmpty } from 'lodash';
import { graphQLTypeToJsType } from './utils';

export const ValueInputType = ({
  jsType,
  componentLevelId,
  path,
  comparatorName,
  value,
  comparatorType,
}: {
  jsType: string;
  componentLevelId: string;
  path: string[];
  comparatorName: string;
  value: any;
  comparatorType: any;
}) => {
  const { setValue } = useContext(rowPermissionsContext);
  const { table } = useContext(tableContext);

  switch (jsType) {
    case 'boolean':
      return (
        <div className="flex">
          <select
            data-testid={componentLevelId}
            className="border border-gray-200 rounded-md"
            value={JSON.stringify(value)}
            defaultValue={JSON.parse(value) ?? false}
            onChange={e => {
              setValue(path, JSON.parse(e.target.value) as Table);
            }}
          >
            <option key="false" value="false">
              False
            </option>
            <option key="true" value="true">
              True
            </option>
          </select>
        </div>
      );

    default:
      return (
        <input
          data-testid={componentLevelId}
          disabled={comparatorName === '_where' && isEmpty(table)}
          className="border border-gray-200 rounded-md p-2 !mr-4"
          type="text"
          value={value}
          onChange={e => {
            setValue(path, graphQLTypeToJsType(e.target.value, comparatorType));
          }}
        />
      );
  }
};
