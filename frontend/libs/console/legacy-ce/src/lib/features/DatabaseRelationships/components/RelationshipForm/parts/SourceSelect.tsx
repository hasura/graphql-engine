import { RemoteSchema, SupportedDriver, Table } from '@hasura/shared/types';
import './index.css';

export interface SourceOption {
  value:
    | {
        type: 'table';
        dataSourceName: string;
        driver: SupportedDriver;
        table: Table;
      }
    | { type: 'remoteSchema'; remoteSchema: RemoteSchema['name'] };
  label: string;
}
