import { TableColumn } from '../../DataSource';

export type ModifyTableColumn = TableColumn & {
  config?: {
    custom_name?: string;
    comment?: string;
  };
};
