import { TableColumn } from '@/features/DataSource';

export type ModifyTableColumn = TableColumn & {
  config?: {
    custom_name?: string;
    comment?: string;
  };
};
