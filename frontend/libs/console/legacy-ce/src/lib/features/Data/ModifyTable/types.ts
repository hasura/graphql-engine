import { TableColumn } from '@hasura/metadata/data-source';
import { MetadataTable, Source } from '@hasura/shared/types';

export type ModifyTableColumn = TableColumn & {
  config?: {
    custom_name?: string;
    comment?: string;
  };
};

export type ModifyTableProps = {
  source: Source;
  table: MetadataTable;
  isView: boolean;
};
