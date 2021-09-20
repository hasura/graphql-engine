import { dataSource } from '../../../../dataSources';

const defaultState = {
  tableName: null,
  tableComment: null,
  columns: [{ name: '', type: '', nullable: false }],
  primaryKeys: [''],
  foreignKeys: [
    {
      refSchemaName: '',
      refTableName: '',
      colMappings: [
        {
          column: '',
          refColumn: '',
        },
      ],
      onUpdate: dataSource?.violationActions?.[0],
      onDelete: dataSource?.violationActions?.[0],
    },
  ],
  uniqueKeys: [[]],
  fkToggled: null,
  ongoingRequest: false,
  lastError: null,
  internalError: null,
  lastSuccess: null,
  checkConstraints: [],
};

export default defaultState;
