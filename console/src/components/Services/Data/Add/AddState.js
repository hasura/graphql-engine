const defaultState = {
  tableName: null,
  tableComment: null,
  columns: [{ name: '', type: '', nullable: false, getDependentSql: null }],
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
      onUpdate: 'restrict',
      onDelete: 'restrict',
    },
  ],
  uniqueKeys: [[]],
  fkToggled: null,
  ongoingRequest: false,
  lastError: null,
  internalError: null,
  lastSuccess: null,
};

export default defaultState;
