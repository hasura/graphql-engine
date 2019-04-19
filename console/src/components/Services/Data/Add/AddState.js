const defaultState = {
  tableName: null,
  tableComment: null,
  columns: [{ name: '', type: '', nullable: false }],
  primaryKeys: [''],
  foreignKeys: [
    {
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
  fkToggled: null,
  ongoingRequest: false,
  lastError: null,
  internalError: null,
  lastSuccess: null,
};

export default defaultState;
