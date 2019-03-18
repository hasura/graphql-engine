const defaultState = {
  tableName: 'test_table',
  tableComment: null,
  columns: [
    { name: 'id', type: 'integer', nullable: false },
    { name: 'text', type: 'text', nullable: false },
  ],
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
  ongoingRequest: false,
  lastError: null,
  internalError: null,
  lastSuccess: null,
};

export default defaultState;
