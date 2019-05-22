export const frequentlyUsedColumns = [
  {
    name: 'id',
    type: 'serial',
    typeText: 'integer (auto-increment)',
    primary: true,
  },
  {
    name: 'id',
    type: 'uuid',
    typeText: 'UUID',
    primary: true,
    default: 'gen_random_uuid()',
  },
  {
    name: 'created_at',
    type: 'timestamp with time zone',
    typeText: 'timestamp',
    primary: false,
    default: 'now()',
  },
];

export const getFrequentlyUsedColumn = c => {
  const title = c.name;
  const subTitle = `${c.typeText}; ${
    c.default ? `default ${c.default};` : ''
  } ${c.primary ? 'primary key' : ''}`;
  return {
    title,
    subTitle,
  };
};
