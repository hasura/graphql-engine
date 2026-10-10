import {
  addTableSchema,
  addTableDefaultValues,
  formValuesToCreateTableArgs,
  frequentlyUsedColumnToFormColumn,
  AddTableFormValues,
} from './schema';

const col = (
  overrides: Partial<AddTableFormValues['columns'][number]>,
): AddTableFormValues['columns'][number] => ({
  name: '',
  type: '',
  nullable: false,
  array: false,
  unique: false,
  default: '',
  isPrimaryKey: false,
  ...overrides,
});

const baseValues: AddTableFormValues = {
  name: 'users',
  schema: 'public',
  comment: '',
  checkConstraints: [],
  foreignKeys: [],
  uniqueKeys: [],
  columns: [
    col({ name: 'id', type: 'uuid', isPrimaryKey: true }),
    col({ name: 'email', type: 'text', nullable: true }),
  ],
};

describe('addTableSchema validation', () => {
  it('accepts a valid table with a named column and a primary key', () => {
    expect(addTableSchema.safeParse(baseValues).success).toBe(true);
  });

  it('rejects an empty table name', () => {
    expect(addTableSchema.safeParse({ ...baseValues, name: '' }).success).toBe(
      false,
    );
  });

  it('rejects a missing schema', () => {
    expect(
      addTableSchema.safeParse({ ...baseValues, schema: '' }).success,
    ).toBe(false);
  });

  it('rejects when there is no named column', () => {
    expect(
      addTableSchema.safeParse({ ...baseValues, columns: [col({})] }).success,
    ).toBe(false);
  });

  it('rejects duplicate column names', () => {
    expect(
      addTableSchema.safeParse({
        ...baseValues,
        columns: [
          col({ name: 'id', type: 'uuid', isPrimaryKey: true }),
          col({ name: 'id', type: 'text' }),
        ],
      }).success,
    ).toBe(false);
  });

  it('rejects when no primary key is selected', () => {
    expect(
      addTableSchema.safeParse({
        ...baseValues,
        columns: baseValues.columns.map((c) => ({ ...c, isPrimaryKey: false })),
      }).success,
    ).toBe(false);
  });

  it('rejects a foreign key the user started but left incomplete', () => {
    expect(
      addTableSchema.safeParse({
        ...baseValues,
        foreignKeys: [
          {
            referenceTable: JSON.stringify({ name: 'orgs', schema: 'public' }),
            columnMappings: [{ from: 'org_id', to: '' }],
            onUpdate: 'restrict',
            onDelete: 'restrict',
          },
        ],
      }).success,
    ).toBe(false);
  });

  it('rejects a composite unique key row with no columns selected', () => {
    expect(
      addTableSchema.safeParse({
        ...baseValues,
        uniqueKeys: [{ columns: [] }],
      }).success,
    ).toBe(false);
  });

  it('allows an untouched (empty) foreign key row', () => {
    expect(
      addTableSchema.safeParse({
        ...baseValues,
        foreignKeys: [
          {
            referenceTable: '',
            columnMappings: [{ from: '', to: '' }],
            onUpdate: 'restrict',
            onDelete: 'restrict',
          },
        ],
      }).success,
    ).toBe(true);
  });
});

describe('formValuesToCreateTableArgs', () => {
  it('maps columns, primary keys and comment into ModifyTableArgs', () => {
    const args = formValuesToCreateTableArgs({
      ...baseValues,
      comment: 'app users',
      columns: [
        col({
          name: 'id',
          type: 'uuid',
          default: 'gen_random_uuid()',
          isPrimaryKey: true,
        }),
        col({ name: 'email', type: 'text', nullable: true }),
      ],
    });

    expect(args.table).toEqual({ name: 'users', schema: 'public' });
    expect(args.columns).toEqual([
      {
        name: 'id',
        type: 'uuid',
        nullable: false,
        default: { value: 'gen_random_uuid()' },
      },
      { name: 'email', type: 'text', nullable: true },
    ]);
    expect(args.primaryKeys).toEqual([0]);
    expect(args.tableComment).toBe('app users');
  });

  it('suffixes array columns with []', () => {
    const args = formValuesToCreateTableArgs({
      ...baseValues,
      columns: [
        col({ name: 'id', type: 'int', isPrimaryKey: true }),
        col({ name: 'tags', type: 'text', array: true, nullable: true }),
      ],
    });
    expect(args.columns[1]).toMatchObject({ name: 'tags', type: 'text[]' });
  });

  it('does not double-suffix a type that is already an array', () => {
    const args = formValuesToCreateTableArgs({
      ...baseValues,
      columns: [
        col({ name: 'id', type: 'int', isPrimaryKey: true }),
        col({ name: 'tags', type: 'text[]', array: true }),
      ],
    });
    expect(args.columns[1]).toMatchObject({ name: 'tags', type: 'text[]' });
  });

  it('maps per-column unique flags into single-column unique keys (excluding PK)', () => {
    const args = formValuesToCreateTableArgs({
      ...baseValues,
      columns: [
        col({ name: 'id', type: 'uuid', isPrimaryKey: true, unique: true }),
        col({ name: 'email', type: 'text', unique: true }),
        col({ name: 'name', type: 'text' }),
      ],
    });
    // PK column is excluded; only `email` (index 1) becomes a unique key.
    expect(args.uniqueKeys).toEqual([[1]]);
  });

  it('maps composite unique keys (column names -> indices)', () => {
    const args = formValuesToCreateTableArgs({
      ...baseValues,
      columns: [
        col({ name: 'id', type: 'uuid', isPrimaryKey: true }),
        col({ name: 'first', type: 'text' }),
        col({ name: 'last', type: 'text' }),
      ],
      uniqueKeys: [{ columns: ['first', 'last'] }],
    });
    expect(args.uniqueKeys).toEqual([[1, 2]]);
  });

  it('de-duplicates a per-column unique that equals a composite unique key', () => {
    const args = formValuesToCreateTableArgs({
      ...baseValues,
      columns: [
        col({ name: 'id', type: 'uuid', isPrimaryKey: true }),
        col({ name: 'email', type: 'text', unique: true }),
      ],
      // same single-column key expressed compositely -> must not duplicate
      uniqueKeys: [{ columns: ['email'] }],
    });
    expect(args.uniqueKeys).toEqual([[1]]);
  });

  it('maps foreign keys, passing the referenced table through and dropping incomplete mappings', () => {
    const args = formValuesToCreateTableArgs({
      ...baseValues,
      foreignKeys: [
        {
          referenceTable: { name: 'orgs', schema: 'public' },
          columnMappings: [
            { from: 'org_id', to: 'id' },
            { from: '', to: '' },
          ],
          onUpdate: 'cascade',
          onDelete: 'restrict',
        },
        // incomplete FK — dropped
        { referenceTable: '', columnMappings: [{ from: '', to: '' }] },
      ],
    });

    expect(args.foreignKeys).toEqual([
      {
        referenceTable: { name: 'orgs', schema: 'public' },
        columnMappings: [{ from: 'org_id', to: 'id' }],
        onUpdate: 'cascade',
        onDelete: 'restrict',
      },
    ]);
  });

  it('filters out empty (unnamed) columns and keeps PK indices aligned', () => {
    const args = formValuesToCreateTableArgs({
      ...baseValues,
      columns: [col({}), col({ name: 'id', type: 'int', isPrimaryKey: true })],
    });
    expect(args.columns.map((c) => c.name)).toEqual(['id']);
    expect(args.primaryKeys).toEqual([0]);
  });

  it('omits tableComment when the comment is blank', () => {
    expect('tableComment' in formValuesToCreateTableArgs(baseValues)).toBe(
      false,
    );
  });

  it('keeps only fully-filled check constraints', () => {
    const args = formValuesToCreateTableArgs({
      ...baseValues,
      checkConstraints: [
        { name: 'positive', check: 'age > 0' },
        { name: '', check: '' },
        { name: 'partial', check: '' },
      ],
    });
    expect(args.checkConstraints).toEqual([
      { name: 'positive', check: 'age > 0' },
    ]);
  });
});

describe('frequentlyUsedColumnToFormColumn', () => {
  it('converts a preset preserving name/type/default/primary', () => {
    expect(
      frequentlyUsedColumnToFormColumn({
        name: 'id',
        type: 'uuid',
        default: 'gen_random_uuid()',
        primary: true,
      }),
    ).toEqual({
      name: 'id',
      type: 'uuid',
      nullable: false,
      array: false,
      unique: false,
      default: 'gen_random_uuid()',
      isPrimaryKey: true,
    });
  });

  it('defaults missing default/primary to empty/false', () => {
    const result = frequentlyUsedColumnToFormColumn({
      name: 'created_at',
      type: 'timestamptz',
    });
    expect(result.default).toBe('');
    expect(result.isPrimaryKey).toBe(false);
  });
});

describe('addTableDefaultValues', () => {
  it('starts with one empty column and no constraints/foreign keys', () => {
    expect(addTableDefaultValues.columns).toHaveLength(1);
    expect(addTableDefaultValues.checkConstraints).toHaveLength(0);
    expect(addTableDefaultValues.foreignKeys).toHaveLength(0);
  });
});
