import { getAddColumnSql } from './addColumn';
import { buildDefaultStatement } from './createTable';

const table = { schema: 'public', name: 'Album' } as never;

describe('getAddColumnSql (postgres family)', () => {
  it('adds a nullable column and drops it on the way down', () => {
    expect(
      getAddColumnSql({
        table,
        column: { name: ' note ', type: 'text', nullable: true },
      }),
    ).toEqual({
      up: 'ALTER TABLE "public"."Album" ADD COLUMN "note" text;',
      down: 'ALTER TABLE "public"."Album" DROP COLUMN "note";',
    });
  });

  it('adds NOT NULL, UNIQUE and a quoted default', () => {
    const { up } = getAddColumnSql({
      table,
      column: {
        name: 'code',
        type: 'text',
        nullable: false,
        unique: true,
        default: { value: "it's" },
      },
    });
    expect(up).toBe(
      `ALTER TABLE "public"."Album" ADD COLUMN "code" text NOT NULL UNIQUE DEFAULT 'it''s';`,
    );
  });

  it('enables pgcrypto for a gen_random_uuid() default', () => {
    const { up } = getAddColumnSql({
      table,
      column: {
        name: 'uid',
        type: 'uuid',
        nullable: false,
        default: { value: 'gen_random_uuid()' },
      },
    });
    expect(up.split('\n')).toEqual([
      'CREATE EXTENSION IF NOT EXISTS pgcrypto;',
      'ALTER TABLE "public"."Album" ADD COLUMN "uid" uuid NOT NULL DEFAULT gen_random_uuid();',
    ]);
  });

  it('runs dependent SQL after adding and undoes it before dropping', () => {
    const { up, down } = getAddColumnSql({
      table,
      column: {
        name: 'updated_at',
        type: 'timestamptz',
        nullable: true,
        default: { value: 'now()' },
        dependentSQLGenerator: (_t, col) => ({
          upSql: `CREATE TRIGGER t_${col};`,
          downSql: `DROP TRIGGER t_${col};`,
        }),
      },
    });
    expect(up.split('\n')).toEqual([
      'ALTER TABLE "public"."Album" ADD COLUMN "updated_at" timestamptz DEFAULT now();',
      'CREATE TRIGGER t_updated_at;',
    ]);
    expect(down.split('\n')).toEqual([
      'DROP TRIGGER t_updated_at;',
      'ALTER TABLE "public"."Album" DROP COLUMN "updated_at";',
    ]);
  });
});

describe('buildDefaultStatement', () => {
  const col = (type: string, value: string) => ({
    name: 'c',
    type,
    default: { value },
  });

  it('keeps function defaults raw on repeated calls', () => {
    // Regression: a global regex alternated between matching and not.
    expect(buildDefaultStatement(col('timestamptz', 'now()'))).toBe(
      ' DEFAULT now()',
    );
    expect(buildDefaultStatement(col('timestamptz', 'now()'))).toBe(
      ' DEFAULT now()',
    );
  });

  it('keeps numeric values and SQL expressions raw', () => {
    expect(buildDefaultStatement(col('integer', '0'))).toBe(' DEFAULT 0');
    expect(buildDefaultStatement(col('text', "'a'::text"))).toBe(
      " DEFAULT 'a'::text",
    );
    expect(buildDefaultStatement(col('text', "'a'"))).toBe(" DEFAULT 'a'");
  });

  it('quotes plain text', () => {
    expect(buildDefaultStatement(col('text', 'hello'))).toBe(
      " DEFAULT 'hello'",
    );
  });
});
