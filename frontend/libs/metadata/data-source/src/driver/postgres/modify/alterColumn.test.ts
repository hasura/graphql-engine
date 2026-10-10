import { getAlterColumnSql } from './alterColumn';

const table = { schema: 'public', name: 'Album' } as never;

const previous = {
  name: 'AlbumId',
  type: 'integer',
  nullable: false,
  default: `nextval('"Album_AlbumId_seq"'::regclass)`,
  unique: false,
};

describe('getAlterColumnSql (postgres family)', () => {
  it('returns no SQL when nothing changed', () => {
    expect(
      getAlterColumnSql({ table, previous, next: { ...previous } }),
    ).toEqual({ up: '', down: '' });
  });

  it('ignores surrounding whitespace in the default expression', () => {
    expect(
      getAlterColumnSql({
        table,
        previous,
        next: { ...previous, default: ` ${previous.default} ` },
      }).up,
    ).toBe('');
  });

  it('changes the type with a cast, and reverts it on the way down', () => {
    const { up, down } = getAlterColumnSql({
      table,
      previous,
      next: { ...previous, type: 'bigint' },
    });
    expect(up).toBe(
      'ALTER TABLE "public"."Album" ALTER COLUMN "AlbumId" TYPE bigint USING "AlbumId"::bigint;',
    );
    expect(down).toBe(
      'ALTER TABLE "public"."Album" ALTER COLUMN "AlbumId" TYPE integer USING "AlbumId"::integer;',
    );
  });

  it('drops a default and restores it on the way down', () => {
    const { up, down } = getAlterColumnSql({
      table,
      previous,
      next: { ...previous, default: '' },
    });
    expect(up).toBe(
      'ALTER TABLE "public"."Album" ALTER COLUMN "AlbumId" DROP DEFAULT;',
    );
    expect(down).toBe(
      `ALTER TABLE "public"."Album" ALTER COLUMN "AlbumId" SET DEFAULT nextval('"Album_AlbumId_seq"'::regclass);`,
    );
  });

  it('toggles nullability', () => {
    const { up, down } = getAlterColumnSql({
      table,
      previous,
      next: { ...previous, nullable: true },
    });
    expect(up).toBe(
      'ALTER TABLE "public"."Album" ALTER COLUMN "AlbumId" DROP NOT NULL;',
    );
    expect(down).toBe(
      'ALTER TABLE "public"."Album" ALTER COLUMN "AlbumId" SET NOT NULL;',
    );
  });

  it('adds a unique constraint with the default Postgres name', () => {
    const { up, down } = getAlterColumnSql({
      table,
      previous,
      next: { ...previous, unique: true },
    });
    expect(up).toBe(
      'ALTER TABLE "public"."Album" ADD CONSTRAINT "Album_AlbumId_key" UNIQUE ("AlbumId");',
    );
    expect(down).toBe(
      'ALTER TABLE "public"."Album" DROP CONSTRAINT "Album_AlbumId_key";',
    );
  });

  it('drops the existing unique constraint by its real name', () => {
    const { up, down } = getAlterColumnSql({
      table,
      previous: { ...previous, unique: true, uniqueConstraintName: 'uq_album' },
      next: { ...previous, unique: false },
    });
    expect(up).toBe('ALTER TABLE "public"."Album" DROP CONSTRAINT "uq_album";');
    expect(down).toBe(
      'ALTER TABLE "public"."Album" ADD CONSTRAINT "uq_album" UNIQUE ("AlbumId");',
    );
  });

  it('renames last on the way up and first on the way down', () => {
    const { up, down } = getAlterColumnSql({
      table,
      previous,
      next: { ...previous, name: 'album_id', nullable: true },
    });
    expect(up.split('\n')).toEqual([
      'ALTER TABLE "public"."Album" ALTER COLUMN "AlbumId" DROP NOT NULL;',
      'ALTER TABLE "public"."Album" RENAME COLUMN "AlbumId" TO "album_id";',
    ]);
    expect(down.split('\n')).toEqual([
      'ALTER TABLE "public"."Album" RENAME COLUMN "album_id" TO "AlbumId";',
      'ALTER TABLE "public"."Album" ALTER COLUMN "AlbumId" SET NOT NULL;',
    ]);
  });
});
