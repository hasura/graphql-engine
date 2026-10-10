import { getDropColumnSql } from './dropColumn';

const table = { schema: 'public', name: 'Album' } as never;

describe('getDropColumnSql (postgres family)', () => {
  it('drops the column and re-adds its definition on the way down', () => {
    expect(
      getDropColumnSql({
        table,
        column: { name: 'Title', type: 'text', nullable: true, default: null },
      }),
    ).toEqual({
      up: 'ALTER TABLE "public"."Album" DROP COLUMN "Title";',
      down: 'ALTER TABLE "public"."Album" ADD COLUMN "Title" text;',
    });
  });

  it('restores NOT NULL only when a default can fill existing rows', () => {
    const withDefault = getDropColumnSql({
      table,
      column: {
        name: 'created_at',
        type: 'timestamptz',
        nullable: false,
        default: 'now()',
      },
    });
    expect(withDefault.down).toBe(
      'ALTER TABLE "public"."Album" ADD COLUMN "created_at" timestamptz DEFAULT now() NOT NULL;',
    );

    const withoutDefault = getDropColumnSql({
      table,
      column: { name: 'Title', type: 'text', nullable: false, default: null },
    });
    expect(withoutDefault.down).toBe(
      'ALTER TABLE "public"."Album" ADD COLUMN "Title" text;',
    );
  });

  it('recreates the primary key, unique keys and foreign keys involving it', () => {
    const { down } = getDropColumnSql({
      table,
      column: {
        name: 'ArtistId',
        type: 'integer',
        nullable: true,
        default: null,
      },
      primaryKey: {
        constraintName: 'PK_Album',
        columns: ['AlbumId', 'ArtistId'],
      },
      uniqueKeys: [{ constraintName: 'uq_artist', columns: ['ArtistId'] }],
      foreignKeys: [
        {
          name: 'FK_AlbumArtistId',
          from: { table, columns: ['ArtistId'] },
          to: {
            table: { schema: 'public', name: 'Artist' } as never,
            columns: ['ArtistId'],
          },
          onUpdate: 'cascade',
          onDelete: 'restrict',
        },
      ],
    });
    const lines = down.split('\n').map((l) => l.trim());
    expect(lines[0]).toBe(
      'ALTER TABLE "public"."Album" ADD COLUMN "ArtistId" integer;',
    );
    expect(down).toContain(
      'add constraint "PK_Album" primary key ("AlbumId", "ArtistId");',
    );
    expect(down).toContain('add constraint "uq_artist" unique ("ArtistId");');
    expect(down).toContain('add constraint "FK_AlbumArtistId"');
    expect(down).toContain('on update cascade on delete restrict;');
  });
});
