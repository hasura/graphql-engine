import { getCreateTriggerSql } from './trigger';

const table = { schema: 'public', name: 'Album' } as never;

const base = {
  table,
  triggerName: 'album_audit',
  timing: 'AFTER' as const,
  events: ['INSERT' as const, 'UPDATE' as const],
  forEach: 'ROW' as const,
  function: { schema: 'public', name: 'audit_fn' },
};

describe('getCreateTriggerSql (postgres family)', () => {
  it('creates a trigger on an existing function', () => {
    expect(getCreateTriggerSql('postgres', base)).toEqual({
      up: `CREATE TRIGGER "album_audit"
AFTER INSERT OR UPDATE ON "public"."Album"
FOR EACH ROW
EXECUTE PROCEDURE "public"."audit_fn"();`,
      down: 'drop trigger "album_audit" on "public"."Album";',
    });
  });

  it('adds a WHEN condition', () => {
    const { up } = getCreateTriggerSql('postgres', {
      ...base,
      condition: ' OLD.* IS DISTINCT FROM NEW.* ',
    });
    expect(up).toContain(
      'FOR EACH ROW\nWHEN (OLD.* IS DISTINCT FROM NEW.*)\nEXECUTE',
    );
  });

  it('uses EXECUTE FUNCTION on CockroachDB', () => {
    expect(getCreateTriggerSql('cockroach', base).up).toContain(
      'EXECUTE FUNCTION "public"."audit_fn"();',
    );
  });

  it('creates the function first and drops it after the trigger', () => {
    const { up, down } = getCreateTriggerSql('postgres', {
      ...base,
      newFunctionBody: '\nBEGIN\n  RETURN NEW;\nEND;\n',
    });
    expect(up).toBe(`CREATE FUNCTION "public"."audit_fn"()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN NEW;
END;
$$;
CREATE TRIGGER "album_audit"
AFTER INSERT OR UPDATE ON "public"."Album"
FOR EACH ROW
EXECUTE PROCEDURE "public"."audit_fn"();`);
    expect(down).toBe(
      'drop trigger "album_audit" on "public"."Album";\nDROP FUNCTION "public"."audit_fn"();',
    );
  });
});
