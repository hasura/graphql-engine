import { useMemo, useState } from 'react';
import LZString from 'lz-string';
import { useHref } from 'react-router';
import { useGetSchema } from '../hooks/useGetSchema';
import { Tabs, Input, Button, AceEditor } from '@hasura/shared/ui';
import { schemaTransformFn, getPublishTime } from '../utils';
import { RoleBasedSchema, Schema } from '../types';
import { FaSearch, FaShareAlt } from 'react-icons/fa';
import { Analytics } from '@hasura/shared/analytics';

interface SchemaChangeDetailsProps {
  schemaId: string;
}

export const SchemaChangeDetails = ({ schemaId }: SchemaChangeDetailsProps) => {
  const response = useGetSchema(schemaId);

  switch (response.kind) {
    case 'loading':
      return <p role="status">Loading schema details…</p>;
    case 'error':
      return <p role="alert">Unable to load schema: {response.message}</p>;
    case 'success': {
      const schema = schemaTransformFn(response.response);
      return schema ? (
        <SchemaDetailsPanel key={schemaId} schema={schema} />
      ) : (
        <p role="status">Schema not found. Select another published schema.</p>
      );
    }
  }
};

const changeLevels = [
  { level: 'BREAKING', label: 'Breaking', color: 'var(--red-11)' },
  { level: 'DANGEROUS', label: 'Dangerous', color: 'var(--amber-11)' },
  { level: 'NON_BREAKING', label: 'Safe', color: 'var(--green-11)' },
] as const;

/** The fetched detail view, kept separate so all comparison states can be previewed. */
export const SchemaDetailsPanel = ({ schema }: { schema: Schema }) => {
  const [tabState, setTabState] = useState('changes');
  const [copyState, setCopyState] = useState<'idle' | 'copied' | 'error'>(
    'idle',
  );
  const roleBasedSchema = schema.roleBasedSchemas[0];
  const sharePath = useHref(`/api/schema-registry/${roleBasedSchema.id}`);
  const { changes, role } = roleBasedSchema;

  const onCopy = async () => {
    try {
      await navigator.clipboard.writeText(
        new URL(sharePath, window.location.origin).toString(),
      );
      setCopyState('copied');
    } catch {
      setCopyState('error');
    }
  };

  return (
    <section
      aria-label="Schema details"
      className="min-w-0 rounded-lg border"
      style={{
        background: 'var(--color-panel-solid)',
        borderColor: 'var(--gray-6)',
        color: 'var(--gray-12)',
      }}
    >
      <div className="flex flex-wrap items-start justify-between gap-4 p-5">
        <div className="min-w-0">
          <p className="text-sm mb-1" style={{ color: 'var(--gray-11)' }}>
            Schema for role
          </p>
          <h2 className="text-lg font-semibold break-all">{role}</h2>
        </div>
        <div className="flex flex-col items-end gap-1">
          <Analytics
            name="schema-registry-share-btn"
            passHtmlAttributesToChildren
          >
            <Button
              variant="outline"
              color="gray"
              leftIcon={FaShareAlt}
              onClick={onCopy}
            >
              {copyState === 'copied' ? 'Link copied' : 'Share schema'}
            </Button>
          </Analytics>
          <span
            role="status"
            className="text-sm"
            style={{ color: 'var(--gray-11)' }}
          >
            {copyState === 'copied' && 'Schema link copied to clipboard.'}
            {copyState === 'error' && 'Unable to copy link. Please try again.'}
          </span>
        </div>
      </div>

      <dl
        aria-label="Change summary"
        className="grid grid-cols-[repeat(auto-fit,minmax(7rem,1fr))] gap-3 px-5 pb-5"
      >
        {[
          ...changeLevels.map(({ level, label, color }) => ({
            label,
            color,
            count: changes?.filter(
              (change) => change.criticality.level === level,
            ).length,
          })),
          { label: 'Total', color: 'var(--gray-12)', count: changes?.length },
        ].map(({ label, color, count }) => (
          <div
            key={label}
            className="min-w-0 rounded-md p-3"
            style={{ background: 'var(--gray-3)' }}
          >
            <dt
              className="text-sm font-medium"
              style={{ color: 'var(--gray-12)' }}
            >
              {label}
            </dt>
            <dd className="mt-1 text-xl font-semibold" style={{ color }}>
              {count ?? <span className="text-sm">Unavailable</span>}
            </dd>
          </div>
        ))}
      </dl>

      <dl
        className="flex flex-wrap gap-x-8 gap-y-3 border-y px-5 py-4 text-sm"
        style={{ borderColor: 'var(--gray-6)' }}
      >
        <div>
          <dt
            className="flex items-center gap-1 mb-1"
            style={{ color: 'var(--gray-11)' }}
          >
            Published
          </dt>
          <dd>
            <time dateTime={schema.created_at}>
              {getPublishTime(schema.created_at)}
            </time>
          </dd>
        </div>
        <div className="min-w-0 flex-1 basis-64">
          <dt
            className="flex items-center gap-1 mb-1"
            style={{ color: 'var(--gray-11)' }}
          >
            Schema hash
          </dt>
          <dd className="font-mono break-all">{roleBasedSchema.hash}</dd>
        </div>
      </dl>

      <div className="min-w-0 px-5 pb-5 pt-3">
        <Tabs
          value={tabState}
          onValueChange={setTabState}
          items={[
            {
              value: 'changes',
              label: 'Changes',
              content: <ChangesView changes={changes} role={role} />,
            },
            {
              value: 'graphql',
              label: 'GraphQL',
              content: <SchemaView schema={roleBasedSchema.raw} />,
            },
          ]}
        />
      </div>
    </section>
  );
};

const SchemaView = ({ schema }: { schema: string }) => (
  <div className="min-w-0 py-4">
    <AceEditor
      mode="graphqlschema"
      fontSize={14}
      width="100%"
      name="schema-registry-schema-modal-view-schema"
      value={LZString.decompressFromBase64(schema) ?? ''}
      disabled
      editorProps={{ $blockScrolling: true }}
      setOptions={{ useWorker: false }}
    />
  </div>
);

const ChangesView = ({
  changes,
  role,
}: {
  changes: RoleBasedSchema['changes'];
  role: string;
}) => {
  const [searchText, setSearchText] = useState('');
  const [selectedChangeLevel, setSelectedChangeLevel] = useState('all');
  const filteredChanges = useMemo(
    () =>
      changes?.filter(
        (change) =>
          (selectedChangeLevel === 'all' ||
            change.criticality.level === selectedChangeLevel) &&
          change.message
            .toLowerCase()
            .includes(searchText.trim().toLowerCase()),
      ) ?? [],
    [changes, searchText, selectedChangeLevel],
  );
  const clearFilters = () => {
    setSearchText('');
    setSelectedChangeLevel('all');
  };

  if (!changes || !changes.length) {
    return (
      <div className="rounded-md py-8 text-center" role="status">
        <h3 className="font-semibold mb-2">
          {changes ? 'No changes detected' : 'Comparison unavailable'}
        </h3>
        <p className="text-sm" style={{ color: 'var(--gray-11)' }}>
          {changes ? (
            <>
              This schema matches the previous schema for role{' '}
              <strong className="break-all">{role}</strong>.
            </>
          ) : (
            <>
              Changes could not be computed for role{' '}
              <strong className="break-all">{role}</strong>. A previous schema
              may not exist, or a schema may contain errors.
            </>
          )}
        </p>
      </div>
    );
  }

  return (
    <div className="py-4">
      <p className="text-sm mb-4" style={{ color: 'var(--gray-11)' }}>
        Compared with the previous schema for role{' '}
        <strong className="break-all">{role}</strong>.
      </p>
      <div className="flex flex-wrap items-end gap-3 mb-4">
        <label className="flex flex-col gap-1 text-sm">
          Change severity
          <Analytics
            name="schema-registry-schema-change-details-filter"
            passHtmlAttributesToChildren
          >
            <select
              className="rounded-md border px-3 py-2"
              style={{
                background: 'var(--color-panel-solid)',
                borderColor: 'var(--gray-7)',
              }}
              value={selectedChangeLevel}
              onChange={(event) => setSelectedChangeLevel(event.target.value)}
            >
              <option value="all">All changes</option>
              {changeLevels.map(({ level, label }) => (
                <option key={level} value={level}>
                  {label}
                </option>
              ))}
            </select>
          </Analytics>
        </label>
        <label className="flex min-w-0 flex-1 basis-56 flex-col gap-1 text-sm">
          Search changes
          <Analytics
            name="schema-registry-schema-change-details-search"
            passHtmlAttributesToChildren
          >
            <Input
              type="search"
              placeholder="Search change descriptions"
              name="search"
              icon={FaSearch}
              value={searchText}
              onChange={(event) => setSearchText(event.target.value)}
            />
          </Analytics>
        </label>
        {(searchText || selectedChangeLevel !== 'all') && (
          <Button variant="ghost" color="gray" onClick={clearFilters}>
            Clear filters
          </Button>
        )}
      </div>
      <p
        role="status"
        className="text-sm mb-3"
        style={{ color: 'var(--gray-11)' }}
      >
        Showing {filteredChanges.length} of {changes.length} changes
      </p>
      {!filteredChanges.length ? (
        <div className="py-6 text-center">
          <h3 className="font-semibold">No matching changes</h3>
          <p className="text-sm mt-1" style={{ color: 'var(--gray-11)' }}>
            Try another search or clear the filters to see all changes.
          </p>
        </div>
      ) : (
        changeLevels.map(({ level, label, color }) => {
          const group = filteredChanges.filter(
            (change) => change.criticality.level === level,
          );
          return group.length ? (
            <section
              key={level}
              aria-label={`${label} changes`}
              className="mb-4"
            >
              <h3 className="text-sm font-semibold mb-2" style={{ color }}>
                {label} ({group.length})
              </h3>
              <ul
                className="rounded-md border divide-y"
                style={{ borderColor: 'var(--gray-6)' }}
              >
                {group.map((change, index) => (
                  <li
                    key={`${change.type}-${change.path}-${index}`}
                    className="px-3 py-3 text-sm break-words"
                    style={{ borderColor: 'var(--gray-6)' }}
                  >
                    {change.message}
                  </li>
                ))}
              </ul>
            </section>
          ) : null;
        })
      )}
    </div>
  );
};
