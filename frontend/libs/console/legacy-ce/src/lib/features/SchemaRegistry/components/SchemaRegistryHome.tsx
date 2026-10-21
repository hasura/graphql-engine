import React, { useEffect, useRef, useState } from 'react';
import { useGetSchemaRegistryList } from '../hooks/useGetSchemaRegistryList';
import { Role, SchemaChangeCard, SchemaRegistryTag } from '../types';
import globals from '../../../Globals';
import {
  CapitalizeFirstLetter,
  getPublishTime,
  getSearchParam,
  schemaChangeListTransformFn,
  setSearchParam,
} from '../utils';
import {
  FaArrowLeft,
  FaArrowRight,
  FaChevronDown,
  FaChevronRight,
  FaPlusCircle,
} from 'react-icons/fa';
import { AddSchemaRegistryTagDialog } from './AddSchemaRegistryTagDialog';
import { SchemaTag } from './SchemaTag';
import { Analytics } from '@hasura/shared/analytics';
import { SchemaChangeDetails } from './SchemaChangeDetails';
import {
  ADMIN_ROLE,
  EMPTY_UUID_STRING,
  SCHEMA_LIST_FETCH_BATCH_SIZE,
} from '../constants';
import { Button } from '@hasura/shared/ui';
import { useGetURLSchema } from '../hooks/useGetURLSchema';
import { useNavigate } from 'react-router';

interface SchemaRegistryHomeProps {
  schemaId: string | undefined;
  v2Cursor: string;
  v2Count: number;
}

export const SchemaRegistryHome: React.FC<SchemaRegistryHomeProps> = (
  props,
) => {
  const { schemaId, v2Cursor, v2Count } = props;
  const projectID = globals.hasuraCloudProjectId || '';
  const navigate = useNavigate();
  const [pageNo, setPageno] = useState(() => {
    const page = Number(getSearchParam(window.location.search, 'page') ?? 0);
    return Number.isInteger(page) && page >= 0 ? page : 0;
  });
  const pendingPageChange = useRef(false);
  const schemaRoleID = schemaId ? schemaId : EMPTY_UUID_STRING;

  const fetchSchemasResponse = useGetSchemaRegistryList(
    projectID,
    pageNo,
    v2Count,
    v2Cursor,
  );

  const fetchURLSchemaResponse = useGetURLSchema(schemaRoleID);

  const { kind } = fetchSchemasResponse;

  let latestAdminRoleID: string | null = null;
  if (kind === 'success') {
    const schemaList = schemaChangeListTransformFn(
      fetchSchemasResponse.response,
    );
    latestAdminRoleID =
      schemaList[0]?.roles?.find((item) => item.role === ADMIN_ROLE)?.id ||
      null;
  }
  const handleNext = (i: number) => {
    pendingPageChange.current = true;
    setPageno(pageNo + i);
  };
  const handlePrev = () => {
    pendingPageChange.current = true;
    setPageno(pageNo - 1);
  };

  // selectedRole ID determines what should render in SchemaChangeDetails.
  // It is fully derived from the URL: a specific role id when the URL points at
  // one, otherwise null until the effects below redirect to the default role.
  const selectedRoleID =
    schemaRoleID !== EMPTY_UUID_STRING ? schemaRoleID : null;

  // `navigate` changes identity on every location change (non-data router), so
  // read it from a ref: otherwise selecting another role (a URL change) would
  // re-run the effects below and bounce back to the latest admin role.
  const navigateRef = useRef(navigate);
  useEffect(() => {
    navigateRef.current = navigate;
  });

  // Preserve direct links and the user's selected role during background refreshes.
  // Only a deliberate page change (or an empty URL) selects the page's default role.
  useEffect(() => {
    if (kind !== 'success') return;
    if (
      latestAdminRoleID &&
      (schemaRoleID === EMPTY_UUID_STRING || pendingPageChange.current)
    ) {
      pendingPageChange.current = false;
      navigateRef.current(`/api/schema-registry/${latestAdminRoleID}`);
    }
    setSearchParam(pageNo);
  }, [kind, schemaRoleID, latestAdminRoleID, pageNo]);

  switch (kind) {
    case 'loading':
      return <p role="status">Loading published schemas…</p>;
    case 'error':
      return (
        <p role="alert">
          Unable to load published schemas: {fetchSchemasResponse.message}
        </p>
      );
    case 'success': {
      const URLSchemaCard =
        fetchURLSchemaResponse.kind === 'success'
          ? schemaChangeListTransformFn(fetchURLSchemaResponse.response)
          : [];
      const schemaList = schemaChangeListTransformFn(
        fetchSchemasResponse.response,
      );
      return (
        <SchemaRegistryLayout
          sidebar={
            <SchemaChangeList
              schemas={schemaList}
              urlSchemaCard={URLSchemaCard}
              pageNumber={pageNo}
              totalCount={fetchSchemasResponse.totalCount}
              selectedRoleID={selectedRoleID}
              handlePrev={handlePrev}
              handleNext={handleNext}
            />
          }
        >
          {selectedRoleID && <SchemaChangeDetails schemaId={selectedRoleID} />}
        </SchemaRegistryLayout>
      );
    }
  }
};

export const SchemaRegistryLayout = ({
  sidebar,
  children,
}: {
  sidebar: React.ReactNode;
  children: React.ReactNode;
}) => (
  <div className="@container w-full min-w-0">
    <div className="grid min-w-0 grid-cols-1 items-start gap-5 @min-[56rem]:grid-cols-[18rem_minmax(0,1fr)]">
      <aside aria-label="Published schemas" className="min-w-0">
        {sidebar}
      </aside>
      <div className="min-w-0">{children}</div>
    </div>
  </div>
);

export const SchemaChangeList: React.FC<{
  schemas: SchemaChangeCard[];
  urlSchemaCard: SchemaChangeCard[];
  pageNumber: number;
  totalCount: number;
  selectedRoleID: string | null;
  handlePrev: () => void;
  handleNext: (i: number) => void;
}> = ({
  schemas,
  urlSchemaCard,
  pageNumber,
  totalCount,
  selectedRoleID,
  handlePrev,
  handleNext,
}) => {
  const [openSchemaCardIndex, setIsOpenSchemaCardIndex] =
    useState<React.Key>(0);
  const pageCount = Math.max(
    1,
    Math.ceil(totalCount / SCHEMA_LIST_FETCH_BATCH_SIZE),
  );
  const isLastPage = pageNumber + 1 >= pageCount;
  // A deep-linked schema can also be present on the current page.
  const schemaList = [...urlSchemaCard, ...schemas].filter(
    (schema, index, all) =>
      all.findIndex((item) => item.id === schema.id) === index,
  );

  return (
    <section
      className="rounded-lg border"
      style={{
        background: 'var(--color-panel-solid)',
        borderColor: 'var(--gray-6)',
        color: 'var(--gray-12)',
      }}
    >
      <h2
        className="border-b px-4 py-3 text-sm font-semibold"
        style={{ borderColor: 'var(--gray-6)' }}
      >
        Published schemas
      </h2>
      {schemaList.length ? (
        <>
          <div className="max-h-[60vh] overflow-y-auto p-2">
            {schemaList.map((schema, index) => (
              <SchemaCard
                key={schema.id}
                cardKey={index}
                openSchemaCardIndex={openSchemaCardIndex}
                selectedRoleID={selectedRoleID}
                createdAt={schema.created_at}
                roles={schema.roles}
                entryHash={schema.entry_hash}
                tags={schema.tags}
                handleClick={() => setIsOpenSchemaCardIndex(index)}
              />
            ))}
          </div>
          <nav
            aria-label="Schema history pagination"
            className="flex flex-wrap items-center justify-between gap-2 border-t p-3"
            style={{ borderColor: 'var(--gray-6)' }}
          >
            <Button
              size="sm"
              variant="outline"
              color="gray"
              leftIcon={FaArrowLeft}
              disabled={pageNumber === 0}
              onClick={() => {
                handlePrev();
                setIsOpenSchemaCardIndex(0);
              }}
            >
              Prev
            </Button>
            <span className="text-xs" aria-current="page">
              Page {pageNumber + 1} of {pageCount}
            </span>
            <Button
              size="sm"
              variant="outline"
              color="gray"
              rightIcon={FaArrowRight}
              disabled={isLastPage}
              onClick={() => {
                handleNext(1);
                setIsOpenSchemaCardIndex(0);
              }}
            >
              Next
            </Button>
          </nav>
        </>
      ) : (
        <p
          className="p-4 text-sm"
          data-test="label-no-domain-found"
          style={{ color: 'var(--gray-11)' }}
        >
          No schemas published to the schema registry yet.
        </p>
      )}
    </section>
  );
};

const SchemaCard: React.FC<{
  cardKey: React.Key;
  createdAt: string;
  roles: Role[];
  entryHash: string;
  tags: SchemaRegistryTag[];
  openSchemaCardIndex: React.Key;
  selectedRoleID: string | null;
  handleClick: () => void;
}> = ({
  cardKey,
  createdAt,
  roles,
  entryHash,
  tags,
  openSchemaCardIndex,
  selectedRoleID,
  handleClick,
}) => {
  const navigate = useNavigate();
  const [isTagModalOpen, setIsTagModalOpen] = useState(false);
  const isSelected = roles.some((role) => role.id === selectedRoleID);
  const isCurrentCardOpen = cardKey === openSchemaCardIndex || isSelected;
  const [tagsList, setTagsList] = useState(tags);
  const [prevTags, setPrevTags] = useState(tags);
  if (tags !== prevTags) {
    setPrevTags(tags);
    setTagsList(tags);
  }
  const onRemoveTag = (id: string) =>
    setTagsList((list) => list.filter((tag) => tag.id !== id));
  // Never mutate the cached response when placing the admin role first.
  const sortedRoles = [...roles].sort(
    (a, b) => Number(b.role === ADMIN_ROLE) - Number(a.role === ADMIN_ROLE),
  );
  const showAllRoles = isCurrentCardOpen || roles.length <= 3;
  const visibleRoles = showAllRoles ? sortedRoles : sortedRoles.slice(0, 3);

  return (
    <article
      className="rounded-md p-2 mb-2"
      style={{ background: isCurrentCardOpen ? 'var(--gray-3)' : undefined }}
    >
      <button
        type="button"
        className="w-full text-left text-sm font-medium py-1 rounded focus-visible:outline-2"
        aria-label={`View schema published ${getPublishTime(createdAt)}`}
        onClick={() => {
          if (!isSelected && sortedRoles[0]) {
            navigate(`/api/schema-registry/${sortedRoles[0].id}`);
          }
          handleClick();
        }}
      >
        <time dateTime={createdAt}>{getPublishTime(createdAt)}</time>
      </button>
      <div className="flex flex-wrap items-center gap-2 py-2">
        {tagsList.map((tag) => (
          <SchemaTag
            key={tag.id}
            schemaRegistryTag={tag}
            onRemove={onRemoveTag}
          />
        ))}
        <Analytics
          name="schema-registry-add-tag-btn"
          passHtmlAttributesToChildren
        >
          <button
            type="button"
            aria-label={`Add tag to schema published ${getPublishTime(createdAt)}`}
            title="Add a tag"
            className="p-1 rounded focus-visible:outline-2"
            onClick={() => setIsTagModalOpen(true)}
          >
            <FaPlusCircle aria-hidden="true" />
          </button>
        </Analytics>
      </div>
      {isTagModalOpen && (
        <AddSchemaRegistryTagDialog
          tagsList={tagsList}
          setTagsList={setTagsList}
          entryHash={entryHash}
          onClose={() => setIsTagModalOpen(false)}
        />
      )}
      <p className="text-xs mb-2" style={{ color: 'var(--gray-11)' }}>
        {roles.length} {roles.length === 1 ? 'role' : 'roles'}
      </p>
      <div className="flex flex-col gap-1">
        {visibleRoles.map((item) => (
          <Analytics
            key={item.id}
            name="schema-registry-schema-change-card"
            passHtmlAttributesToChildren
          >
            <button
              type="button"
              aria-current={item.id === selectedRoleID ? 'true' : undefined}
              className="flex w-full min-w-0 items-center justify-between gap-2 rounded px-2 py-2 text-left text-sm focus-visible:outline-2"
              style={{
                background:
                  item.id === selectedRoleID ? 'var(--accent-4)' : undefined,
                color:
                  item.id === selectedRoleID
                    ? 'var(--accent-12)'
                    : 'var(--gray-12)',
                fontWeight: item.id === selectedRoleID ? 600 : undefined,
              }}
              onClick={() => {
                handleClick();
                navigate(`/api/schema-registry/${item.id}`);
              }}
            >
              <span className="break-all">
                {CapitalizeFirstLetter(item.role)}
              </span>
              <FaChevronRight className="shrink-0" aria-hidden="true" />
            </button>
          </Analytics>
        ))}
        {!showAllRoles && (
          <Analytics
            name="schema-registry-see-more-roles-btn"
            passHtmlAttributesToChildren
          >
            <button
              type="button"
              className="flex items-center gap-2 px-2 py-2 text-sm"
              onClick={handleClick}
            >
              Show {roles.length - visibleRoles.length} more roles{' '}
              <FaChevronDown aria-hidden="true" />
            </button>
          </Analytics>
        )}
      </div>
    </article>
  );
};
