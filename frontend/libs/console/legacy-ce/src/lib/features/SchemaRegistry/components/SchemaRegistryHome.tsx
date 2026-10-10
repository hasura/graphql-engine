import React, { useEffect, useState } from 'react';
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
import { IconTooltip } from '@hasura/shared/ui';
import { useGetURLSchema } from '../hooks/useGetURLSchema';
import { useNavigate } from 'react-router';
import { Flex } from '@radix-ui/themes';

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
  const [pageNo, setPageno] = useState(
    parseInt(getSearchParam(window.location.search, 'page') || '0'),
  );
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
  //selectedRole ID determines what should render in SchemaChangeDetails
  const [selectedRoleID, setSelectedRoleID] = useState<string | null>(null);
  const handleNext = (i: number) => {
    setPageno(pageNo + i);
  };
  const handlePrev = () => {
    setPageno(pageNo - 1);
  };

  //setting the selected role ID as admin role ID by default
  useEffect(() => {
    if (schemaRoleID === EMPTY_UUID_STRING) {
      if (latestAdminRoleID) {
        setSelectedRoleID(latestAdminRoleID);
        navigate(`/api/schema-registry/${latestAdminRoleID}`);
      }
    } else {
      setSelectedRoleID(schemaRoleID);
    }
  }, [schemaRoleID]);

  useEffect(() => {
    if (latestAdminRoleID) {
      setSelectedRoleID(latestAdminRoleID);
      navigate(`/api/schema-registry/${latestAdminRoleID}`);
    }
    setSearchParam(pageNo);
  }, [latestAdminRoleID, pageNo]);

  switch (kind) {
    case 'loading':
      return <p>Loading...</p>;
    case 'error':
      return <p>Error: {fetchSchemasResponse.message}</p>;
    case 'success': {
      let URLSchemaCard: SchemaChangeCard[] = [];
      if (
        fetchURLSchemaResponse.kind === 'success' &&
        fetchURLSchemaResponse.response[0]
      ) {
        const URLAdminRoleID = URLSchemaCard[0]?.roles.find(
          (item) => item.role === ADMIN_ROLE,
        )?.id;

        if (URLAdminRoleID !== latestAdminRoleID) {
          URLSchemaCard = schemaChangeListTransformFn(
            fetchURLSchemaResponse.response,
          );
        }
      }
      const schemaList = schemaChangeListTransformFn(
        fetchSchemasResponse.response,
      );
      return (
        <Flex justify="center" className="w-[80%]">
          <div className="w-[20%]">
            <SchemaChangeList
              schemas={schemaList}
              urlSchemaCard={URLSchemaCard}
              pageNumber={pageNo}
              totalCount={fetchSchemasResponse.totalCount}
              selectedRoleID={selectedRoleID}
              handlePrev={handlePrev}
              handleNext={handleNext}
            />
          </div>
          <div className="w-3/4">
            {selectedRoleID && (
              <SchemaChangeDetails schemaId={selectedRoleID} />
            )}
          </div>
        </Flex>
      );
    }
  }
};

export const SchemaChangeList: React.FC<{
  schemas: SchemaChangeCard[];
  urlSchemaCard: SchemaChangeCard[];
  pageNumber: number;
  totalCount: number;
  selectedRoleID: string | null;
  handlePrev: () => void;
  handleNext: (i: number) => void;
}> = (props) => {
  const {
    schemas,
    urlSchemaCard,
    pageNumber,
    totalCount,
    selectedRoleID,
    handlePrev,
    handleNext,
  } = props;
  const [openSchemaCardIndex, setIsOpenSchemaCardIndex] =
    useState<React.Key>(0);
  const isLastPage =
    totalCount <= (pageNumber + 1) * SCHEMA_LIST_FETCH_BATCH_SIZE;
  const isSecondLastPage =
    totalCount <= (pageNumber + 2) * SCHEMA_LIST_FETCH_BATCH_SIZE;
  const schemaList: SchemaChangeCard[] = [...urlSchemaCard, ...schemas];

  return (
    <div className="overflow-x-auto rounded-md border-neutral-200 bg-white border mr-2">
      <Flex className="w-full bg-gray-100 px-4 py-2">
        <Flex justify="start" className="text-base w-[50%]">
          <span className="text-sm font-bold">PUBLISHED SCHEMA</span>
        </Flex>
      </Flex>

      <Flex direction="column" className="w-full">
        {schemas.length ? (
          <div className="mb-4">
            <div
              style={{ maxHeight: '80vh' }}
              className={`flex flex-col w-full max-h-400px overflow-y-scroll`}
            >
              {schemaList.map((schema, index) => (
                <SchemaCard
                  key={index}
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
            <Flex justify="center" align="center" className="w-full mt-2">
              <button
                className="btn btn-primary items-center max-w-full justify-center inline-flex text-sm font-sans font-semibold border rounded shadow-sm focus-visible:outline-none h-btn px-sm mx-1 disabled:border-gray-300  disabled:opacity-60 disabled:cursor-not-allowed"
                disabled={pageNumber === 0}
                onClick={() => {
                  handlePrev();
                  setIsOpenSchemaCardIndex(0);
                }}
              >
                <Flex align="center">
                  <FaArrowLeft />
                  <span className="ml-1">Prev</span>
                </Flex>
              </button>
              <div className="btn btn-primary items-center max-w-full justify-center inline-flex text-sm text-white font-sans font-semibold bg-[#36A7E6] border rounded shadow-sm focus-visible:outline-none h-btn px-sm mx-1">
                {pageNumber + 1}
              </div>
              {!isLastPage && (
                <button
                  className="btn btn-primary items-center max-w-full justify-center inline-flex text-sm font-sans font-semibold border rounded shadow-sm focus-visible:outline-none h-btn px-sm mx-2 disabled:border-gray-300  disabled:opacity-60 disabled:cursor-not-allowed"
                  onClick={() => {
                    handleNext(1);
                    setIsOpenSchemaCardIndex(0);
                  }}
                >
                  {pageNumber + 2}
                </button>
              )}
              {!(isLastPage || isSecondLastPage) && (
                <button
                  className="btn btn-primary items-center max-w-full justify-center inline-flex text-sm font-sans font-semibold border rounded shadow-sm focus-visible:outline-none h-btn px-sm mx-1 disabled:border-gray-300  disabled:opacity-60 disabled:cursor-not-allowed"
                  onClick={() => {
                    handleNext(2);
                    setIsOpenSchemaCardIndex(0);
                  }}
                >
                  {pageNumber + 3}
                </button>
              )}
              <button
                className="btn btn-primary items-center max-w-full justify-center inline-flex text-sm font-sans font-semibold border rounded shadow-sm focus-visible:outline-none h-btn px-sm mx-1 disabled:border-gray-300  disabled:opacity-60 disabled:cursor-not-allowed"
                disabled={isLastPage}
                onClick={() => {
                  handleNext(1);
                  setIsOpenSchemaCardIndex(0);
                }}
              >
                <Flex align="center">
                  <span className="mr-1">Next</span>
                  <FaArrowRight />
                </Flex>
              </button>
            </Flex>
          </div>
        ) : (
          <div className="white border-t border-neutral-200">
            <div className="p-xs" data-test="label-no-domain-found">
              No schemas published to the schema registry yet
            </div>
          </div>
        )}
      </Flex>
    </div>
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
}> = (props) => {
  const {
    cardKey,
    createdAt,
    roles,
    entryHash,
    tags,
    openSchemaCardIndex,
    selectedRoleID,
    handleClick,
  } = props;
  const navigate = useNavigate();
  const [isTagModalOpen, setIsTagModalOpen] = useState(false);
  const [isCurrentCardOpen, setIsCurrentCardOpen] = useState(false);

  const [tagsList, setTagsList] = React.useState<SchemaRegistryTag[]>(tags);
  const onRemoveTag = (id: string) => {
    const filteredTags = tagsList.filter(
      (schemaRegistryTag) => schemaRegistryTag.id !== id,
    );
    setTagsList(filteredTags);
  };
  const adminIndex = roles.findIndex((role) => role.role === 'admin');

  // If "admin" is not at the 0 index, move it there
  if (adminIndex !== 0 && adminIndex !== -1) {
    const adminRole = roles.splice(adminIndex, 1)[0];
    roles.unshift(adminRole);
  }

  useEffect(() => {
    setTagsList(tags);
  }, [tags]);
  React.useEffect(() => {
    setIsCurrentCardOpen(cardKey === openSchemaCardIndex);
  }, [cardKey, openSchemaCardIndex]);
  const handleRoleClick = (roleBasedChange: Role) => {
    navigate(`/api/schema-registry/${roleBasedChange.id}`);
  };

  const defaultShowAllRoles = isCurrentCardOpen || roles.length <= 3;
  const RolesList = defaultShowAllRoles ? roles : roles.slice(0, 3);
  return (
    <div
      className={`w-full flex flex-col px-4 bg-white rounded mb-1`}
      onClick={() => {
        if (!roles.some((role) => role.id === selectedRoleID)) {
          navigate(`/api/schema-registry/${roles[0].id}`);
        }
        handleClick();
      }}
    >
      <div
        className={`mt-2 ${
          isCurrentCardOpen ? 'bg-gray-100' : ' bg-white'
        } hover:bg-gray-100 `}
      >
        <Flex
          align="center"
          justify="start"
          className="mt-1 mb-2 text-gray-600 text-sm h-full px-2"
        >
          <span>{getPublishTime(createdAt)}</span>
          <span>
            <Analytics name="schema-registry-add-tag-btn">
              {isTagModalOpen && (
                <AddSchemaRegistryTagDialog
                  tagsList={tagsList}
                  setTagsList={setTagsList}
                  entryHash={entryHash}
                  onClose={() => {
                    setIsTagModalOpen(false);
                  }}
                />
              )}
              <Flex align="center">
                <Flex wrap="nowrap" align="center" justify="start">
                  {tagsList &&
                    tagsList.map((schemaRegistryTag) => (
                      <div key={schemaRegistryTag.id} className="mx-1 ">
                        <SchemaTag
                          schemaRegistryTag={schemaRegistryTag}
                          onRemove={onRemoveTag}
                        />
                      </div>
                    ))}
                  <div className="ml-1" onClick={() => setIsTagModalOpen(true)}>
                    <span>
                      <IconTooltip
                        message="Add a Tag"
                        icon={<FaPlusCircle />}
                      />
                    </span>
                  </div>
                </Flex>
              </Flex>
            </Analytics>
          </span>
        </Flex>
        <div
          className={`flex flex-col justify-start w-full pt-auto ${
            isCurrentCardOpen ? 'bg-gray-100' : ''
          } rounded `}
        >
          <div className="text-sm text-gray-500 font-bold mb-1 px-2">
            {roles.length} {roles.length === 1 ? 'ROLE' : 'ROLES'}
          </div>
          <div>
            {RolesList.map((roleBasedChange, index) => (
              <Analytics key={index} name="schema-registry-schema-change-card">
                <div
                  className={`flex w-full px-2 py-1 ${
                    isCurrentCardOpen && roleBasedChange.id === selectedRoleID
                      ? 'bg-gray-200'
                      : ''
                  } rounded hover:bg-gray-200`}
                  onClick={() => {
                    handleRoleClick(roleBasedChange);
                  }}
                  key={index}
                >
                  <Flex
                    align="center"
                    justify="between"
                    className="w-full rounded"
                  >
                    <div className="text-base rounded cursor-pointer">
                      <p className="text-sm text-teal-800 font-bold bg-gray-200 px-1 rounded">
                        {CapitalizeFirstLetter(roleBasedChange.role)}
                      </p>
                    </div>
                    <FaChevronRight />
                  </Flex>
                </div>
              </Analytics>
            ))}
            {!defaultShowAllRoles && (
              <Analytics name="schema-registry-see-more-roles-btn">
                <Flex
                  justify="center"
                  align="center"
                  className="cursor-pointer text-base hover:bg-gray-200 pt-1"
                  //here there is no need for onClick as clicking anywhere on a schemaCard sets the schemaCard as open and defaultShowAllRoles will be set to true
                >
                  <span className="text-gray font-semibold">
                    See More Roles
                  </span>
                  <FaChevronDown />
                </Flex>
              </Analytics>
            )}
          </div>
        </div>
      </div>
      <Flex className="w-full border-b border-gray-300 my-1"></Flex>
    </div>
  );
};
