import React, { useEffect, useState } from 'react';
import { Dispatch } from 'redux';
import { useGetSchemaChangeList } from '../hooks/useGetSchemaChangeList';
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
import { Analytics } from '../../Analytics';
import { SchemaChangeDetails } from './SchemaChangeDetails';
import {
  ADMIN_ROLE,
  EMPTY_UUID_STRING,
  SCHEMA_LIST_FETCH_BATCH_SIZE,
} from '../constants';
import { IconTooltip } from '../../../new-components/Tooltip';
import _push from '../../../components/Services/Data/push';
import { useAppDispatch } from '../../../storeHooks';
interface SchemaRegistryHomeProps {
  schemaId: string | undefined;
}
export const SchemaRegistryHome: React.FC<SchemaRegistryHomeProps> = props => {
  const { schemaId } = props;
  const projectID = globals.hasuraCloudProjectId || '';
  const dispatch = useAppDispatch();
  const [pageNo, setPageno] = useState(
    parseInt(getSearchParam(window.location.search, 'page') || '0')
  );
  const schemaRoleID = schemaId ? schemaId : EMPTY_UUID_STRING;
  const fetchSchemaResponse = useGetSchemaChangeList(
    projectID,
    SCHEMA_LIST_FETCH_BATCH_SIZE,
    pageNo * SCHEMA_LIST_FETCH_BATCH_SIZE,
    schemaRoleID
  );

  const { kind } = fetchSchemaResponse;

  //to check if fetchSchemaResponse changed

  let latestAdminRoleID: string | null = null;
  if (kind === 'success') {
    const schemaList = schemaChangeListTransformFn(
      fetchSchemaResponse.response
    );
    latestAdminRoleID =
      schemaList[0]?.roles?.find(item => item.role === ADMIN_ROLE)?.id || null;
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
        dispatch(_push(`/settings/schema-registry/${latestAdminRoleID}`));
      }
    } else {
      setSelectedRoleID(schemaRoleID);
    }
  }, [schemaRoleID]);
  useEffect(() => {
    if (latestAdminRoleID) {
      setSelectedRoleID(latestAdminRoleID);
      dispatch(_push(`/settings/schema-registry/${latestAdminRoleID}`));
    }
    setSearchParam(pageNo);
  }, [latestAdminRoleID, pageNo]);
  switch (kind) {
    case 'loading':
      return <p>Loading...</p>;
    case 'error':
      return <p>Error: {fetchSchemaResponse.message}</p>;
    case 'success': {
      const schemaList = schemaChangeListTransformFn(
        fetchSchemaResponse.response
      );
      return (
        <div className="flex w-full">
          <div className="w-1/4">
            <SchemaChangeList
              schemas={schemaList}
              pageNumber={pageNo}
              totalCount={fetchSchemaResponse.totalCount}
              selectedRoleID={selectedRoleID}
              dispatch={dispatch}
              handlePrev={handlePrev}
              handleNext={handleNext}
            />
          </div>
          <div className="w-3/4">
            {selectedRoleID && (
              <SchemaChangeDetails schemaId={selectedRoleID} />
            )}
          </div>
        </div>
      );
    }
  }
};

export const SchemaChangeList: React.VFC<{
  schemas: SchemaChangeCard[];
  pageNumber: number;
  totalCount: number;
  selectedRoleID: string | null;
  dispatch: Dispatch;
  handlePrev: () => void;
  handleNext: (i: number) => void;
}> = props => {
  const {
    schemas,
    pageNumber,
    totalCount,
    selectedRoleID,
    handlePrev,
    handleNext,
    dispatch,
  } = props;
  const [openSchemaCardIndex, setIsOpenSchemaCardIndex] =
    useState<React.Key>(0);
  const isLastPage =
    totalCount <= (pageNumber + 1) * SCHEMA_LIST_FETCH_BATCH_SIZE;
  const isSecondLastPage =
    totalCount <= (pageNumber + 2) * SCHEMA_LIST_FETCH_BATCH_SIZE;
  return (
    <div className="overflow-x-auto rounded-md border-neutral-200 bg-white border mr-sm">
      <div className="w-full flex bg-gray-100 px-4 py-2">
        <div className="flex text-base w-[69%] justify-start">
          <span className="text-sm font-bold">PUBLISHED SCHEMA</span>
        </div>
      </div>
      <div className="flex flex-col w-full">
        {schemas.length ? (
          <div className="mb-md">
            {schemas.map((schema, index) => (
              <SchemaCard
                cardKey={index}
                openSchemaCardIndex={openSchemaCardIndex}
                selectedRoleID={selectedRoleID}
                createdAt={schema.created_at}
                roles={schema.roles}
                entryHash={schema.entry_hash}
                tags={schema.tags}
                dispatch={dispatch}
                handleClick={() => setIsOpenSchemaCardIndex(index)}
              />
            ))}
            <div className="flex w-full justify-center items-center">
              <button
                className="btn btn-primary items-center max-w-full justify-center inline-flex text-sm font-sans font-semibold border rounded shadow-sm focus-visible:outline-none h-btn px-sm mx-1 disabled:border-gray-300  disabled:opacity-60 disabled:cursor-not-allowed"
                disabled={pageNumber === 0}
                onClick={() => {
                  handlePrev();
                  setIsOpenSchemaCardIndex(0);
                }}
              >
                <div className="flex items-center">
                  <FaArrowLeft />
                  <span className="ml-1">Prev</span>
                </div>
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
                <div className="flex items-center">
                  <span className="mr-1">Next</span>
                  <FaArrowRight />
                </div>
              </button>
            </div>
          </div>
        ) : (
          <div className="white border-t border-neutral-200">
            <div className="p-xs" data-test="label-no-domain-found">
              No schemas published to the schema registry yet
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

const SchemaCard: React.VFC<{
  cardKey: React.Key;
  createdAt: string;
  roles: Role[];
  entryHash: string;
  tags: SchemaRegistryTag[];
  openSchemaCardIndex: React.Key;
  selectedRoleID: string | null;
  dispatch: Dispatch;
  handleClick: () => void;
}> = props => {
  const {
    cardKey,
    createdAt,
    roles,
    entryHash,
    tags,
    openSchemaCardIndex,
    selectedRoleID,
    dispatch,
    handleClick,
  } = props;
  const [isTagModalOpen, setIsTagModalOpen] = React.useState(false);
  const [isRolesMenuOpen, setIsRolesMenuOpen] = useState(false);
  const [tagsList, setTagsList] = React.useState<SchemaRegistryTag[]>(tags);
  const onRemoveTag = (id: string) => {
    const filteredTags = tagsList.filter(
      schemaRegistryTag => schemaRegistryTag.id !== id
    );
    setTagsList(filteredTags);
  };
  useEffect(() => {
    setTagsList(tags);
  }, [tags]);
  React.useEffect(() => {
    setIsRolesMenuOpen(cardKey === openSchemaCardIndex);
  }, [cardKey, openSchemaCardIndex]);
  const handleRoleClick = (roleBasedChange: Role) => {
    dispatch(_push(`/settings/schema-registry/${roleBasedChange.id}`));
  };
  return (
    <div
      className="w-full flex flex-col px-4 bg-white rounded"
      onClick={handleClick}
    >
      <div className="flex flex-col mt-2 justify-between h-full">
        <div className="flex text-gray-600 text-sm justify-start">
          <span>{getPublishTime(createdAt)}</span>
        </div>
        <div
          className={`flex font-semibold  text-md justify-end hover:text-gray-600  ${
            isRolesMenuOpen ? 'text-gray-600' : 'text-gray-400'
          } cursor-pointer`}
        >
          {roles.length} {roles.length === 1 ? 'Role' : 'Roles'}
          <span className="mr-1">{<FaChevronDown />}</span>
        </div>
      </div>
      {isRolesMenuOpen && (
        <div className="w-full pt-auto">
          <div className="text-sm text-gray-500 font-bold mb-3">
            {roles.length === 1 ? 'ROLE' : 'ROLES'}
          </div>
          {roles.map((roleBasedChange, index) => (
            <div
              className={`flex w-full p-2 ${
                roleBasedChange.id === selectedRoleID ? 'bg-gray-100' : ''
              } rounded hover:bg-gray-200`}
              onClick={() => {
                handleRoleClick(roleBasedChange);
              }}
              key={index}
            >
              <div className="flex items-center justify-between w-full rounded">
                <div className="text-base rounded cursor-pointer">
                  <p className="text-sm text-teal-800 font-bold bg-gray-200 px-1 rounded">
                    {CapitalizeFirstLetter(roleBasedChange.role)}
                  </p>
                </div>
                <FaChevronRight />
              </div>
            </div>
          ))}
        </div>
      )}
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
      <div className="flex flex-nowrap items-center justify-start mt-2">
        {tagsList &&
          tagsList.map(schemaRegistryTag => (
            <div className="mr-2 ">
              <SchemaTag
                schemaRegistryTag={schemaRegistryTag}
                onRemove={onRemoveTag}
              />
            </div>
          ))}
        <Analytics name="schema-registry-add-tag-btn">
          <div
            className="mt-[7px] ml-[-6px] pb-2"
            onClick={() => setIsTagModalOpen(true)}
          >
            <span>
              <IconTooltip message="Add a Tag" icon={<FaPlusCircle />} />
            </span>
          </div>
        </Analytics>
      </div>
      <div className="flex w-full border-b border-gray-300 my-2"></div>
    </div>
  );
};
