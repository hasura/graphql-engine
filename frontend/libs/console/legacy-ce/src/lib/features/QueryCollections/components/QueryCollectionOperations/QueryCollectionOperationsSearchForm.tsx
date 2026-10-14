import z from 'zod';
import { SimpleForm, InputField } from '@hasura/shared/ui';
import React, { useEffect, useRef } from 'react';
import { useFormContext } from 'react-hook-form';
import { FaSearch } from 'react-icons/fa';

const schema = z.object({
  search: z.string(),
});

interface QueryCollectionsOperationsSearchFormProps {
  setSearch: (search: string) => void;
}
const SearchInput: React.FC<QueryCollectionsOperationsSearchFormProps> = ({
  setSearch,
}) => {
  const { watch } = useFormContext();
  const search = watch('search');
  // `setSearch` is typically an inline callback recreated on every parent
  // render, so it must not be an effect dependency (that would re-notify the
  // parent on every render and can cause an update loop). We only want to
  // propagate when the searched value actually changes, always calling the
  // latest `setSearch` via a ref kept up to date in its own effect.
  const setSearchRef = useRef(setSearch);
  useEffect(() => {
    setSearchRef.current = setSearch;
  }, [setSearch]);
  useEffect(() => {
    setSearchRef.current(search);
  }, [search]);

  return (
    <InputField
      id="search"
      name="search"
      fieldProps={{
        placeholder: 'Search Operations...',
        icon: FaSearch,
      }}
    />
  );
};

export const QueryCollectionsOperationsSearchForm: React.FC<
  QueryCollectionsOperationsSearchFormProps
> = ({ setSearch }) => {
  return (
    <SimpleForm
      schema={schema}
      onSubmit={() => {
        /* noop */
      }}
      className="pr-0 pt-0 pb-0 relative top-2"
    >
      <SearchInput setSearch={setSearch} />
    </SimpleForm>
  );
};
