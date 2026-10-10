import z from 'zod';
import { SimpleForm, InputField } from '@hasura/shared/ui';
import React, { useEffect } from 'react';
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
  useEffect(() => {
    setSearch(search);
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
      onSubmit={() => {}}
      className="pr-0 pt-0 pb-0 relative top-2"
    >
      <SearchInput setSearch={setSearch} />
    </SimpleForm>
  );
};
