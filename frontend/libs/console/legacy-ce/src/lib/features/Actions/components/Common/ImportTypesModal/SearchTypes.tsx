import z from 'zod';
import { InputField, SimpleForm } from '@hasura/shared/ui';
import React, { useEffect } from 'react';
import { useFormContext } from 'react-hook-form';
import { FaSearch } from 'react-icons/fa';

const schema = z.object({
  search: z.string(),
});

interface TypeSearchFormProps {
  setSearch: (search: string) => void;
}
const SearchInput: React.FC<TypeSearchFormProps> = ({ setSearch }) => {
  const { watch } = useFormContext();
  const search = watch('search');
  useEffect(() => {
    setSearch(search);
  }, [search]);

  return (
    <InputField
      id="search"
      fieldProps={{
        placeholder: 'Search Types...',
        icon: FaSearch,
      }}
      name="search"
    />
  );
};

export const TypeSearchForm: React.FC<TypeSearchFormProps> = ({
  setSearch,
}) => {
  return (
    <SimpleForm
      schema={schema}
      onSubmit={() => {}}
      options={{ defaultValues: { search: '' } }}
      className="pr-0 pt-0 pb-0 relative"
    >
      <SearchInput setSearch={setSearch} />
    </SimpleForm>
  );
};
