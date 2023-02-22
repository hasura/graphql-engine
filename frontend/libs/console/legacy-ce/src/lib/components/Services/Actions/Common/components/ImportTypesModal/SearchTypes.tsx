import z from 'zod';
import { InputField, SimpleForm } from '../../../../../../new-components/Form';
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
      placeholder="Search Types..."
      icon={<FaSearch />}
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
      className="pr-0 pt-0 pb-0 relative"
    >
      <SearchInput setSearch={setSearch} />
    </SimpleForm>
  );
};
