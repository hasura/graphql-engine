import z from 'zod';
import { Form, InputField } from '@/new-components/Form';
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
      placeholder="Search Operations..."
      icon={<FaSearch />}
      name="search"
    />
  );
};

export const QueryCollectionsOperationsSearchForm: React.FC<QueryCollectionsOperationsSearchFormProps> =
  ({ setSearch }) => {
    return (
      <Form schema={schema} onSubmit={() => {}} className="p-0 relative top-2">
        {() => <SearchInput setSearch={setSearch} />}
      </Form>
    );
  };
