import z from 'zod';
import { Form, InputField } from '@/new-components/Form';
import React, { useEffect } from 'react';
import { useFormContext } from 'react-hook-form';
import { FaSearch } from 'react-icons/fa';

const schema = z.object({
  search: z.string(),
});

interface AllowListSidebarSearchFormProps {
  setSearch: (search: string) => void;
}
const SearchInput: React.FC<AllowListSidebarSearchFormProps> = ({
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
      placeholder="Search Collections..."
      icon={<FaSearch />}
      name="search"
    />
  );
};

export const AllowListSidebarSearchForm: React.FC<AllowListSidebarSearchFormProps> =
  ({ setSearch }) => {
    return (
      <Form
        schema={schema}
        onSubmit={() => {}}
        className="pl-0 pr-0 !p-0 !bg-white"
      >
        {() => <SearchInput setSearch={setSearch} />}
      </Form>
    );
  };
