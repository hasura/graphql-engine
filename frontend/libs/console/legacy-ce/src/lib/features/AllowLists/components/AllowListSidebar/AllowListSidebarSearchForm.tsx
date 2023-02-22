import z from 'zod';
import { InputField, useConsoleForm } from '../../../../new-components/Form';
import React, { useEffect } from 'react';
import { FaSearch } from 'react-icons/fa';

interface AllowListSidebarSearchFormProps {
  setSearch: (search: string) => void;
}

export const AllowListSidebarSearchForm: React.FC<
  AllowListSidebarSearchFormProps
> = ({ setSearch }) => {
  const schema = z.object({
    search: z.string(),
  });

  const {
    methods: { watch },
    Form,
  } = useConsoleForm({
    schema,
  });

  const search = watch('search');

  useEffect(() => {
    setSearch(search);
  }, [search]);

  return (
    <Form onSubmit={() => {}} className="pl-0 pr-0 !p-0 bg-transparent">
      <InputField
        id="search"
        placeholder="Search Collections..."
        icon={<FaSearch />}
        name="search"
      />
    </Form>
  );
};
