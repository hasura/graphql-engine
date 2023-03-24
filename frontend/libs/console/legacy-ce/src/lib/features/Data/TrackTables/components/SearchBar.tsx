import { FaSearch } from 'react-icons/fa';
import { z } from 'zod';
import { Button } from '../../../../new-components/Button';
import { InputField, SimpleForm } from '../../../../new-components/Form';

type SearchBarProps = {
  onSubmit: (searchText: string) => void;
};

const schema = z.object({
  searchText: z.string().optional(),
});

export const SearchBar = (props: SearchBarProps) => {
  const { onSubmit } = props;

  return (
    <SimpleForm
      schema={schema}
      onSubmit={data => {
        onSubmit(data.searchText ?? '');
      }}
    >
      <div className="flex gap-2">
        <InputField
          name="searchText"
          icon={<FaSearch />}
          iconPosition="start"
          noErrorPlaceholder
          clearButton
        />
        <Button type="submit">Search</Button>
      </div>
    </SimpleForm>
  );
};
