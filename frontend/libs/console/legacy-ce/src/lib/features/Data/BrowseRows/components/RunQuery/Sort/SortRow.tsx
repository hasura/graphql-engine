import {
  getDialogPortalTarget,
  IconButton,
  ReactSelectField,
  ReactSelectOptionType,
  SelectField,
  SelectItemProps,
} from '@hasura/shared/ui';
import { FaTimesCircle } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { createFilter } from 'react-select';

type SortRowProps = {
  name: string;
  columnOptions: ReactSelectOptionType[];
  orderByOptions: SelectItemProps[];
  onRemove: () => void;
};

export const SortRow = ({
  name,
  columnOptions,
  orderByOptions,
  onRemove,
}: SortRowProps) => (
  <Flex gap="2" align="center">
    <div className="sm:w-4/12">
      <ReactSelectField
        name={`${name}.column`}
        options={columnOptions}
        placeholder="Select a column"
        data-testid={`${name}.column`}
        noErrorPlaceholder
        selectProps={{
          filterOption: createFilter({
            ignoreCase: true,
            matchFrom: 'any',
          }),
          menuPortalTarget: getDialogPortalTarget(),
        }}
      />
    </div>
    <div className="sm:w-4/12">
      <SelectField
        name={`${name}.type`}
        options={orderByOptions}
        placeholder="Order by"
        data-testid={`${name}.type`}
        noErrorPlaceholder
        full
      />
    </div>

    <IconButton
      color="indigo"
      variant="ghost"
      onClick={onRemove}
      data-testid={`${name}.remove`}
    >
      <FaTimesCircle />
    </IconButton>
  </Flex>
);
