import clsx from 'clsx';
import { CardedTable } from '../../../../../new-components/CardedTable';
import { InputField, Select } from '../../../../../new-components/Form';
import { useFieldArray, useFormContext } from 'react-hook-form';
import { BooleanInput } from '../../components/BooleanInput';
import { Button } from '../../../../../new-components/Button';
import { FiTrash2 } from 'react-icons/fi';

export const ArgumentsInput = ({
  name,
  types,
  disabled,
}: {
  name: string;
  types: string[];
  disabled?: boolean;
}) => {
  const { control } = useFormContext();

  const { fields, append, remove } = useFieldArray({
    control,
    name,
  });

  return (
    <div>
      <label className={clsx('block pt-1 text-gray-600 mb-xs')}>
        <span className={clsx('flex items-center')}>
          <span className={clsx('font-semibold')}>Arguments</span>
        </span>
      </label>
      {fields.length ? (
        <CardedTable.Table>
          <CardedTable.TableHead>
            <CardedTable.TableHeadRow>
              <CardedTable.TableHeadCell>
                Argument name
              </CardedTable.TableHeadCell>
              <CardedTable.TableHeadCell>Type</CardedTable.TableHeadCell>
              <CardedTable.TableHeadCell>Nullable</CardedTable.TableHeadCell>
              <CardedTable.TableHeadCell>Actions</CardedTable.TableHeadCell>
            </CardedTable.TableHeadRow>
          </CardedTable.TableHead>
          <CardedTable.TableBody>
            {fields.map((field, index) => (
              <CardedTable.TableBodyRow key={field.id}>
                <CardedTable.TableBodyCell>
                  <InputField
                    dataTestId={`${name}[${index}].name`}
                    name={`${name}[${index}].name`}
                    placeholder="Field Name"
                    disabled={disabled}
                  />
                </CardedTable.TableBodyCell>
                <CardedTable.TableBodyCell>
                  <Select
                    name={`${name}[${index}].type`}
                    label=""
                    options={types.map(t => ({ label: t, value: t }))}
                    disabled={disabled}
                  />
                </CardedTable.TableBodyCell>
                <CardedTable.TableBodyCell>
                  <div className="mb-md">
                    <BooleanInput
                      name={`${name}[${index}].nullable`}
                      disabled={disabled}
                    />
                  </div>
                </CardedTable.TableBodyCell>
                <CardedTable.TableBodyCell className="align-top">
                  <div className="px-sm py-xs">
                    <Button
                      icon={<FiTrash2 />}
                      onClick={() => remove(index)}
                      mode="destructive"
                      disabled={disabled}
                    />
                  </div>
                </CardedTable.TableBodyCell>
              </CardedTable.TableBodyRow>
            ))}
          </CardedTable.TableBody>
        </CardedTable.Table>
      ) : null}
      <Button
        className="mb-sm"
        onClick={() => {
          append({ name: '', type: 'text', nullable: true });
        }}
        disabled={disabled}
      >
        Add new argument
      </Button>
    </div>
  );
};
