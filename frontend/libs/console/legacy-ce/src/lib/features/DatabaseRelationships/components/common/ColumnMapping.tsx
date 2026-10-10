import { FieldError, useFieldArray, useFormContext } from 'react-hook-form';
import { Button, IndicatorCard, SelectField } from '@hasura/shared/ui';
import {
  FaArrowAltCircleLeft,
  FaArrowAltCircleRight,
  FaArrowRight,
  FaExclamationCircle,
  FaTimesCircle,
} from 'react-icons/fa';
import { Flex, Skeleton } from '@radix-ui/themes';
import { useTableColumns } from '@hasura/metadata/data-source';
import { useMetadata } from '@hasura/metadata/api';
import { MetadataSelectors } from '@hasura/metadata/helpers';

const name = 'columnMap';

export const MapColumns = () => {
  const { data: meta } = useMetadata();
  const { fields, append } = useFieldArray({ name });

  const {
    watch,
    setValue,
    formState: { errors },
  } = useFormContext();

  const formErrorMessage = (errors?.[name] as unknown as FieldError)?.message;

  const fromSource = watch('fromSource');
  const toSource = watch('toSource');
  const fromSourceData = MetadataSelectors.findSource(
    fromSource?.value?.dataSourceName,
  )(meta);
  const toSourceData = MetadataSelectors.findSource(
    toSource?.value?.dataSourceName,
  )(meta);

  const columnMappings = watch(name);
  const {
    data: sourceTableColumns,
    isLoading: areSourceColumnsLoading,
    error: sourceColumnsFetchError,
  } = useTableColumns(
    {
      source:
        fromSourceData?.name && fromSourceData?.kind
          ? {
              name: fromSourceData.name,
              kind: fromSourceData.kind,
            }
          : undefined,
      table: fromSource?.value?.table,
    },
    {
      select: (data) => data.columns,
    },
  );

  const {
    data: targetTableColumns,
    isLoading: areTargetColumnsLoading,
    error: targetColumnsFetchError,
  } = useTableColumns(
    {
      source:
        toSourceData?.name || toSourceData?.kind
          ? {
              name: toSourceData.name,
              kind: toSourceData.kind,
            }
          : undefined,
      table: toSource?.value?.table,
    },
    {
      select: (data) => data.columns,
    },
  );

  if (sourceColumnsFetchError || targetColumnsFetchError)
    return (
      <div className="px-4 pb-4 mb-4 mt-0 h">
        <div className="items-center mb-2 font-semibold text-gray-600">
          <IndicatorCard
            status="negative"
            headline="Errors while fetching columns"
            showIcon
          >
            <ul>
              {!!sourceColumnsFetchError && (
                <li>
                  source table error:{' '}
                  {JSON.stringify(
                    (sourceColumnsFetchError as any).response?.data,
                  )}
                </li>
              )}
              {!!targetColumnsFetchError && (
                <li>
                  target table error:
                  {JSON.stringify(
                    (targetColumnsFetchError as any).response?.data,
                  )}
                </li>
              )}
            </ul>
          </IndicatorCard>
        </div>
      </div>
    );

  return (
    <div className="px-4 pb-4 mb-4 mt-0 h">
      <div className="grid grid-cols-12 mb-1 items-center font-semibold text-muted">
        <Flex align="center" className="col-span-6">
          Source Column{' '}
          <FaArrowAltCircleRight className="fill-emerald-700 ml-1.5" />
        </Flex>
        <Flex align="center" className="col-span-6">
          Reference Column{' '}
          <FaArrowAltCircleLeft className="fill-violet-700 ml-1.5" />
        </Flex>
      </div>
      {fields.map((field, index) => {
        return (
          <div
            className="grid grid-cols-12 items-center"
            key={`${index}_column_map_row`}
          >
            <div className="col-span-5">
              <Skeleton loading={areSourceColumnsLoading}>
                <SelectField
                  options={(sourceTableColumns ?? []).map((column) => ({
                    label: column.name,
                    value: column.name,
                  }))}
                  name={`${name}.${index}.from`}
                  disabled={!sourceTableColumns?.length}
                  placeholder="Select source column"
                />
              </Skeleton>
            </div>

            <Flex className="justify-around">
              <FaArrowRight className="fill-muted" />
            </Flex>
            <div className="col-span-5">
              <Skeleton loading={areTargetColumnsLoading}>
                <SelectField
                  options={(targetTableColumns ?? []).map((column) => ({
                    label: column.name,
                    value: column.name,
                  }))}
                  name={`${name}.${index}.to`}
                  disabled={!targetTableColumns?.length}
                  placeholder="Select reference column"
                  noErrorPlaceholder
                />
              </Skeleton>
            </div>
            <Flex className="justify-around">
              <Button
                type="button"
                size="sm"
                className="h-10"
                leftIcon={FaTimesCircle}
                onClick={() => {
                  setValue(
                    name,
                    columnMappings.filter((_, i) => index !== i),
                  );
                }}
              />
            </Flex>
          </div>
        );
      })}
      {formErrorMessage && (
        <Flex align="center" className="text-red-600 mt-1 text-sm">
          <FaExclamationCircle className="fill-current h-4 w-4 mr-1 shrink-0" />{' '}
          {formErrorMessage}
        </Flex>
      )}
      <div className="my-4">
        <Button
          type="button"
          onClick={() => append({})}
          disabled={!targetTableColumns?.length || !sourceTableColumns?.length}
        >
          Add New Mapping
        </Button>
      </div>
    </div>
  );
};
