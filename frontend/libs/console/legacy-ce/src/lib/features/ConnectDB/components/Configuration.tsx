import { useFormContext } from 'react-hook-form';
import { SupportedDriver } from '@hasura/shared/types';
import { IndicatorCard, OpenApi3Form } from '@hasura/shared/ui';
import {
  NotImplementedError,
  useDatabaseConfiguration,
} from '@hasura/metadata/data-source';

interface Props {
  name: string;
}

export const Configuration = ({ name }: Props) => {
  const { watch } = useFormContext();
  const driver: SupportedDriver = watch('driver');
  const { data: schema, isLoading, error } = useDatabaseConfiguration(driver);

  if (error) {
    if (error instanceof NotImplementedError) {
      return (
        <IndicatorCard>Feature is not available for {driver}</IndicatorCard>
      );
    }

    return (
      <IndicatorCard status="negative">
        Error loading driver configuration
      </IndicatorCard>
    );
  }

  if (!driver) {
    return <IndicatorCard>Driver not selected</IndicatorCard>;
  }

  if (isLoading) {
    return <IndicatorCard>Loading configuration info...</IndicatorCard>;
  }

  if (!schema)
    return (
      <IndicatorCard status="negative">
        Unable to find a valid schema for the {driver}
      </IndicatorCard>
    );

  return (
    <OpenApi3Form
      name={name}
      schemaObject={schema.configSchema}
      references={schema.otherSchemas}
    />
  );
};
