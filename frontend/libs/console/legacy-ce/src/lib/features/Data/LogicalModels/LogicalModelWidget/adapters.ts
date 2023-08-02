import { LogicalModel } from '../../../hasura-metadata-types';
import { AddLogicalModelFormData } from './validationSchema';

export const adaptLogicalModelToFormData = ({
  dataSourceName,
  logicalModel,
}: {
  dataSourceName: string;
  logicalModel: LogicalModel;
}): AddLogicalModelFormData => {
  return {
    dataSourceName,
    name: logicalModel.name,
    fields: logicalModel.fields,
  };
};
