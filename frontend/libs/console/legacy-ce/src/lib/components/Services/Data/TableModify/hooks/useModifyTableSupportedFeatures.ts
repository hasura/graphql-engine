import { isFeatureSupported } from '../../../../../dataSources';

export const useModifyTableSupportedFeatures = () => {
  const isUntrackSupported = isFeatureSupported('tables.modify.untrack');
  const isModifySupported = isFeatureSupported('tables.modify.delete');
  const isSetAsEnumSupported = isFeatureSupported('tables.modify.setAsEnum');
  const isReadOnlySupported = isFeatureSupported('tables.modify.readOnly');
  const isCommentsViewSupported = isFeatureSupported(
    'tables.modify.comments.view'
  );
  const isCommentsEditSupported = isFeatureSupported(
    'tables.modify.comments.edit'
  );
  const isColumnsViewSupported = isFeatureSupported(
    'tables.modify.columns.view'
  );
  const isColumnsEditSupported = isFeatureSupported(
    'tables.modify.columns.edit'
  );
  const isPrimaryKeysViewSupported = isFeatureSupported(
    'tables.modify.primaryKeys.view'
  );
  const isPrimaryKeysEditSupported = isFeatureSupported(
    'tables.modify.primaryKeys.edit'
  );
  const isForeignKeysEditSupported = isFeatureSupported(
    'tables.modify.foreignKeys.edit'
  );
  const isUniqueKeysEditSupported = isFeatureSupported(
    'tables.modify.uniqueKeys.edit'
  );
  const isCheckConstraintsViewSupported = isFeatureSupported(
    'tables.modify.checkConstraints.view'
  );
  const isCheckConstraintsEditSupported = isFeatureSupported(
    'tables.modify.checkConstraints.edit'
  );
  const isIndexViewSupported = isFeatureSupported('tables.modify.indexes.view');
  const areTriggersSupported = isFeatureSupported('tables.modify.triggers');
  const areComputedFieldSupported = isFeatureSupported(
    'tables.modify.computedFields'
  );
  const isCustomGqlRootSupported = isFeatureSupported(
    'tables.modify.customGqlRoot'
  );

  return {
    areComputedFieldSupported,
    areTriggersSupported,
    isCheckConstraintsEditSupported,
    isCheckConstraintsViewSupported,
    isColumnsEditSupported,
    isColumnsViewSupported,
    isCommentsEditSupported,
    isCommentsViewSupported,
    isCustomGqlRootSupported,
    isForeignKeysEditSupported,
    isIndexViewSupported,
    isModifySupported,
    isPrimaryKeysEditSupported,
    isPrimaryKeysViewSupported,
    isReadOnlySupported,
    isSetAsEnumSupported,
    isUniqueKeysEditSupported,
    isUntrackSupported,
  };
};
