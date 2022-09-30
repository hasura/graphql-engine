import { useState, useEffect } from 'react';
import { UseFormReturn, useFieldArray } from 'react-hook-form';
import {
  defaultColumn,
  defaultOrder,
  FiltersAndSortFormValues,
} from '../types';

type UseSortRowsProps = {
  methods: UseFormReturn<FiltersAndSortFormValues, any>;
};

export const useSortRows = ({ methods }: UseSortRowsProps) => {
  const getRowIndex = (fieldName: string) =>
    parseInt(fieldName.replace('sort.', '').split('.')[0], 10);
  const [showFirstRemove, setShowFirstRemove] = useState(false);

  const { setValue } = methods;

  const { control, watch, getValues } = methods;

  const { fields, append, remove } = useFieldArray({
    control,
    name: 'sort',
  });

  useEffect(() => {
    const subscription = watch((formValues, { name: fieldName }) => {
      const rowId = getRowIndex(fieldName || '');

      if (
        rowId === 0 &&
        fieldName?.endsWith('.column') &&
        getValues(fieldName) !== defaultColumn
      ) {
        setShowFirstRemove(true);
      }

      if (
        rowId === 0 &&
        fieldName?.endsWith('.order') &&
        getValues(fieldName) !== ''
      ) {
        setShowFirstRemove(true);
      }
    });
    return () => subscription.unsubscribe();
  }, [watch, getValues]);

  const onRemove = (index: number) => {
    if (index > 0) {
      remove(index);
      return;
    }

    setValue(`sort.0.column`, defaultColumn);
    setValue(`sort.0.order`, defaultOrder);
    setShowFirstRemove(false);
  };

  const onAdd = () =>
    append({
      column: defaultColumn,
      order: defaultOrder,
    });

  return {
    onRemoveSortRow: onRemove,
    onAddSortRow: onAdd,
    sortFields: fields,
    showFirstRemoveOnSort: showFirstRemove,
  };
};
