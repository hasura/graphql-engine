import { useState, useEffect } from 'react';
import { useFieldArray, UseFormReturn } from 'react-hook-form';
import { OperatorItem } from '../FilterRow';
import { defaultColumn, defaultOperator, FormValues } from '../types';

type UseFilterRowsProps = {
  methods: UseFormReturn<FormValues, any>;
  operators: OperatorItem[];
};

export const useFilterRows = ({ methods, operators }: UseFilterRowsProps) => {
  const getRowIndex = (fieldName: string) =>
    parseInt(fieldName.replace('filter.', '').split('.')[0], 10);

  const { control, watch, getValues, setValue } = methods;

  const { fields, append, remove } = useFieldArray({
    control,
    name: 'filter',
  });

  const [showFirstRemove, setShowFirstRemove] = useState(false);

  useEffect(() => {
    const subscription = watch(
      (formValues, { name: fieldName, type: eventType }) => {
        console.log(formValues, fieldName, eventType);
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
          fieldName?.endsWith('.value') &&
          getValues(fieldName) !== ''
        ) {
          setShowFirstRemove(true);
        }

        if (fieldName?.endsWith('.operator')) {
          const operatorValue = getValues(fieldName);
          const operatorDefinition = operators.find(
            op => op.value === operatorValue
          );
          setValue(
            `filter.${rowId}.value`,
            operatorDefinition?.defaultValue || ''
          );
        }
      }
    );
    return () => subscription.unsubscribe();
  }, [watch]);

  const onAdd = () =>
    append({
      column: defaultColumn,
      operator: defaultOperator,
      value: '',
    });

  const onRemove = (index: number) => {
    if (index > 0) {
      remove(index);
      return;
    }

    setValue(`filter.0.column`, defaultColumn);
    setValue(`filter.0.operator`, defaultOperator);
    setValue(`filter.0.value`, '');
    setShowFirstRemove(false);
  };

  return {
    onAddFilterRow: onAdd,
    onRemoveFilterRow: onRemove,
    filterFields: fields,
    showFirstRemoveOnFilter: showFirstRemove,
  };
};
