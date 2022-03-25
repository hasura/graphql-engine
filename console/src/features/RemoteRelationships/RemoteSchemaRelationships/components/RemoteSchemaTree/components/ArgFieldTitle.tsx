import React from 'react';
import { ArgValueForm } from './ArgValueForm';
import { RelationshipFields, ArgValue, HasuraColumn } from '../../../types';

type ArgFieldTitleProps = {
  title: string;
  argKey: string;
  relationshipFields: RelationshipFields[];
  setRelationshipFields: React.Dispatch<
    React.SetStateAction<RelationshipFields[]>
  >;
  showForm: boolean;
  argValue: ArgValue;
  columns: HasuraColumn;
};

const titleStyles =
  'flex items-center cursor-pointer text-purple-600 whitespace-nowrap hover:text-purple-900';

export const ArgFieldTitle = ({
  title,
  argKey,
  relationshipFields,
  setRelationshipFields,
  showForm,
  argValue,
  columns,
}: ArgFieldTitleProps) => {
  return showForm ? (
    <>
      <div className={titleStyles}>{title}</div>
      <ArgValueForm
        argKey={argKey}
        relationshipFields={relationshipFields}
        setRelationshipFields={setRelationshipFields}
        argValue={argValue}
        columns={columns}
      />
    </>
  ) : (
    <div className={titleStyles}>{title}</div>
  );
};
