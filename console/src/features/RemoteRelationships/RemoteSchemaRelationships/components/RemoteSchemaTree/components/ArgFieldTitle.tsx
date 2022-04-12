import React from 'react';
import { GraphQLType } from 'graphql';
import { ArgValueForm } from './ArgValueForm';
import { RelationshipFields, ArgValue, HasuraRsFields } from '../../../types';

type ArgFieldTitleProps = {
  title: string;
  argKey: string;
  relationshipFields: RelationshipFields[];
  setRelationshipFields: React.Dispatch<
    React.SetStateAction<RelationshipFields[]>
  >;
  showForm: boolean;
  argValue: ArgValue;
  fields: HasuraRsFields;
  argType: GraphQLType;
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
  fields,
  argType,
}: ArgFieldTitleProps) => {
  return showForm ? (
    <>
      <div className={titleStyles}>{title}</div>
      <ArgValueForm
        argKey={argKey}
        relationshipFields={relationshipFields}
        setRelationshipFields={setRelationshipFields}
        argValue={argValue}
        fields={fields}
        argType={argType}
      />
    </>
  ) : (
    <div className={titleStyles}>{title}</div>
  );
};
