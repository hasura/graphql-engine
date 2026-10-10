import React from 'react';
import { GraphQLType } from 'graphql';
import { ArgValueForm } from './ArgValueForm';
import { RelationshipFields, ArgValue, HasuraRsFields } from '../../../types';
import { Text } from '@hasura/shared/ui';

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
  const textContent = (
    <Text
      className="cursor-pointer whitespace-nowrap hover:text-purple-500!"
      color="purple"
    >
      {title}
    </Text>
  );

  return showForm ? (
    <>
      {textContent}
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
    textContent
  );
};
