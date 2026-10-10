import React from 'react';
import { FaShareSquare } from 'react-icons/fa';
import { Source } from '@hasura/shared/types';
import { getSchemasForDb } from './utils';
import { modalOpenFn } from './types';
import { useGlobalSchemaSharingConfiguration } from './hooks/useGlobalSchemaSharingConfiguration';
import { CardedTable, IndicatorCard, Text } from '@hasura/shared/ui';
import { Heading, Link, Skeleton } from '@radix-ui/themes';

export const TemplateGalleryBody: React.FC<{
  onModalOpen: modalOpenFn;
  showHeader?: boolean;
  source: Source;
}> = ({ onModalOpen, showHeader = true, source }) => {
  const { data, isFetching, error } = useGlobalSchemaSharingConfiguration();

  if (!data && isFetching) {
    return (
      <Text as="p" align="center">
        Loading templates...
      </Text>
    );
  }

  if (!data && error) {
    return (
      <IndicatorCard status="negative" showIcon>
        Something went wrong, please try again later.
      </IndicatorCard>
    );
  }

  const templateForDb = getSchemasForDb(data ?? [], source.kind);

  if (!templateForDb.length) {
    return (
      <Text as="p" align="center">
        No templates
      </Text>
    );
  }

  return (
    <>
      {showHeader && <Heading size="4">Template Gallery</Heading>}
      <Text as="p">
        Templates are a utility for applying pre-created sets of SQL migrations
        and Hasura metadata.
      </Text>
      <Text as="p">
        Below are sets of pre-created templates made to help you get up to speed
        with the functionality of the Hasura platform.
      </Text>
      <div className="my-4">
        <Skeleton loading={isFetching}>
          <CardedTable
            columns={['Template Name', 'Description']}
            data={templateForDb.flatMap((section) => [
              [
                <Text key={section.name} weight="bold">
                  {section.name}
                </Text>,
              ],
              ...section.templates.map((template) => [
                <Link
                  key={template.title}
                  onClick={() => onModalOpen(template)}
                  className="cursor-pointer!"
                >
                  {template.title}
                </Link>,
                template.description,
              ]),
            ])}
          />
        </Skeleton>
      </div>
      <Text as="p">
        Want to contribute to the official template gallery?{' '}
        <Link
          target="_blank"
          rel="noopener noreferrer"
          href="https://github.com/hasura/template-gallery/discussions/2"
        >
          Find out more <FaShareSquare aria-hidden="true" />
        </Link>
      </Text>
    </>
  );
};
