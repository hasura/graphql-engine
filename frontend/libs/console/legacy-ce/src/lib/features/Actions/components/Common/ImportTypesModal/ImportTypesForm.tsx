import React, { useEffect, useState } from 'react';
import { FaArrowRight } from 'react-icons/fa';
import { CardedTable, AceEditor } from '@hasura/shared/ui';
import { TypeSearchForm } from './SearchTypes';
import {
  editorOptions,
  generateAllTypeDefinitions,
  getAllTypeNames,
  SchemaType,
} from './utils';
import { useIntrospectSchema } from '@hasura/metadata/api';
import { Flex } from '@radix-ui/themes';

const filterItemsBySearch = (searchQuery: string, itemList: string[]) => {
  const caseSensitiveResults: string[] = [];
  const caseAgnosticResults: string[] = [];
  itemList.forEach((item) => {
    if (item.includes(searchQuery)) {
      caseSensitiveResults.push(item);
    } else if (item.toLowerCase().includes(searchQuery.toLowerCase())) {
      caseAgnosticResults.push(item);
    }
  });
  return [
    ...caseSensitiveResults.sort((item1, item2) => {
      return item1.search(searchQuery) > item2.search(searchQuery) ? 1 : -1;
    }),
    ...caseAgnosticResults.sort((item1, item2) => {
      return item1.toLowerCase().search(searchQuery.toLowerCase()) >
        item2.toLowerCase().search(searchQuery.toLowerCase())
        ? 1
        : -1;
    }),
  ];
};

export const ImportTypesForm = (props: {
  setValues: (values: SchemaType) => void;
}) => {
  const { setValues } = props;
  const [searchText, setSearchText] = React.useState('');

  const { data: clientSchema, isLoading } = useIntrospectSchema();
  const [selectedTypes, setSelectedTypes] = useState([] as string[]);
  const [typeDef, setTypeDef] = useState('');

  useEffect(() => {
    setValues({
      selectedTypes,
      typeDef,
    });
  }, [selectedTypes, typeDef, setValues]);

  const allTypes = clientSchema ? getAllTypeNames(clientSchema, true) : [];

  const itemSearchResults = searchText
    ? filterItemsBySearch(searchText, allTypes)
    : allTypes;

  const handleSearch = (value: string) => setSearchText(value);

  useEffect(() => {
    if (isLoading || !clientSchema) return;

    const allTypeDefs = generateAllTypeDefinitions(
      clientSchema,
      selectedTypes,
      'type',
    );

    setTypeDef(` # Imported Types from table schema
${allTypeDefs}`);
  }, [selectedTypes, clientSchema, isLoading]);

  const rowData =
    itemSearchResults?.map((typeName) => {
      return [
        <input
          key={`cb-type-${typeName}`}
          id={`cb-type-${typeName}`}
          type="checkbox"
          checked={selectedTypes.includes(typeName)}
          className="cursor-pointer rounded border shadow-sm"
          onChange={() => {
            const newSet = new Set(selectedTypes);
            if (newSet.has(typeName)) {
              newSet.delete(typeName);
            } else {
              newSet.add(typeName);
            }
            setSelectedTypes(Array.from(newSet));
          }}
        />,
        typeName,
      ];
    }) ?? [];

  if (isLoading) return <div>Loading...</div>;

  return (
    <div>
      <Flex gap="8">
        <div className="w-1/2 mb-4">
          <TypeSearchForm setSearch={handleSearch} />
          <Flex align="center" className="relative">
            <div className="w-full max-h-[312px] overflow-y-auto">
              <CardedTable columns={['', 'TYPE NAME']} data={rowData} />
            </div>
            <div className="absolute -right-8">
              <FaArrowRight />
            </div>
          </Flex>
        </div>

        <AceEditor
          value={typeDef}
          mode="typescript"
          disabled
          setOptions={editorOptions}
        />
      </Flex>
    </div>
  );
};
