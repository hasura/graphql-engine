import React, { useEffect, useMemo, useState } from 'react';
import { buildClientSchema, GraphQLSchema, IntrospectionQuery } from 'graphql';
import AceEditor from 'react-ace';
import { FaArrowRight } from 'react-icons/fa';
import 'ace-builds/src-noconflict/theme-github';
import 'ace-builds/src-noconflict/theme-eclipse';
import 'ace-builds/src-noconflict/ext-language_tools';
import { CardedTable } from '../../../../../../new-components/CardedTable';

import { useIntrospectionSchema } from './useIntrospectionSchema';
import { TypeSearchForm } from './SearchTypes';
import {
  editorOptions,
  generateAllTypeDefinitions,
  getAllTypeNames,
  SchemaType,
} from './utils';

const filterItemsBySearch = (searchQuery: string, itemList: string[]) => {
  const caseSensitiveResults: string[] = [];
  const caseAgnosticResults: string[] = [];
  itemList.forEach(item => {
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

  const { data: schemaResp, isLoading } = useIntrospectionSchema();
  const [selectedTypes, setSelectedTypes] = useState([] as string[]);
  const [typeDef, setTypeDef] = useState('');

  useEffect(() => {
    setValues({
      selectedTypes,
      typeDef,
    });
  }, [selectedTypes, typeDef, setValues]);

  const clientSchema = useMemo(
    () =>
      schemaResp?.data
        ? buildClientSchema(schemaResp?.data as IntrospectionQuery)
        : ({} as GraphQLSchema),
    [schemaResp]
  );

  const allTypes =
    clientSchema && schemaResp?.data ? getAllTypeNames(clientSchema, true) : [];

  const itemSearchResults = searchText
    ? filterItemsBySearch(searchText, allTypes)
    : allTypes;

  const handleSearch = (value: string) => setSearchText(value);

  useEffect(() => {
    if (isLoading || !schemaResp?.data) return;

    const allTypeDefs = generateAllTypeDefinitions(
      clientSchema,
      selectedTypes,
      'type'
    );

    setTypeDef(` # Imported Types from table schema
${allTypeDefs}`);
  }, [selectedTypes, clientSchema, isLoading]);

  const rowData =
    itemSearchResults?.map(typeName => {
      return [
        <input
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
      <div className="flex gap-8">
        <div className="w-1/2 mb-4">
          <TypeSearchForm setSearch={handleSearch} />
          <div className="flex items-center relative">
            <div className="w-full max-h-[312px] overflow-y-auto">
              <CardedTable columns={['', 'TYPE NAME']} data={rowData} />
            </div>
            <div className="absolute -right-8">
              <FaArrowRight />
            </div>
          </div>
        </div>

        <AceEditor
          value={typeDef}
          theme="github"
          mode="typescript"
          readOnly
          setOptions={editorOptions}
          className="block relative inset-0 w-full max-w-xl h-code h-full input shadow-sm rounded border border-gray-300 focus-within:outline-0 focus-within:ring-2 focus-within:ring-yellow-200 focus-within:border-yellow-400 placeholder-gray-500 bg-gray-200 hover:border-gray-200 mb-4 ml-4"
        />
      </div>
    </div>
  );
};
