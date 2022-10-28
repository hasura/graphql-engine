import React from 'react';
import AceEditor from 'react-ace';

import { useFormContext } from 'react-hook-form';

import { Builder, JsonItem } from './components';
import { useIntrospectSchema } from './hooks';
import { createDisplayJson } from './utils';

interface Props {
  tableName: string;
  /**
   * The builder is a recursive structure
   * the nesting describes the level of the structure
   * so react hook form can correctly register the fields
   * e.g. ['filter', 'Title', '_eq'] would be registered as 'filter.Title._eq'
   */
  nesting: string[];
}

export const RowPermissionBuilder = ({ tableName, nesting }: Props) => {
  const { watch } = useFormContext();
  const { data: schema } = useIntrospectSchema();

  // by watching the top level of nesting we can get the values for the whole builder
  // this value will always be 'filter' or 'check' depending on the query type
  const value = watch(nesting[0]);
  const json = createDisplayJson(value || {});

  if (!schema) {
    return null;
  }

  return (
    <div className="flex flex-col space-y-4 w-full">
      <div className="p-6 rounded-lg bg-white border border-gray-200 min-h-32 w-full">
        <AceEditor
          mode="json"
          minLines={1}
          fontSize={14}
          height="18px"
          width="100%"
          theme="github"
          name={`${tableName}-json-editor`}
          value={json}
          editorProps={{ $blockScrolling: true }}
        />
      </div>
      <div className="p-6 rounded-lg bg-white border border-gray-200w-full">
        <JsonItem text="{" />
        <div className="py-2">
          <Builder tableName={tableName} nesting={nesting} schema={schema} />
        </div>
        <JsonItem text="}" />
      </div>
    </div>
  );
};
