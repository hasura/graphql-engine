import React from 'react';
import { IAceOptions } from 'react-ace';
import {
  InputField,
  CodeEditorField,
} from '../../../../../../new-components/Form';
import { RequiredEnvVar } from '../../../types';
import { PgDatabaseField } from './PgDatabaseField';

type Props = {
  envVar: RequiredEnvVar;
};

const editorOptions: IAceOptions = {
  fontSize: 12,
  showGutter: true,
  tabSize: 2,
  showLineNumbers: true,
  minLines: 10,
  maxLines: 10,
};

export function DatabaseField(props: Props) {
  const { envVar } = props;

  if (envVar.SubKind === 'postgres') {
    return <PgDatabaseField dbEnvVar={envVar} />;
  }

  if (envVar.ValueType === 'JSON') {
    return (
      <CodeEditorField
        name={envVar.Name}
        label={`${envVar.Name} *`}
        description={envVar.Description}
        editorOptions={editorOptions}
        theme="eclipse"
      />
    );
  }

  return (
    <InputField
      name={envVar.Name}
      label={`${envVar.Name} *`}
      placeholder={envVar.Placeholder ? envVar.Placeholder : envVar.Name}
      description={envVar.Description}
    />
  );
}
