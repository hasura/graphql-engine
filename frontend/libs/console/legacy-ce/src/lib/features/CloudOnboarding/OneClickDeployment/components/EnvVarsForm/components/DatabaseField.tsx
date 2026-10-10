import { IAceOptions } from 'react-ace';
import { InputField, CodeEditorField } from '@hasura/shared/ui';
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
  mode: 'json',
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
      />
    );
  }

  return (
    <InputField
      name={envVar.Name}
      label={`${envVar.Name} *`}
      description={envVar.Description}
      fieldProps={{
        placeholder: envVar.Placeholder ? envVar.Placeholder : envVar.Name,
      }}
    />
  );
}
