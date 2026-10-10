import {
  CodeEditorField,
  LearnMoreLink,
  IndicatorCard,
} from '@hasura/shared/ui';
import { Code } from '@radix-ui/themes';

export const Template = ({ name }: { name: string }) => {
  return (
    <div>
      <IndicatorCard headline="Using Environment variables">
        It&apos;s good practice to always use environment variables for any
        secrets and avoid sensitive data from being exposed in your metadata as
        plain-text. The{' '}
        <code className="bg-slate-100 rounded text-red-600">template</code>{' '}
        property can be used to provide environment variables set in your Hasura
        instance to be used as connection parameters and uses{' '}
        <LearnMoreLink
          href="https://hasura.io/docs/latest/api-reference/kriti-templating"
          text="Kriti Templating"
          className="ml-0"
        />
        .
        <br /> For example, to use an environment variable for{' '}
        <Code>jdbc_url</Code> would look like -
        <div className="pmt-2 py-2">
          <Code>
            {`{\"jdbc_url\": \"{{getEnvironmentVariable(\"SNOWFLAKE_JDBC_URL\")}}\"}`}
          </Code>
        </div>
      </IndicatorCard>
      <CodeEditorField
        name={name}
        label="Template"
        editorOptions={{
          mode: 'json',
        }}
      />
    </div>
  );
};
