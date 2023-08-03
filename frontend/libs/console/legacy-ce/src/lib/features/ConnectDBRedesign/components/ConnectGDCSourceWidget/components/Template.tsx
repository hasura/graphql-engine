import { CodeEditorField } from '../../../../../new-components/Form';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { LearnMoreLink } from '../../../../../new-components/LearnMoreLink';

export const Template = ({ name }: { name: string }) => {
  return (
    <div>
      <IndicatorCard headline="Using Environment variables">
        It's good practice to always use environment variables for any secrets
        and avoid sensitive data from being exposed in your metadata as
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
        <code className="bg-slate-100 rounded text-red-600">jdbc_url</code>{' '}
        would look like -
        <div className="bg-gray-100 mt-sm py-sm">
          <code>
            {`{\"jdbc_url\": \"{{getEnvironmentVariable(\"SNOWFLAKE_JDBC_URL\")}}\"}`}
          </code>
        </div>
      </IndicatorCard>
      <CodeEditorField name={name} label="Template" />
    </div>
  );
};
