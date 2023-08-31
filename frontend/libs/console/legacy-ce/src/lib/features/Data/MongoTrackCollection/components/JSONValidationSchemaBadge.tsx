import { FaInfo } from 'react-icons/fa';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';

export const JSONValidationSchemaBadge = () => {
  return (
    <IndicatorCard
      status="info"
      className="py-4 px-md"
      showIcon
      contentFullWidth
      customIcon={() => <FaInfo />}
    >
      <div className='flex items-center justify-between mx-4"'>
        <div>
          <h1 className="font-bold text-lg">
            A JSON Validation Schema is required
          </h1>
          <div className="text-muted whitespace-break-spaces">
            Please ensure a JSON validation schema is loaded in your Collection.
            A JSON validation schema is required for Hasura to automatically
            generate a GraphQL types from your database.{' '}
            {/* <LearnMoreLink href="" text="(Know More)" /> */}
          </div>
        </div>
      </div>
    </IndicatorCard>
  );
};
