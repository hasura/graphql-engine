import { FaInfo } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { IndicatorCard } from '@hasura/shared/ui';

export const JSONValidationSchemaBadge = () => {
  return (
    <IndicatorCard
      status="info"
      className="py-4 px-4"
      showIcon
      customIcon={() => <FaInfo />}
    >
      <Flex align="center" justify="between" className='mx-4"'>
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
      </Flex>
    </IndicatorCard>
  );
};
