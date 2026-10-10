import { Flex } from '@radix-ui/themes';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { RelativeLink } from '@hasura/shared/ui';
import { useAppContext } from '@hasura/shared/context';

export class NotFoundError extends Error {}

type PageNotFoundProps = {
  resetCallback?: () => void;
};

const PageNotFound = (props: PageNotFoundProps) => {
  useDocumentTitle('404 - Page Not Found | Hasura');
  const { envVars } = useAppContext();
  const errorImage = `${envVars.assetsPath}/common/img/hasura_icon_green.svg`;

  return (
    <Flex align="center" justify="center" className="h-screen w-screen">
      <Flex justify="between" className="w-7/12">
        <div className="px-5 md:p-0">
          <h1 className="font-bold text-6xl">404</h1>
          <br />
          This page does not exist. Head back{' '}
          <RelativeLink to="/" onClick={props.resetCallback}>
            Home
          </RelativeLink>
          .
        </div>
        <div className="w-1/3">
          <img
            src={errorImage}
            title="We think you are lost!"
            alt="Not found"
          />
        </div>
      </Flex>
    </Flex>
  );
};

export default PageNotFound;
