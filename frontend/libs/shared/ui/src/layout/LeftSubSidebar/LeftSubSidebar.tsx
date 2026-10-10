import React from 'react';
import { Link } from 'react-router';
import { Flex, Skeleton } from '@radix-ui/themes';
import { Button, Text } from '../../components';

interface Props extends React.ComponentProps<'div'> {
  loading?: boolean;
  showAddBtn: boolean;
  searchInput: React.ReactNode;
  heading: React.ReactNode;
  addLink: string;
  addLabel: string;
  addTestString: string;
  childListTestString: string;
  /* padding addBtn override the default "create" button
  e.g. for action creation in pro console we pass the dropdown button to choose between
  action form and import from OpenAPI
  */
  addBtn?: React.ReactNode;
  children?: React.ReactNode;
}

export const LeftSubSidebar: React.FC<Props> = ({
  loading = false,
  showAddBtn,
  searchInput,
  heading,
  addLink,
  addLabel,
  addTestString,
  children,
  childListTestString,
  addBtn,
}) => {
  const getAddButton = () => {
    if (showAddBtn) {
      return (
        <div>
          <Link to={addLink}>
            <Button size="1" mode="default" data-test={addTestString}>
              {addLabel}
            </Button>
          </Link>
        </div>
      );
    }

    return null;
  };

  return (
    <div className="px-6">
      <div className="my-4">{searchInput}</div>
      <div>
        <Flex gap="2" justify="between" align="center">
          <Text weight="medium">{heading}</Text>
          {addBtn ?? getAddButton()}
        </Flex>
        <div className="py-4" data-test={childListTestString}>
          <Skeleton loading={loading}>{children}</Skeleton>
        </div>
      </div>
    </div>
  );
};
