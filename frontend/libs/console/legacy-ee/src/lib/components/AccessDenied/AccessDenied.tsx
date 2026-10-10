import { Flex } from '@radix-ui/themes';

const AccessDenied = ({ alignCenter = true }) => {
  return (
    <Flex
      style={{
        ...(alignCenter && { justifyContent: 'center' }),
        marginTop: '50px',
      }}
    >
      You don&apos;t have enough permissions to view this section. Ask the
      project owner to grant you the required privileges.
    </Flex>
  );
};

export default AccessDenied;
