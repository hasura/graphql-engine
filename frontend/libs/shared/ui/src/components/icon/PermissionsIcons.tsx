import { AccessType } from '@hasura/shared/types';
import { Text, TextProps } from '@radix-ui/themes';
import { FaCheck, FaExclamation, FaFilter, FaTimes } from 'react-icons/fa';

export type PermissionsIconProps = TextProps & {
  type: AccessType;
};

export const PermissionsIcon = ({ type, ...rest }: PermissionsIconProps) => {
  if (type === 'fullAccess') {
    return (
      <Text color="green" {...rest}>
        <FaCheck />
      </Text>
    );
  }

  if (type === 'partialAccess') {
    return (
      <Text color="indigo" {...rest}>
        <FaFilter />
      </Text>
    );
  }

  if (type === 'partialAccessWarning') {
    return (
      <Text color="amber" {...rest}>
        <FaExclamation />
      </Text>
    );
  }

  return (
    <Text color="red" {...rest}>
      <FaTimes />
    </Text>
  );
};
