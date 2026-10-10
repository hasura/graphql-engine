import { Text } from '@hasura/shared/ui';

export const Token = ({ token, inline }: { token: any; inline?: boolean }) => {
  return <Text className={inline ? 'inline-block' : ''}>{token}</Text>;
};
