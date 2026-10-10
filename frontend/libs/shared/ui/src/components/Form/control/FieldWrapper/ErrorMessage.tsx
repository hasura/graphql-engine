import { Text } from '@radix-ui/themes';

type Props = {
  error?: React.ReactNode;
  noErrorPlaceholder?: boolean;
};

export const ErrorMessage = ({ error, noErrorPlaceholder }: Props) => {
  if (!error) {
    return noErrorPlaceholder ? null : (
      <div>
        <Text size="1">
          <>&nbsp;</>
        </Text>
      </div>
    );
  }

  return (
    <div role="alert">
      <Text color="red" size="1">
        {error}
      </Text>
    </div>
  );
};
