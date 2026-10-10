import { Separator, Text } from '@hasura/shared/ui';

type Props = {
  label: string;
};

export function ListHeader(props: Props) {
  const { label } = props;
  return (
    <>
      <Text as="p" weight="bold">
        {label}
      </Text>
      <Separator size="4" />
    </>
  );
}
