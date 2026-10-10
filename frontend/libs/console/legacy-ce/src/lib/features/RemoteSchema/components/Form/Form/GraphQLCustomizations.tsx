import { Button, InputField, IconTooltip, Text, Card } from '@hasura/shared/ui';
import { Flex, Heading } from '@radix-ui/themes';
import { FieldError } from 'react-hook-form';
import { FaExclamationCircle } from 'react-icons/fa';

type Props = {
  onClose: () => void;
  disabled?: boolean;
  queryRootError: FieldError | undefined;
  mutationRootError: FieldError | undefined;
};

const GraphQLCustomizations = ({
  disabled,
  onClose,
  queryRootError,
  mutationRootError,
}: Props) => {
  return (
    <Card size="2" variant="surface">
      <Button type="button" mode="default" onClick={onClose}>
        Close
      </Button>

      <Flex align="start" gap="2" className="my-4">
        <Flex align="center" className="w-4/12 pt-1" gap="2">
          <Text as="label">Root Field Namespace</Text>
          <IconTooltip message="Root field type names will be prefixed by this name." />
        </Flex>
        <div className="w-8/12">
          <InputField
            name="customization.root_fields_namespace"
            data-testid="customization.root_fields_namespace"
            fieldProps={{
              disabled,
              placeholder: 'namespace_',
            }}
          />
        </div>
      </Flex>

      <Heading size="4">
        <Flex className="mb-2" align="center" gap="2">
          Types
          <IconTooltip message="add a prefix / suffix to all types of the remote schema" />
        </Flex>
      </Heading>

      <Flex align="start" gap="2" className="my-4">
        <Flex align="center" className="w-4/12 pt-1" gap="2">
          <Text weight="medium" color="gray">
            Prefix
          </Text>
        </Flex>
        <div className="w-8/12">
          <InputField
            name="customization.type_prefix"
            data-testid="customization.type_prefix"
            fieldProps={{
              disabled,
              placeholder: 'prefix_',
            }}
          />
        </div>
      </Flex>
      <Flex align="start" gap="2" className="my-4">
        <Flex align="center" className="w-4/12 pt-1" gap="2">
          <Text as="label" weight="medium" color="gray">
            Suffix
          </Text>
        </Flex>
        <div className="w-8/12">
          <InputField
            name="customization.type_suffix"
            data-testid="customization.type_suffix"
            fieldProps={{
              disabled,
              placeholder: '_suffix',
            }}
          />
        </div>
      </Flex>

      <Flex align="center" gap="2" className="mb-2">
        <Heading color="gray" size="4">
          Fields
        </Heading>
        <IconTooltip message="add a prefix / suffix to the fields of the query / mutation root fields" />
      </Flex>
      <Heading color="gray" size="3">
        Query root
      </Heading>
      {queryRootError?.message && (
        <div
          role="alert"
          aria-label={queryRootError.message}
          className="mt-2 text-red-600 flex items-center"
        >
          <FaExclamationCircle className="fill-current h-4 mr-1" />
          {queryRootError.message}
        </div>
      )}
      <Flex align="start" gap="2" className="my-4">
        <Flex align="center" className="w-4/12 pt-1" gap="2">
          <Text weight="medium" color="gray">
            Type Name
          </Text>
        </Flex>
        <div className="w-8/12">
          <InputField
            name="customization.query_root.parent_type"
            data-testid="customization.query_root.parent_type"
            fieldProps={{
              disabled,
              placeholder: 'Query/query_root',
            }}
          />
        </div>
      </Flex>

      <Flex align="start" gap="2" className="my-4">
        <Flex align="center" className="w-4/12 pt-1" gap="2">
          <Text weight="medium" color="gray">
            Prefix
          </Text>
        </Flex>
        <div className="w-8/12">
          <InputField
            name="customization.query_root.prefix"
            data-testid="customization.query_root.prefix"
            fieldProps={{
              disabled,
              placeholder: 'prefix_',
            }}
          />
        </div>
      </Flex>

      <Flex align="start" gap="2" className="my-4">
        <Flex align="center" className="w-4/12 pt-1" gap="2">
          <Text weight="medium" color="gray">
            Suffix
          </Text>
        </Flex>
        <div className="w-8/12">
          <InputField
            name="customization.query_root.suffix"
            data-testid="customization.query_root.suffix"
            fieldProps={{
              disabled,
              placeholder: '_suffix',
            }}
          />
        </div>
      </Flex>

      <Heading color="gray" size="3">
        Mutation root
      </Heading>
      {mutationRootError?.message && (
        <div
          role="alert"
          aria-label={mutationRootError.message}
          className="mt-2 text-red-600 flex items-center"
        >
          <FaExclamationCircle className="fill-current h-4 mr-1" />
          {mutationRootError.message}
        </div>
      )}

      <Flex align="start" gap="2" className="my-4">
        <Flex align="center" className="w-4/12 pt-1" gap="2">
          <Text weight="medium" color="gray">
            Type Name
          </Text>
        </Flex>
        <div className="w-8/12">
          <InputField
            name="customization.mutation_root.parent_type"
            data-testid="customization.mutation_root.parent_type"
            fieldProps={{
              disabled,
              placeholder: 'Mutation/mutation_root',
            }}
          />
        </div>
      </Flex>

      <Flex align="start" gap="2" className="my-4">
        <Flex align="center" className="w-4/12 pt-1" gap="2">
          <Text weight="medium" color="gray">
            Prefix
          </Text>
        </Flex>
        <div className="w-8/12">
          <InputField
            name="customization.mutation_root.prefix"
            data-testid="customization.mutation_root.prefix"
            fieldProps={{
              disabled,
              placeholder: 'prefix_',
            }}
          />
        </div>
      </Flex>
      <Flex align="start" gap="2" className="my-4">
        <Flex align="center" className="w-4/12 pt-1" gap="2">
          <Text weight="medium" color="gray">
            Suffix
          </Text>
        </Flex>
        <div className="w-8/12">
          <InputField
            name="customization.mutation_root.suffix"
            data-testid="customization.mutation_root.suffix"
            fieldProps={{
              disabled,
              placeholder: '_suffix',
            }}
          />
        </div>
      </Flex>
    </Card>
  );
};

export default GraphQLCustomizations;
