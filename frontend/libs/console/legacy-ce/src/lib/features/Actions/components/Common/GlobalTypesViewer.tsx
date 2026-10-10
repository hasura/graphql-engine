import { buildTypeSDL } from '../../../../shared/utils/sdlUtils';
import GlobalTypesDefIcon from '../../../../components/Common/Icons/GlobalTypesDef';
import { useMetadata } from '@hasura/metadata/api';
import { FieldLabel, GraphqlCodeBlock } from '@hasura/shared/ui';
import { Skeleton } from '@radix-ui/themes';

const GlobalTypesViewer = () => {
  const { data: allTypes, isFetching } = useMetadata(
    (m) => m.metadata?.custom_types,
  );

  const existingTypeDefSdl = allTypes ? buildTypeSDL(allTypes) : '';

  return (
    <Skeleton loading={isFetching}>
      <FieldLabel
        labelIcon={<GlobalTypesDefIcon />}
        label="Declared Global Types"
        description="Global types which have been declared previously."
      />

      <GraphqlCodeBlock
        text={existingTypeDefSdl}
        className="mt-4"
        scrollable
        hideCopyButton
        size="1"
      />
    </Skeleton>
  );
};

export default GlobalTypesViewer;
