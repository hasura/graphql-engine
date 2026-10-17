import React, { useState } from 'react';
import CustomTypesContainer from '../CustomTypesContainer';
import { Button, IconTooltip, Separator } from '@hasura/shared/ui';
import {
  buildCustomTypesFromSDL,
  buildTypeSDL,
} from '../../../../../shared/utils/sdlUtils';
import { hasuraToast } from '@hasura/shared/ui';
import { useAppContext } from '@hasura/shared/context';
import useSetCustomGraphQLTypes from '../../../hooks/useSetCustomGraphQLTypes';
import { GraphQLError } from 'graphql';
import { useMetadata } from '@hasura/metadata/api';
import GraphQLEditor from '../../Common/GraphQLEditor';
import { Flex } from '@radix-ui/themes';

type DefinitionState = {
  sdl: string;
  error: GraphQLError | null | undefined;
  timer: NodeJS.Timeout | null | undefined;
  ast: Record<string, any> | null | undefined;
};

// Sentinel that `allTypes` can never equal, so the render-phase seeding always
// runs on the first render (mirroring the original mount effect).
const NOT_SEEDED = Symbol('types-manage-not-seeded');

const TypesManage = () => {
  const { readOnlyMode } = useAppContext();
  const { data: allTypes } = useMetadata((m) => m.metadata.custom_types);
  const { setCustomGraphQLTypes, isPending: isSettingCustomTypes } =
    useSetCustomGraphQLTypes();
  const [definition, setDefinition] = useState<DefinitionState>({
    sdl: '',
    error: null,
    timer: null,
    ast: null,
  });

  const sdlOnChange = (
    sdl: string | null | undefined,
    error?: GraphQLError | null,
    timer?: NodeJS.Timeout | null,
    ast?: Record<string, any> | null,
  ) => {
    setDefinition({
      error,
      timer,
      ast,
      sdl: sdl ?? '',
    });
  };

  const init = () => {
    if (!allTypes) {
      return;
    }

    const existingTypeDefSdl = buildTypeSDL(allTypes);
    sdlOnChange(existingTypeDefSdl);
  };

  // Seed the editor from metadata during render (guarded by a previous-value
  // check) instead of in an effect. This adjusts state when the `allTypes`
  // metadata changes without an extra setState-in-effect commit, and preserves
  // the original reset-on-metadata-change behaviour (including the initial seed
  // on mount, hence the NOT_SEEDED sentinel so the first render always runs
  // `init`). `init` remains available for the explicit "Reset" button below.
  const [prevAllTypes, setPrevAllTypes] = React.useState<unknown>(NOT_SEEDED);
  if (allTypes !== prevAllTypes) {
    setPrevAllTypes(allTypes);
    init();
  }

  const onSave = () => {
    if (!allTypes) {
      return;
    }

    const { types: newTypes, error: _error } = buildCustomTypesFromSDL(
      definition.sdl,
    );
    if (_error) {
      hasuraToast({
        type: 'error',
        title: 'Invalid Types Definition',
        message: _error,
      });
      return;
    }

    setCustomGraphQLTypes({ existingTypes: allTypes, newTypes });
  };

  // TODO handling error elegantly
  const allowSave = !isSettingCustomTypes && !definition.error && !readOnlyMode;
  const editorTooltip = 'All GraphQL types used in actions';
  const editorLabel = 'All custom types';

  return (
    <div>
      <CustomTypesContainer tabName="manage">
        <GraphQLEditor
          value={definition.sdl}
          error={definition.error}
          timer={definition.timer}
          onChange={sdlOnChange}
          placeholder={''}
          label={editorLabel}
          tooltip={editorTooltip}
          height="600px"
          readOnlyMode={readOnlyMode}
          fontSize="14px"
          allowEmpty
        />
        <Separator size="4" className="my-4" />
        <Flex>
          <div className="mr-5">
            <Button onClick={onSave} disabled={!allowSave} mode="primary">
              Save
            </Button>
          </div>
          <Button
            onClick={init}
            mode="default"
            disabled={readOnlyMode || isSettingCustomTypes}
          >
            Reset
          </Button>
        </Flex>
        {readOnlyMode && (
          <IconTooltip message="Modifying custom type is not allowed in Read only mode!" />
        )}
      </CustomTypesContainer>
    </div>
  );
};

export default TypesManage;
