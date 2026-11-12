import { screen } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { testRenderWithClient } from '@hasura/shared/testing';
import { Widget } from './Widget';
import { SourceOption } from './parts/SourceSelect';
import { MODE, Relationship } from '../../types';
import { RenderWidget } from '../RenderWidget/RenderWidget';

class ResizeObserverStub {
  observe() {
    /* no-op */
  }
  unobserve() {
    /* no-op */
  }
  disconnect() {
    /* no-op */
  }
}
globalThis.ResizeObserver =
  globalThis.ResizeObserver ?? (ResizeObserverStub as never);

vi.mock('@hasura/metadata/data-source', () => ({
  useTableColumns: () => ({ data: { columns: [] } }),
  useTableColumnInfos: () => ({ data: [], isLoading: false }),
}));

vi.mock('@hasura/metadata/api', () => ({
  useIntrospectRemoteSchema: () => ({ data: undefined, isLoading: false }),
}));

vi.mock('./utils', () => ({
  useHandleSubmit: () => ({ handleSubmit: vi.fn(), isPending: false }),
}));

vi.mock('../../hooks/useSourceOptions', () => ({
  useSourceOptions: () => ({ sourceOptions, inconsistentSources: [] }),
}));

const sourceOptions: SourceOption[] = [
  {
    label: 'chinook / public.Album',
    value: {
      type: 'table',
      dataSourceName: 'chinook',
      driver: 'postgres',
      table: { name: 'Album', schema: 'public' },
    },
  },
  {
    label: 'chinook / public.Artist',
    value: {
      type: 'table',
      dataSourceName: 'chinook',
      driver: 'postgres',
      table: { name: 'Artist', schema: 'public' },
    },
  },
];

const relationship = {
  name: 'artist',
  fromSource: 'chinook',
  fromTable: { name: 'Album', schema: 'public' },
  type: 'localRelationship',
  relationshipType: 'Object',
  definition: {
    // in another key order than the option's table
    toTable: { schema: 'public', name: 'Artist' },
    mapping: { ArtistId: 'ArtistId' },
  },
} as Relationship;

describe('RelationshipForm Widget', () => {
  it('selects the target table in "To Reference" when editing', () => {
    testRenderWithClient(
      <Theme>
        <Widget
          dataSourceName="chinook"
          table={{ name: 'Album', schema: 'public' }}
          onCancel={vi.fn()}
          onSuccess={vi.fn()}
          onError={vi.fn()}
          defaultValue={relationship}
          sourceOptions={sourceOptions}
          inconsistentSources={[]}
        />
      </Theme>,
    );

    expect(screen.getByText('chinook / public.Artist')).toBeInTheDocument();
  });

  it('opens the edit dialog with the relationship', () => {
    testRenderWithClient(
      <Theme>
        <RenderWidget
          dataSourceName="chinook"
          table={{ name: 'Album', schema: 'public' }}
          mode={MODE.EDIT}
          relationship={relationship}
          onCancel={vi.fn()}
          onSuccess={vi.fn()}
          onError={vi.fn()}
        />
      </Theme>,
    );

    expect(screen.getByDisplayValue('artist')).toBeInTheDocument();
    expect(screen.getByText('chinook / public.Artist')).toBeInTheDocument();
  });
});
