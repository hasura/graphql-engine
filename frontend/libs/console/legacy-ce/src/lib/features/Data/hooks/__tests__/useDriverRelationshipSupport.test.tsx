import { renderHook } from '@testing-library/react';
import { useDriverRelationshipSupport } from '../useDriverRelationshipSupport';
import {
  useAvailableDrivers,
  useDriverCapabilities,
} from '@hasura/metadata/data-source';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { useMetadata } from '@hasura/metadata/api';
import { vi } from 'vitest';
import type { Mock } from 'vitest';

vi.mock('@hasura/metadata/data-source');
vi.mock('@hasura/metadata/api');

const mockUseMetadata = useMetadata as unknown as Mock;
const mockUseDriverCapabilities = useDriverCapabilities as unknown as Mock;
const mockUseAvailableDrivers = useAvailableDrivers as unknown as Mock;

const fullSupport = {
  data: {
    relationships: {},
    queries: {
      foreach: {},
    },
  },
};

const localSupport = {
  data: {
    relationships: {},
  },
};

const remoteSupport = {
  data: {
    queries: {
      foreach: {},
    },
  },
};

const noSupport = { data: {} };

const queryClient = new QueryClient();
const wrapper = ({ children }: { children: React.ReactNode }) => (
  <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
);

describe('useDriverRelationshipSupport', () => {
  beforeEach(() => {
    vi.resetAllMocks();
  });

  it('should always return truthy for native data sources', () => {
    mockUseMetadata.mockReturnValue({
      data: {
        kind: 'postgres',
      },
    });
    mockUseDriverCapabilities.mockReturnValue(noSupport);

    mockUseAvailableDrivers.mockReturnValue({
      data: [
        {
          name: 'postgres',
          native: true,
        },
      ],
    });

    const { result } = renderHook(
      () => useDriverRelationshipSupport({ dataSourceName: 'postgres' }),
      { wrapper },
    );

    expect(result.current.driverSupportsLocalRelationship).toBe(true);
    expect(result.current.driverSupportsRemoteRelationship).toBe(true);
  });

  it('should return a true for local relationship support for non native source', () => {
    mockUseMetadata.mockReturnValue({
      data: {
        kind: 'mysql',
      },
    });

    mockUseDriverCapabilities.mockReturnValue(localSupport);

    mockUseAvailableDrivers.mockReturnValue({
      data: [
        {
          name: 'MySQL',
          native: false,
        },
      ],
    });

    const { result } = renderHook(() =>
      useDriverRelationshipSupport({ dataSourceName: 'MySQL' }),
    );

    expect(result.current.driverSupportsLocalRelationship).toBe(true);
    expect(result.current.driverSupportsRemoteRelationship).toBe(false);
  });

  it('should return a true for remote relationship support for non native source', () => {
    mockUseMetadata.mockReturnValue({
      data: {
        kind: 'mysql',
      },
    });
    mockUseDriverCapabilities.mockReturnValue(remoteSupport);

    mockUseAvailableDrivers.mockReturnValue({
      data: [
        {
          name: 'MySQL',
          native: false,
        },
      ],
    });

    const { result } = renderHook(() =>
      useDriverRelationshipSupport({ dataSourceName: 'MySQL' }),
    );

    expect(result.current.driverSupportsLocalRelationship).toBe(false);
    expect(result.current.driverSupportsRemoteRelationship).toBe(true);
  });

  it('should return a true for remote and local relationship support for non native source', () => {
    mockUseMetadata.mockReturnValue({
      data: {
        kind: 'mysql',
      },
    });
    mockUseDriverCapabilities.mockReturnValue(fullSupport);

    mockUseAvailableDrivers.mockReturnValue({
      data: [
        {
          name: 'MySQL',
          native: false,
        },
      ],
    });

    const { result } = renderHook(() =>
      useDriverRelationshipSupport({ dataSourceName: 'MySQL' }),
    );

    expect(result.current.driverSupportsLocalRelationship).toBe(true);
    expect(result.current.driverSupportsRemoteRelationship).toBe(true);
  });
});
