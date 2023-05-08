import { renderHook } from '@testing-library/react-hooks';
import { useDriverRelationshipSupport } from '../useDriverRelationshipSupport';
import { useDriverCapabilities } from '../useDriverCapabilities';
import { useAvailableDrivers } from '../../../ConnectDB';

jest.mock('../useDriverCapabilities');
jest.mock('../../../ConnectDB');

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

describe('useDriverRelationshipSupport', () => {
  beforeEach(() => {
    jest.resetAllMocks();
  });

  it('should always return truthy for native data sources', () => {
    useDriverCapabilities.mockReturnValue(noSupport);

    useAvailableDrivers.mockReturnValue({
      data: [
        {
          name: 'postgres',
          native: true,
        },
      ],
    });

    const { result } = renderHook(() =>
      useDriverRelationshipSupport({ dataSourceName: 'postgres' })
    );

    expect(result.current.driverSupportsLocalRelationship).toBe(true);
    expect(result.current.driverSupportsRemoteRelationship).toBe(true);
  });

  it('should return a true for local relationship support for non native source', () => {
    useDriverCapabilities.mockReturnValue(localSupport);

    useAvailableDrivers.mockReturnValue({
      data: [
        {
          name: 'MySQL',
          native: false,
        },
      ],
    });

    const { result } = renderHook(() =>
      useDriverRelationshipSupport({ dataSourceName: 'MySQL' })
    );

    expect(result.current.driverSupportsLocalRelationship).toBe(true);
    expect(result.current.driverSupportsRemoteRelationship).toBe(false);
  });

  it('should return a true for remote relationship support for non native source', () => {
    useDriverCapabilities.mockReturnValue(remoteSupport);

    useAvailableDrivers.mockReturnValue({
      data: [
        {
          name: 'MySQL',
          native: false,
        },
      ],
    });

    const { result } = renderHook(() =>
      useDriverRelationshipSupport({ dataSourceName: 'MySQL' })
    );

    expect(result.current.driverSupportsLocalRelationship).toBe(false);
    expect(result.current.driverSupportsRemoteRelationship).toBe(true);
  });

  it('should return a true for remote and local relationship support for non native source', () => {
    useDriverCapabilities.mockReturnValue(fullSupport);

    useAvailableDrivers.mockReturnValue({
      data: [
        {
          name: 'MySQL',
          native: false,
        },
      ],
    });

    const { result } = renderHook(() =>
      useDriverRelationshipSupport({ dataSourceName: 'MySQL' })
    );

    expect(result.current.driverSupportsLocalRelationship).toBe(true);
    expect(result.current.driverSupportsRemoteRelationship).toBe(true);
  });
});
