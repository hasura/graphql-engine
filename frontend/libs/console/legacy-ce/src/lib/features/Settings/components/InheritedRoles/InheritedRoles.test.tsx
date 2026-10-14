import { render } from '@testing-library/react';
import { vi } from 'vitest';
import { METADATA_STATUS_PATH } from '@hasura/shared/types';
import InheritedRoles from './InheritedRoles';

const { navigateMock, useInconsistentMetadataMock } = vi.hoisted(() => ({
  navigateMock: vi.fn(),
  useInconsistentMetadataMock: vi.fn(),
}));

vi.mock('react-router', async (importOriginal) => ({
  ...(await importOriginal<Record<string, unknown>>()),
  useNavigate: () => navigateMock,
}));

vi.mock('@hasura/metadata/api', () => ({
  useMetadata: () => ({
    data: { metadata: { inherited_roles: [] } },
    isLoading: false,
  }),
  useInconsistentMetadata: useInconsistentMetadataMock,
  useDropInheritedRole: () => vi.fn(),
  useUpdateInheritedRole: () => vi.fn(),
  useAddInheritedRole: () => vi.fn(),
}));

vi.mock('@hasura/metadata/helpers', async (importOriginal) => ({
  ...(await importOriginal<Record<string, unknown>>()),
  MetadataSelectors: { getRoles: () => [] },
}));

const withInconsistency = {
  data: {
    inconsistent_objects: [
      { type: 'inherited role permission inconsistency', name: 'bad' },
    ],
  },
  refetch: vi.fn(),
  isLoading: false,
};

const withoutInconsistency = {
  data: { inconsistent_objects: [] },
  refetch: vi.fn(),
  isLoading: false,
};

describe('InheritedRoles inconsistency redirect', () => {
  beforeEach(() => {
    navigateMock.mockReset();
    useInconsistentMetadataMock.mockReset();
  });

  it('does not redirect when there is no inherited-role inconsistency', () => {
    useInconsistentMetadataMock.mockReturnValue(withoutInconsistency);
    render(<InheritedRoles />);
    expect(navigateMock).not.toHaveBeenCalled();
  });

  it('redirects to the metadata status page when an inconsistency is present', () => {
    useInconsistentMetadataMock.mockReturnValue(withInconsistency);
    render(<InheritedRoles />);
    expect(navigateMock).toHaveBeenCalledWith(METADATA_STATUS_PATH);
  });

  it('redirects only once (no per-render runaway) across re-renders', () => {
    useInconsistentMetadataMock.mockReturnValue(withInconsistency);
    const { rerender } = render(<InheritedRoles />);
    rerender(<InheritedRoles />);
    rerender(<InheritedRoles />);
    expect(navigateMock).toHaveBeenCalledTimes(1);
  });
});
