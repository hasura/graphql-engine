import { shouldRedirectToMetadataStatus } from './metadataStatusRedirect';

describe('shouldRedirectToMetadataStatus (EE Main redirect guard)', () => {
  it('does NOT redirect while the query is pending (data undefined)', () => {
    expect(
      shouldRedirectToMetadataStatus({
        hasDataAccess: true,
        isLoading: true,
        inconsistentMetadata: undefined,
      }),
    ).toBe(false);
    // resolved-but-still-undefined guard (defensive)
    expect(
      shouldRedirectToMetadataStatus({
        hasDataAccess: true,
        isLoading: false,
        inconsistentMetadata: undefined,
      }),
    ).toBe(false);
  });

  it('does NOT redirect when metadata is consistent', () => {
    expect(
      shouldRedirectToMetadataStatus({
        hasDataAccess: true,
        isLoading: false,
        inconsistentMetadata: { is_consistent: true },
      }),
    ).toBe(false);
  });

  it('DOES redirect when resolved + inconsistent + has data access', () => {
    expect(
      shouldRedirectToMetadataStatus({
        hasDataAccess: true,
        isLoading: false,
        inconsistentMetadata: { is_consistent: false },
      }),
    ).toBe(true);
  });

  it('does NOT redirect without data access, even when inconsistent', () => {
    expect(
      shouldRedirectToMetadataStatus({
        hasDataAccess: false,
        isLoading: false,
        inconsistentMetadata: { is_consistent: false },
      }),
    ).toBe(false);
  });

  it('reacts to a data transition: pending -> resolved inconsistent', () => {
    // first render: pending
    expect(
      shouldRedirectToMetadataStatus({
        hasDataAccess: true,
        isLoading: true,
        inconsistentMetadata: undefined,
      }),
    ).toBe(false);
    // query resolves inconsistent -> now redirect
    expect(
      shouldRedirectToMetadataStatus({
        hasDataAccess: true,
        isLoading: false,
        inconsistentMetadata: { is_consistent: false },
      }),
    ).toBe(true);
  });

  it('reacts to a re-fetch transition: inconsistent -> consistent (no redirect)', () => {
    expect(
      shouldRedirectToMetadataStatus({
        hasDataAccess: true,
        isLoading: false,
        inconsistentMetadata: { is_consistent: false },
      }),
    ).toBe(true);
    expect(
      shouldRedirectToMetadataStatus({
        hasDataAccess: true,
        isLoading: false,
        inconsistentMetadata: { is_consistent: true },
      }),
    ).toBe(false);
  });
});
