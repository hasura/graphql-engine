import { render, screen } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { AccessType } from '@hasura/shared/types';
import PermissionEditor from './PermissionEditor';

// Regression for the no-constant-condition fix: the partial-permissions branch
// used to be `=== 'partialAccess' || 'partialAccessWarning'`, which is always
// truthy, so EVERY access type wrongly rendered the "Partial permissions"
// message (including noAccess and fullAccess). These tests pin the intended
// behaviour for all four AccessTypes so the always-truthy form cannot return.
//
// Discriminators (robust against the message being split across text nodes):
//  - the bold <b>select</b> word is rendered ONLY in the partial branch;
//  - a "Save" button is rendered ONLY for noAccess (every other type shows
//    "Remove").
const renderEditor = (permissionAccessInMetadata: AccessType) =>
  render(
    <Theme>
      <PermissionEditor
        role="user"
        isEditing
        closeFn={vi.fn()}
        saveFn={vi.fn()}
        removeFn={vi.fn()}
        permissionAccessInMetadata={permissionAccessInMetadata}
        table="orders"
        isSaving={false}
        isDeleting={false}
      />
    </Theme>,
  );

const partialMarker = () => screen.queryByText('select');
const saveButton = () => screen.queryByRole('button', { name: 'Save' });
const removeButton = () => screen.queryByRole('button', { name: 'Remove' });

describe('PermissionEditor access-type branches', () => {
  it('partialAccess: shows the partial-permissions message, offers Remove', () => {
    renderEditor('partialAccess');
    expect(partialMarker()).toBeInTheDocument();
    expect(removeButton()).toBeInTheDocument();
    expect(saveButton()).not.toBeInTheDocument();
  });

  it('partialAccessWarning: shows the partial-permissions message, offers Remove', () => {
    renderEditor('partialAccessWarning');
    expect(partialMarker()).toBeInTheDocument();
    expect(removeButton()).toBeInTheDocument();
    expect(saveButton()).not.toBeInTheDocument();
  });

  it('noAccess: does NOT show the partial message (regression), offers Save', () => {
    renderEditor('noAccess');
    expect(partialMarker()).not.toBeInTheDocument();
    expect(saveButton()).toBeInTheDocument();
    expect(removeButton()).not.toBeInTheDocument();
  });

  it('fullAccess: does NOT show the partial message (regression), offers Remove', () => {
    renderEditor('fullAccess');
    expect(partialMarker()).not.toBeInTheDocument();
    expect(removeButton()).toBeInTheDocument();
    expect(saveButton()).not.toBeInTheDocument();
  });
});
