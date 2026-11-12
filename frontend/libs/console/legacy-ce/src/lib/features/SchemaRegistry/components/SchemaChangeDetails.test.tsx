import { render, screen, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';
import { MemoryRouter } from 'react-router';
import { SchemaDetailsPanel } from './SchemaChangeDetails';
import { sampleSchema, schemaWithChanges } from './SchemaRegistry.fixtures';

const renderDetails = (schema = sampleSchema) =>
  render(
    <Theme>
      <MemoryRouter
        basename="/console"
        initialEntries={['/console/api/schema-registry/admin-schema']}
      >
        <SchemaDetailsPanel schema={schema} />
      </MemoryRouter>
    </Theme>,
  );

describe('SchemaDetailsPanel', () => {
  it('keeps severity labels beside their counts', () => {
    renderDetails();
    const list = screen.getByLabelText('Change summary');
    for (const label of ['Breaking', 'Dangerous', 'Safe', 'Total']) {
      expect(within(list).getByText(label)).toBeVisible();
    }
    expect(within(list).getByText('3')).toBeVisible();
  });

  it('distinguishes an unavailable comparison from zero changes', () => {
    renderDetails(schemaWithChanges(undefined));
    expect(screen.getByText('Comparison unavailable')).toBeVisible();
    expect(screen.getAllByText('Unavailable')).toHaveLength(4);
    expect(screen.queryByText('0')).not.toBeInTheDocument();
  });

  it('explains an empty comparison', () => {
    renderDetails(schemaWithChanges([]));
    expect(screen.getByText('No changes detected')).toBeVisible();
    expect(screen.getAllByText('0')).toHaveLength(4);
  });

  it('combines severity and search, reports no results, and clears both', async () => {
    const user = userEvent.setup();
    renderDetails();
    await user.selectOptions(
      screen.getByLabelText('Change severity'),
      'BREAKING',
    );
    expect(screen.getByText('Showing 1 of 3 changes')).toBeVisible();
    expect(
      screen.queryByText(/Field 'display_name' was added/),
    ).not.toBeInTheDocument();
    await user.type(screen.getByLabelText('Search changes'), 'missing');
    expect(screen.getByText('No matching changes')).toBeVisible();
    await user.click(screen.getByRole('button', { name: 'Clear filters' }));
    expect(screen.getByText('Showing 3 of 3 changes')).toBeVisible();
    expect(screen.getByLabelText('Search changes')).toHaveValue('');
    expect(screen.getByLabelText('Change severity')).toHaveValue('all');
  });

  it('offers All changes after selecting a severity', async () => {
    const user = userEvent.setup();
    renderDetails();
    await user.selectOptions(
      screen.getByLabelText('Change severity'),
      'DANGEROUS',
    );
    await user.selectOptions(screen.getByLabelText('Change severity'), 'all');
    expect(screen.getByText('Showing 3 of 3 changes')).toBeVisible();
  });

  it('copies a canonical schema URL including the console prefix exactly once', async () => {
    const user = userEvent.setup();
    renderDetails();
    const writeText = vi
      .spyOn(navigator.clipboard, 'writeText')
      .mockResolvedValue();
    await user.click(screen.getByRole('button', { name: 'Share schema' }));
    expect(writeText).toHaveBeenCalledWith(
      `${window.location.origin}/console/api/schema-registry/admin-schema`,
    );
    expect(screen.getByText('Schema link copied to clipboard.')).toBeVisible();
  });

  it('handles asynchronous clipboard rejection without claiming success', async () => {
    const user = userEvent.setup();
    renderDetails();
    vi.spyOn(navigator.clipboard, 'writeText').mockRejectedValue(
      new Error('Clipboard denied'),
    );
    await user.click(screen.getByRole('button', { name: 'Share schema' }));
    expect(
      screen.getByText('Unable to copy link. Please try again.'),
    ).toBeVisible();
    expect(screen.queryByText('Link copied')).not.toBeInTheDocument();
  });
});
