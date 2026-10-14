import { render, screen } from '@testing-library/react';
import { useForm, FormProvider, useWatch } from 'react-hook-form';
import { PgDatabaseField } from './PgDatabaseField';
import { RequiredEnvVar } from '../../../../types';
import { NeonIntegrationStatus } from '../../../../../NeonOnboardingWizard/hooks/useNeonIntegration';

// Drive the integration status from the test.
let mockStatus: NeonIntegrationStatus;
vi.mock('../../hooks', () => ({
  useNeonIntegrationForOneClickDeployment: () => mockStatus,
}));

// Keep the child components trivial so we can assert the props PgDatabaseField
// derives (neonDBURL, toggle visibility) without their heavy ui dependencies.
vi.mock('./InputWrapper', () => ({
  InputWrapper: ({ neonDBURL }: { neonDBURL: string }) => (
    <div data-testid="neon-db-url">{neonDBURL}</div>
  ),
}));
vi.mock('./InputModeToggle', () => ({
  InputModeToggle: () => <div data-testid="input-mode-toggle" />,
}));

const dbEnvVar: RequiredEnvVar = {
  Kind: 'ENV_TYPE_DATABASE',
  Name: 'PG_DATABASE_URL',
  Description: 'Postgres database url',
};

// Reflects the current form value into the DOM so the test can assert the
// side-effect write into react-hook-form without reaching outside the tree.
function ValueSpy() {
  const value = useWatch({ name: 'PG_DATABASE_URL' });
  return <div data-testid="form-value">{value ?? ''}</div>;
}

function Harness() {
  const methods = useForm();
  return (
    <FormProvider {...methods}>
      <PgDatabaseField dbEnvVar={dbEnvVar} />
      <ValueSpy />
    </FormProvider>
  );
}

describe('PgDatabaseField', () => {
  it('renders with an empty derived url and the toggle visible before success', () => {
    mockStatus = { status: 'idle', action: () => undefined };
    render(<Harness />);

    expect(screen.getByTestId('neon-db-url')).toHaveTextContent('');
    expect(screen.getByTestId('input-mode-toggle')).toBeInTheDocument();
    expect(screen.getByTestId('form-value')).toHaveTextContent('');
  });

  it('derives the db url and writes it into the form once integration succeeds', () => {
    mockStatus = {
      status: 'neon-database-creation-success',
      payload: {
        databaseUrl: 'postgres://user:pass@host/db',
        email: 'a@b.com',
      },
    };
    render(<Harness />);

    // derived during render and passed to the child
    expect(screen.getByTestId('neon-db-url')).toHaveTextContent(
      'postgres://user:pass@host/db',
    );
    // the toggle is hidden once a url exists
    expect(screen.queryByTestId('input-mode-toggle')).not.toBeInTheDocument();
    // side effect: form value populated
    expect(screen.getByTestId('form-value')).toHaveTextContent(
      'postgres://user:pass@host/db',
    );
  });

  it('transitions from empty to populated when status changes to success', () => {
    mockStatus = { status: 'idle', action: () => undefined };
    const { rerender } = render(<Harness />);
    expect(screen.getByTestId('neon-db-url')).toHaveTextContent('');

    mockStatus = {
      status: 'neon-database-creation-success',
      payload: { databaseUrl: 'postgres://new/db', email: '' },
    };
    rerender(<Harness />);

    expect(screen.getByTestId('neon-db-url')).toHaveTextContent(
      'postgres://new/db',
    );
    expect(screen.getByTestId('form-value')).toHaveTextContent(
      'postgres://new/db',
    );
  });
});
