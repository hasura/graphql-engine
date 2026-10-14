import { render } from '@testing-library/react';
import { EnvVarsForm } from './EnvVarsForm';

// Controllable tenant env data returned by the data hook.
let tenantEnvData: any = undefined;
vi.mock('./hooks', () => ({
  useGetTenantEnvs: () => ({ data: tenantEnvData }),
  useUpdateTenantEnv: () => ({ onSubmitHandler: vi.fn() }),
}));
vi.mock('./utils', () => ({
  getFormProperties: () => ({ schema: {}, defaultValues: {} }),
}));
vi.mock('./components', () => ({
  EnvVarsFormFields: () => <div data-testid="fields" />,
  CustomFooter: () => <div data-testid="footer" />,
}));
vi.mock('@hasura/shared/ui', () => ({
  SimpleForm: ({ children }: { children: React.ReactNode }) => (
    <form>{children}</form>
  ),
  Dialog: ({ children }: { children: React.ReactNode }) => (
    <div>{children}</div>
  ),
  Text: ({ children }: { children: React.ReactNode }) => <p>{children}</p>,
}));

// `useErrorNotification` intentionally returns a NEW function on every render
// (mirroring the real hook). The spy underneath is stable so we can count calls.
const notifySpy = vi.fn();
vi.mock('@hasura/metadata/api', () => ({
  useErrorNotification: () => (args: unknown) => notifySpy(args),
}));

describe('EnvVarsForm', () => {
  beforeEach(() => {
    tenantEnvData = undefined;
    notifySpy.mockClear();
  });

  it('renders without firing a notification when there are no errors', () => {
    tenantEnvData = { data: { getTenantEnv: { hash: 'h', envVars: [] } } };
    render(
      <EnvVarsForm envVars={[]} formState="default" setFormState={vi.fn()} />,
    );
    expect(notifySpy).not.toHaveBeenCalled();
  });

  it('shows the fetch error notification when tenant env data has errors', () => {
    tenantEnvData = { errors: [{ message: 'boom' }] };
    render(
      <EnvVarsForm envVars={[]} formState="default" setFormState={vi.fn()} />,
    );
    expect(notifySpy).toHaveBeenCalledTimes(1);
    expect(notifySpy).toHaveBeenCalledWith({
      title: 'Error fetching Environment Variables',
      error: { message: 'boom' },
    });
  });

  it('does not re-fire the notification on re-render (unstable notifier is not an effect dep)', () => {
    tenantEnvData = { errors: [{ message: 'boom' }] };
    const { rerender } = render(
      <EnvVarsForm envVars={[]} formState="default" setFormState={vi.fn()} />,
    );
    expect(notifySpy).toHaveBeenCalledTimes(1);

    // Re-render (e.g. parent passes a new formState). tenantEnvData is unchanged
    // so the effect must not run again despite the notifier identity changing.
    rerender(
      <EnvVarsForm envVars={[]} formState="loading" setFormState={vi.fn()} />,
    );
    rerender(
      <EnvVarsForm envVars={[]} formState="error" setFormState={vi.fn()} />,
    );
    expect(notifySpy).toHaveBeenCalledTimes(1);
  });
});
