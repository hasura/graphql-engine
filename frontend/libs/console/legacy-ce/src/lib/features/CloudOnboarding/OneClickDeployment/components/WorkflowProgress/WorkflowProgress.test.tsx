import { act, render, screen } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { WorkflowProgress } from './WorkflowProgress';
import { OneClickDeploymentState, ProgressState } from '../../types';

// ---------------------------------------------------------------------------
// Controllable progress state + subscription plumbing
// ---------------------------------------------------------------------------
const buildProgress = (awaiting: boolean): ProgressState =>
  ({
    [OneClickDeploymentState.AwaitingEnvironmentVariables]: awaiting
      ? { kind: 'awaiting', payload: { envs: [{ Name: 'PG_URL' }] } }
      : { kind: 'idle' },
    [OneClickDeploymentState.ApplyingMetadataMigrationsSeeds]: { kind: 'idle' },
    [OneClickDeploymentState.SufficientEnvironmentVariables]: { kind: 'idle' },
    [OneClickDeploymentState.Completed]: { kind: 'idle' },
  }) as unknown as ProgressState;

let currentProgress: ProgressState = buildProgress(false);

vi.mock('../../util', () => ({
  getCliProgressState: () => currentProgress,
  shouldTriggerFirstDeployment: () => false,
  isHasuraPath: () => true,
}));

let capturedOnData: ((data: any) => void) | undefined;
const unsubscribeSpy = vi.fn();
const subscribeSpy = vi.fn(
  (_query: unknown, _vars: unknown, onData: (data: any) => void) => {
    capturedOnData = onData;
    return { unsubscribe: unsubscribeSpy };
  },
);

vi.mock('../../../../ControlPlane', async (importOriginal) => {
  const actual = await importOriginal<Record<string, unknown>>();
  return {
    ...actual,
    controlPlaneClient: {
      subscribe: (query: unknown, vars: unknown, onData: (data: any) => void) =>
        subscribeSpy(query, vars, onData),
    },
  };
});

const triggerDeploymentSpy = vi.fn();
vi.mock('../../hooks', () => ({
  useTriggerDeployment: () => ({ triggerDeployment: triggerDeploymentSpy }),
  useSampleQuery: () => '',
}));

// Trivial doubles for the heavy children.
vi.mock('../CliScreen/CliScreen', () => ({
  CliScreen: () => <div data-testid="cli-screen" />,
}));
vi.mock('../RedirectCountdown/RedirectCountDown', () => ({
  RedirectCountDown: () => <div data-testid="redirect-countdown" />,
}));
vi.mock('../Disclaimer/Disclaimer', () => ({
  Disclaimer: () => <div data-testid="disclaimer" />,
}));
vi.mock('../EnvVarsForm/EnvVarsForm', () => ({
  EnvVarsForm: ({ envVars }: { envVars: Array<{ Name: string }> }) => (
    <div data-testid="env-vars-form">
      {envVars.map((e) => e.Name).join(',')}
    </div>
  ),
}));

const renderWorkflow = () => {
  const client = new QueryClient();
  const refetchSpy = vi.spyOn(client, 'refetchQueries').mockResolvedValue();
  const setStepperIndex = vi.fn();
  const utils = render(
    <QueryClientProvider client={client}>
      <WorkflowProgress
        setStepperIndex={setStepperIndex}
        deploymentId="deployment-123"
        projectId="project-1"
        gitRepoName="hasura/repo"
        sampleQueriesFileUrl="url"
        fallbackApps={[]}
      />
    </QueryClientProvider>,
  );
  return { refetchSpy, setStepperIndex, ...utils };
};

describe('WorkflowProgress', () => {
  beforeEach(() => {
    currentProgress = buildProgress(false);
    capturedOnData = undefined;
    subscribeSpy.mockClear();
    unsubscribeSpy.mockClear();
    triggerDeploymentSpy.mockClear();
  });

  it('renders the CLI screen and no env-vars form initially', () => {
    renderWorkflow();
    expect(screen.getByTestId('cli-screen')).toBeInTheDocument();
    expect(screen.queryByTestId('env-vars-form')).not.toBeInTheDocument();
  });

  it('subscribes exactly once using the deploymentId', () => {
    renderWorkflow();
    expect(subscribeSpy).toHaveBeenCalledTimes(1);
    // second arg carries the subscription variables with the deployment id
    expect(subscribeSpy.mock.calls[0][1]).toEqual({ id: 'deployment-123' });
  });

  it('shows the env-vars form and refetches tenant envs when state becomes awaiting', () => {
    const { refetchSpy } = renderWorkflow();
    expect(screen.queryByTestId('env-vars-form')).not.toBeInTheDocument();

    currentProgress = buildProgress(true);
    act(() => {
      capturedOnData?.({
        one_click_deployment_by_pk: { one_click_deployment_state_logs: [] },
      });
    });

    const form = screen.getByTestId('env-vars-form');
    expect(form).toBeInTheDocument();
    expect(form).toHaveTextContent('PG_URL');
    expect(refetchSpy).toHaveBeenCalled();
  });

  it('hides the env-vars form again when state leaves awaiting', () => {
    renderWorkflow();

    currentProgress = buildProgress(true);
    act(() => {
      capturedOnData?.({
        one_click_deployment_by_pk: { one_click_deployment_state_logs: [] },
      });
    });
    expect(screen.getByTestId('env-vars-form')).toBeInTheDocument();

    currentProgress = buildProgress(false);
    act(() => {
      capturedOnData?.({
        one_click_deployment_by_pk: { one_click_deployment_state_logs: [] },
      });
    });
    expect(screen.queryByTestId('env-vars-form')).not.toBeInTheDocument();
  });

  it('unsubscribes on unmount', () => {
    const { unmount } = renderWorkflow();
    unmount();
    expect(unsubscribeSpy).toHaveBeenCalled();
  });
});
