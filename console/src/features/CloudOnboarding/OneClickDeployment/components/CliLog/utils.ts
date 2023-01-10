import {
  OneClickDeploymentState,
  ProgressStateStatus,
  UserFacingStep,
} from '../../types';

export const getStepText = (
  step: UserFacingStep,
  status: ProgressStateStatus
) => {
  switch (step) {
    case OneClickDeploymentState.Initialized:
      switch (status.kind) {
        case 'success':
          return 'Initialized';
        case 'error':
          return 'Failed initializing';
        default:
          return 'Initializing...';
      }
    case OneClickDeploymentState.CloningGitRepository:
      switch (status.kind) {
        case 'success':
          return 'Cloned assets from GitHub';
        case 'error':
          return 'Cloning assets from GitHub';
        default:
          return 'Cloning assets from GitHub';
      }
    case OneClickDeploymentState.ReadingEnvironmentVariables:
      switch (status.kind) {
        case 'success':
          return 'Project environment variables loaded successfully';
        case 'error':
          return 'Failed loading environment variables for the project';
        default:
          return 'Loading environment variables for the project...';
      }
    case OneClickDeploymentState.AwaitingEnvironmentVariables:
      return 'Please add the required environment variables...';
    case OneClickDeploymentState.SufficientEnvironmentVariables:
      return 'Required environment variables are present in the project';
    case OneClickDeploymentState.ApplyingMetadataMigrationsSeeds:
      switch (status.kind) {
        case 'success':
          return 'Deployed project successfully';
        case 'error':
          return 'Failed deploying project';
        default:
          return 'Deploying project...';
      }
    case OneClickDeploymentState.Completed:
      return 'Your project is ready to go!';
    default:
      return null;
  }
};

export const getErrorText = (uFacingStep: UserFacingStep) => {
  switch (uFacingStep) {
    case OneClickDeploymentState.Initialized:
      return 'There was a problem initialising the deployment.';
    case OneClickDeploymentState.CloningGitRepository:
      return 'There was a problem getting the metadata and migrations from GitHub.';
    case OneClickDeploymentState.ReadingEnvironmentVariables:
      return 'There was a problem getting the required environment variables for this deployment.';
    case OneClickDeploymentState.ApplyingMetadataMigrationsSeeds:
      return 'There was a problem applying the assets from the given git repository to your project.';
    default:
      return 'There was an unexpected problem with deploying your project';
  }
};
