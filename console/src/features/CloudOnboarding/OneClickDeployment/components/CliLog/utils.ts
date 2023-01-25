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
          return 'Cloning assets from GitHub failed';
        default:
          return 'Cloning assets from GitHub';
      }
    case OneClickDeploymentState.ReadingEnvironmentVariables:
      switch (status.kind) {
        case 'success':
          return 'Project Environment Variables set successfully';
        case 'error':
          return 'Failed setting Environment Variables for the project';
        default:
          return 'Setting Environment Variables for the project...';
      }
    case OneClickDeploymentState.AwaitingEnvironmentVariables:
      return 'Please add the required Environment Variables...';
    case OneClickDeploymentState.SufficientEnvironmentVariables:
      return 'Required Environment Variables are present in the project';
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
      return 'There was a problem initializing the deployment';
    case OneClickDeploymentState.CloningGitRepository:
      return 'There was a problem cloning assets from GitHub';
    case OneClickDeploymentState.ReadingEnvironmentVariables:
      return 'There was a problem getting the required Environment Variables for this deployment';
    case OneClickDeploymentState.ApplyingMetadataMigrationsSeeds:
      return 'There was a problem applying the assets from the given git repository to your project';
    default:
      return 'There was an unexpected problem with deploying your project';
  }
};
