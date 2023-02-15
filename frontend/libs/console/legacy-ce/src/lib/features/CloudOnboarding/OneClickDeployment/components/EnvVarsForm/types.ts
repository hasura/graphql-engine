import { GraphQLError } from 'graphql';
import React from 'react';
import {
  GetTenantEnvQuery,
  UpdateTenantMutation,
} from '../../../../ControlPlane/generatedGraphQLTypes';

type Status =
  | {
      status: 'loading';
    }
  | {
      status: 'error';
      errorTitle: string;
      errorDescription: string | React.ReactNode;
    }
  | {
      status: 'default';
    }
  | {
      status: 'success';
    };

export type NeonButtonIcons = 'refresh' | 'loading' | 'create';

export type NeonButtonProps = {
  status: Status;
  onClickConnect: VoidFunction;
  buttonText: string;
  icon?: NeonButtonIcons;
  dbURL?: string;
};

export type UpdateEnvObj = {
  key: string;
  value: string;
};

export type GetTenantEnvResponse = {
  data?: GetTenantEnvQuery;
  errors?: GraphQLError[];
};

export type UpdateTenantEnvResponse = {
  data?: UpdateTenantMutation;
  errors?: GraphQLError[];
};
