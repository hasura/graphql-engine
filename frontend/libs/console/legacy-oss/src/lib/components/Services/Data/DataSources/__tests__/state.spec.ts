import type { GraphQLFieldCustomization } from '../../../../../metadata/types';
import type { ConnectDBActions, ConnectDBState } from '../state';
import { connectDBReducer, removeEmptyValues } from '../state';

const initialState: ConnectDBState = {
  displayName: 'displayName',
  dbType: 'postgres',
  connectionParamState: {
    host: 'anHost',
    port: 'aPort',
    username: 'aUsername',
    password: 'aPassword',
    database: 'aDatabase',
  },
  databaseURLState: {
    dbURL: 'aDbUrl',
    serviceAccount: 'aServiceAccount',
    global_select_limit: 10,
    projectId: 'aProjectId',
    datasets: 'aDataset',
  },
  envVarState: {
    envVar: 'anEnvVar',
  },
};

type StateTest = {
  action: ConnectDBActions;
  expectedState: GraphQLFieldCustomization;
  expectedCleanedState: GraphQLFieldCustomization;
};

describe('removeEmptyValues', () => {
  it('removes empty string values from an object', () => {
    expect(
      removeEmptyValues({
        a: 1,
        b: 0,
        c: '',
        d: null,
        e: {
          f: '',
        },
      })
    ).toEqual({
      a: 1,
      b: 0,
      d: null,
      e: {
        f: '',
      },
    });
  });
});

describe('Data state', () => {
  it('handles INIT action', () => {
    const state = connectDBReducer(initialState, {
      type: 'INIT',
      data: {
        name: 'name',
        driver: 'postgres',
        databaseUrl: 'url',
        connectionParamState: {
          host: 'anHost',
          port: 'aPort',
          username: 'aUsername',
          password: 'aPassword',
          database: 'aDatabase',
        },
        connectionSettings: undefined,
        preparedStatements: false,
        isolationLevel: 'read-committed',
        sslConfiguration: undefined,
      },
    });

    expect(state).toEqual({
      displayName: 'name',
      envVarState: {
        envVar: 'anEnvVar',
      },
      dbType: 'postgres',
      databaseURLState: {
        datasets: 'aDataset',
        dbURL: 'url',
        global_select_limit: 10,
        projectId: 'aProjectId',
        serviceAccount: 'aServiceAccount',
      },
      connectionParamState: {
        host: 'anHost',
        port: 'aPort',
        username: 'aUsername',
        password: 'aPassword',
        database: 'aDatabase',
      },
      connectionSettings: undefined,
      preparedStatements: false,
      isolationLevel: 'read-committed',
      sslConfiguration: undefined,
    });
  });

  const baseCustomization = {
    rootFields: {
      namespace: 'aNamespace',
      prefix: 'aPrefix',
      suffix: 'aSuffix',
    },
    typeNames: {
      prefix: 'aTypeNamePrefix',
      suffix: 'aTypeNameSuffix',
    },
  };

  const actionTests: StateTest[] = [
    {
      action: {
        type: 'UPDATE_CUSTOMIZATION_ROOT_FIELDS_NAMESPACE',
        data: 'aNamespace',
      },
      expectedState: {
        rootFields: { namespace: 'aNamespace' },
      },
      expectedCleanedState: {
        rootFields: {
          prefix: baseCustomization.rootFields.prefix,
          suffix: baseCustomization.rootFields.suffix,
        },
        typeNames: baseCustomization.typeNames,
      },
    },
    {
      action: {
        type: 'UPDATE_CUSTOMIZATION_ROOT_FIELDS_PREFIX',
        data: 'aPrefix',
      },
      expectedState: {
        rootFields: { prefix: 'aPrefix' },
      },
      expectedCleanedState: {
        rootFields: {
          namespace: baseCustomization.rootFields.namespace,
          suffix: baseCustomization.rootFields.suffix,
        },
        typeNames: baseCustomization.typeNames,
      },
    },
    {
      action: {
        type: 'UPDATE_CUSTOMIZATION_ROOT_FIELDS_SUFFIX',
        data: 'aSuffix',
      },
      expectedState: {
        rootFields: { suffix: 'aSuffix' },
      },
      expectedCleanedState: {
        rootFields: {
          namespace: baseCustomization.rootFields.namespace,
          prefix: baseCustomization.rootFields.prefix,
        },
        typeNames: baseCustomization.typeNames,
      },
    },
    {
      action: {
        type: 'UPDATE_CUSTOMIZATION_TYPE_NAMES_PREFIX',
        data: 'aTypeNamePrefix',
      },
      expectedState: {
        typeNames: { prefix: 'aTypeNamePrefix' },
      },
      expectedCleanedState: {
        rootFields: baseCustomization.rootFields,
        typeNames: {
          suffix: baseCustomization.typeNames.suffix,
        },
      },
    },
    {
      action: {
        type: 'UPDATE_CUSTOMIZATION_TYPE_NAMES_SUFFIX',
        data: 'aTypeNameSuffix',
      },
      expectedState: {
        typeNames: { suffix: 'aTypeNameSuffix' },
      },
      expectedCleanedState: {
        rootFields: baseCustomization.rootFields,
        typeNames: {
          prefix: baseCustomization.typeNames.prefix,
        },
      },
    },
  ];

  actionTests.forEach(actionTest => {
    describe(`when action type is ${actionTest.action.type}`, () => {
      it('updates the state', () => {
        const state = connectDBReducer(initialState, actionTest.action);

        expect(state.customization).toEqual(actionTest.expectedState);
      });

      it('removes empty values', () => {
        const state = connectDBReducer(
          {
            ...initialState,
            customization: baseCustomization,
          },
          {
            ...actionTest.action,
            data: '',
          } as ConnectDBActions
        );

        expect(state.customization).toEqual(actionTest.expectedCleanedState);
      });
    });
  });
});
