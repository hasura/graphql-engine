import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { z } from 'zod';

// ConnectGDCSourceWidget derives its validation `schema` from the loaded config
// (via useMemo) instead of mirroring it into state in an effect. These mocks
// keep the heavy data/UI surface out of the way so the test can assert that the
// derived schema is actually wired into the form resolver and that the form
// renders on first paint (no skeleton flash loop).
const { createConnection } = vi.hoisted(() => ({ createConnection: vi.fn() }));

vi.mock('@hasura/metadata/data-source', () => ({
  useAvailableDrivers: () => ({
    data: [{ name: 'snowflake', displayName: 'Snowflake' }],
    isLoading: false,
    isError: false,
    error: undefined,
  }),
}));

vi.mock('@hasura/metadata/api', () => ({
  useMetadata: () => ({
    data: undefined,
    isLoading: false,
    isError: false,
    error: undefined,
  }),
}));

vi.mock('../../hooks/useManageDatabaseConnection', () => ({
  useManageDatabaseConnection: () => ({
    createConnection,
    editConnection: vi.fn(),
    isPending: false,
  }),
}));

vi.mock('./useFormValidationSchema', () => ({
  useFormValidationSchema: () => ({
    data: {
      validationSchema: z.object({
        name: z.string().min(1, { message: 'Name is required' }),
      }),
      configSchemas: { configSchema: {}, otherSchemas: {} },
    },
    isLoading: false,
    isError: false,
    error: undefined,
  }),
  // Used by the real generateGDCRequestPayload on the valid-submit path.
  templateVariableArrayToMap: () => ({}),
}));

// Keep the real form primitives (useConsoleForm, InputField, Button, Tabs...)
// but stub the OpenAPI-driven config form which needs a full JSON-schema.
vi.mock('@hasura/shared/ui', async (importActual) => {
  const actual = await importActual<any>();
  return { ...actual, OpenApi3Form: () => <div data-testid="openapi-form" /> };
});

vi.mock('./components/Template', () => ({ Template: () => null }));
vi.mock('./components/Timeout', () => ({ Timeout: () => null }));
vi.mock('./components/TemplateVariables', () => ({
  TemplateVariables: () => null,
}));
vi.mock('../GraphQLCustomization/GraphQLCustomization', () => ({
  GraphQLCustomization: () => null,
}));

import { ConnectGDCSourceWidget } from './ConnectGDCSourceWidget';

describe('ConnectGDCSourceWidget (derived validation schema)', () => {
  beforeEach(() => createConnection.mockReset());

  it('renders the connect form on first paint (schema derived during render)', () => {
    render(<ConnectGDCSourceWidget driver="snowflake" />);

    expect(screen.getByText('Connect Snowflake Database')).toBeInTheDocument();
    // The schema-derived form is active on the first render (no skeleton flash).
    expect(screen.getByPlaceholderText('Database name')).toBeInTheDocument();
    expect(screen.getByTestId('openapi-form')).toBeInTheDocument();
  });

  it('applies the derived schema as the form resolver (blocks submit on invalid input)', async () => {
    const user = userEvent.setup();
    render(<ConnectGDCSourceWidget driver="snowflake" />);

    await user.click(screen.getByRole('button', { name: /connect database/i }));

    // The derived zod schema (name required) must gate submission: with an empty
    // name the valid-submit handler (createConnection) must not run.
    await new Promise((resolve) => setTimeout(resolve, 100));
    expect(createConnection).not.toHaveBeenCalled();

    // Sanity: a valid name lets the submit through, proving the resolver is the
    // one gating (not a broken form).
    await user.type(screen.getByPlaceholderText('Database name'), 'my_db');
    await user.click(screen.getByRole('button', { name: /connect database/i }));
    await waitFor(() => expect(createConnection).toHaveBeenCalledTimes(1));
  });
});
