import { Meta, StoryObj } from '@storybook/react-webpack5';
import { expect, screen, userEvent, within } from 'storybook/test';
import { delay, http, HttpResponse } from 'msw';
import LivePreview from './index';
import { HeaderState } from './state';

const pageHost = 'http://localhost:8080/api/rest/users';

const dataHeaders: HeaderState[] = [
  {
    key: 'x-hasura-admin-secret',
    value: 'myadminsecret',
    selected: true,
    isDisabled: false,
    index: 0,
  },
  {
    key: 'x-hasura-role',
    value: 'user',
    selected: true,
    isDisabled: false,
    index: 1,
  },
];

const usersEndpoint = {
  name: 'users',
  url: 'users',
  methods: ['GET' as const],
  definition: {
    query: { query_name: 'users', collection_name: 'allowed-queries' },
  },
  currentQuery: `query users {
  users {
    id
    name
  }
}`,
};

// `:id` is a URL variable, `limit` is sent in the request body.
const userByIdEndpoint = {
  name: 'user_by_id',
  url: 'user/:id',
  methods: ['GET' as const, 'POST' as const],
  definition: {
    query: { query_name: 'user_by_id', collection_name: 'allowed-queries' },
  },
  currentQuery: `query user_by_id($id: Int!, $limit: Int) {
  users(where: { id: { _eq: $id } }, limit: $limit) {
    id
    name
  }
}`,
};

export default {
  title: 'Features/REST endpoints/Live Preview',
  component: LivePreview,
  args: {
    pageHost,
    endpointState: usersEndpoint,
    dataHeaders,
  },
  parameters: {
    msw: [
      http.all(pageHost, () =>
        HttpResponse.json({
          users: [
            { id: 1, name: 'Alice' },
            { id: 2, name: 'Bob' },
          ],
        }),
      ),
      // URL variables are resolved against the current page host, which
      // depends on where Storybook runs, so match any origin.
      http.all('*/api/rest/user/:id', ({ params }) =>
        HttpResponse.json({
          users: [{ id: Number(params['id']), name: 'Alice' }],
        }),
      ),
    ],
  },
} satisfies Meta<typeof LivePreview>;

type Story = StoryObj<typeof LivePreview>;

export const Default: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await expect(
      canvas.getByDisplayValue('x-hasura-admin-secret'),
    ).toBeVisible();
    await expect(
      canvas.getByText("This query doesn't require any request variables"),
    ).toBeVisible();

    await userEvent.click(canvas.getByRole('button', { name: 'Run Request' }));

    await expect(await canvas.findByText(/Alice/)).toBeVisible();
  },
};

export const WithVariables: Story = {
  args: {
    endpointState: userByIdEndpoint,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const idRow = canvas.getByText('id').closest('tr') as HTMLElement;
    await userEvent.type(within(idRow).getByPlaceholderText('Value...'), '1');

    await userEvent.click(canvas.getByRole('button', { name: 'Run Request' }));

    await expect(await canvas.findByText(/Alice/)).toBeVisible();
  },
};

export const NoHeaders: Story = {
  args: {
    dataHeaders: [],
  },
};

export const RequestError: Story = {
  parameters: {
    msw: [http.all(pageHost, () => new HttpResponse(null, { status: 404 }))],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByRole('button', { name: 'Run Request' }));

    await expect(
      await canvas.findByText('The endpoint does not exist'),
    ).toBeVisible();
    await expect(canvas.getByText('404')).toBeVisible();
  },
};

export const Loading: Story = {
  parameters: {
    msw: [
      http.all(pageHost, async () => {
        await delay('infinite');
        return HttpResponse.json({});
      }),
    ],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByRole('button', { name: 'Run Request' }));

    await expect(screen.queryByText(/Alice/)).not.toBeInTheDocument();
  },
};
