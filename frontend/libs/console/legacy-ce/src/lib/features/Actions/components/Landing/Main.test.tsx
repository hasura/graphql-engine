import { render, screen, fireEvent } from '@testing-library/react';
import { MemoryRouter, useLocation } from 'react-router';
import { Theme } from '@radix-ui/themes';
import { AppContext, defaultAppState } from '@hasura/shared/context';
import Landing from './Main';

const LocationDisplay = () => {
  const location = useLocation();
  return <div data-testid="location-display">{location.pathname}</div>;
};

const renderLanding = ({ readOnlyMode = false } = {}) =>
  render(
    <AppContext.Provider value={{ ...defaultAppState, readOnlyMode }}>
      <MemoryRouter initialEntries={['/actions/manage']}>
        <Theme>
          <Landing />
          <LocationDisplay />
        </Theme>
      </MemoryRouter>
    </AppContext.Provider>,
  );

describe('Landing (Actions)', () => {
  it('renders the heading and both call-to-action buttons', () => {
    renderLanding();

    expect(
      screen.getByRole('heading', { name: 'Actions' }),
    ).toBeInTheDocument();
    expect(screen.getByTestId('data-create-actions')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: /import from openapi/i }),
    ).toBeInTheDocument();
  });

  it('navigates to the create-action route when "Create" is clicked', () => {
    renderLanding();

    fireEvent.click(screen.getByTestId('data-create-actions'));

    expect(screen.getByTestId('location-display')).toHaveTextContent(
      '/actions/manage/add',
    );
  });

  it('navigates to the OpenAPI import route when "Import from OpenAPI" is clicked', () => {
    renderLanding();

    fireEvent.click(
      screen.getByRole('button', { name: /import from openapi/i }),
    );

    expect(screen.getByTestId('location-display')).toHaveTextContent(
      '/actions/manage/add-oas',
    );
  });

  it('hides the "Create" button in read-only mode', () => {
    renderLanding({ readOnlyMode: true });

    expect(screen.queryByTestId('data-create-actions')).not.toBeInTheDocument();
    // the import button is still available
    expect(
      screen.getByRole('button', { name: /import from openapi/i }),
    ).toBeInTheDocument();
  });
});
