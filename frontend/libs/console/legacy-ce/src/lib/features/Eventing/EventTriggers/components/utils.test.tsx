import { render, screen } from '@testing-library/react';
import {
  getEventStatusIcon,
  getEventDeliveryIcon,
  getInvocationLogStatus,
} from './utils';

describe('getEventStatusIcon', () => {
  it('renders a titled clock for a scheduled event', () => {
    render(<>{getEventStatusIcon('scheduled')}</>);
    expect(
      screen.getByTitle('This event has been scheduled'),
    ).toBeInTheDocument();
  });

  it('renders a titled warning for a dead event', () => {
    render(<>{getEventStatusIcon('dead')}</>);
    expect(
      screen.getByTitle('This event is dead and will never be delivered'),
    ).toBeInTheDocument();
  });

  it('renders a titled check for a delivered event', () => {
    render(<>{getEventStatusIcon('delivered')}</>);
    expect(
      screen.getByTitle('This event has been delivered'),
    ).toBeInTheDocument();
  });

  it('renders a titled cross for an errored event', () => {
    render(<>{getEventStatusIcon('error')}</>);
    expect(
      screen.getByTitle('This event failed with an error'),
    ).toBeInTheDocument();
  });

  it('renders nothing for an unknown status', () => {
    const { container } = render(<>{getEventStatusIcon('something-else')}</>);
    expect(container).toBeEmptyDOMElement();
  });
});

describe('getEventDeliveryIcon', () => {
  it('renders a delivered icon when delivered is true', () => {
    render(<>{getEventDeliveryIcon(true)}</>);
    expect(
      screen.getByTitle('This event has been delivered'),
    ).toBeInTheDocument();
  });

  it('renders a not-delivered icon when delivered is false', () => {
    render(<>{getEventDeliveryIcon(false)}</>);
    expect(
      screen.getByTitle('This event has not been delivered'),
    ).toBeInTheDocument();
  });
});

describe('getInvocationLogStatus', () => {
  it('renders a success button for a 2xx status', () => {
    render(<>{getInvocationLogStatus(200)}</>);
    expect(screen.getByRole('button')).toBeInTheDocument();
  });

  it('renders a success button just below the 300 boundary', () => {
    render(<>{getInvocationLogStatus(299)}</>);
    expect(screen.getByRole('button')).toBeInTheDocument();
  });

  it('renders a destructive button for a status of 300 or above', () => {
    render(<>{getInvocationLogStatus(500)}</>);
    expect(screen.getByRole('button')).toBeInTheDocument();
  });
});
