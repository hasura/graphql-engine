import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { TypeSearchForm } from './SearchTypes';

const renderSearchForm = () => {
  const setSearch = vi.fn();
  render(
    <Theme>
      <TypeSearchForm setSearch={setSearch} />
    </Theme>,
  );
  return { setSearch };
};

describe('TypeSearchForm', () => {
  it('renders the search input', () => {
    renderSearchForm();
    expect(screen.getByPlaceholderText('Search Types...')).toBeInTheDocument();
  });

  it('calls setSearch with the initial empty value on mount', () => {
    const { setSearch } = renderSearchForm();
    expect(setSearch).toHaveBeenCalledWith('');
  });

  it('calls setSearch with the current input value as the user types', async () => {
    const { setSearch } = renderSearchForm();

    fireEvent.change(screen.getByPlaceholderText('Search Types...'), {
      target: { value: 'User' },
    });

    await waitFor(() => {
      expect(setSearch).toHaveBeenLastCalledWith('User');
    });
  });
});
