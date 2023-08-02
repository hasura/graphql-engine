type LinkProps = {
  onClick: () => void;
};

export const Link: React.FC<LinkProps> = ({ onClick, children }) => (
  <button
    onClick={onClick}
    className={`text-secondary focus-visible:ring-secondary hover:bg-secondary-light active:bg-secondary active:bg-opacity-25 py-2 px-3 rounded transition-all duration-100`}
  >
    {children}
  </button>
);
