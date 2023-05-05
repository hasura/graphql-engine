export const PageWrapper: React.FC = ({ children }) => {
  return (
    <div className="flex flex-col">
      <div className="py-lg px-md border-b border-slate-300 w-full flex">
        <div className="max-w-3xl w-full">
          <div className="text-xl font-bold">Create Native Query</div>
          <div className="text-muted">
            Access more queries and operators through SQL on your database.
          </div>
        </div>
      </div>
      <div className="py-lg px-md w-full flex flex-col">{children}</div>
    </div>
  );
};
