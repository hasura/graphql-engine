import {
  SelectDatabase,
  SelectDatabaseProps,
} from './components/SelectDatabase/SelectDatabase';

export const ConnectDatabase = (props: SelectDatabaseProps) => {
  return (
    <div className="flex flex-col items-center">
      <div className="py-lg border-b border-slate-300 w-full flex justify-center">
        <div className="max-w-3xl w-full">
          <div className="text-xl font-bold">Connect Your First Database</div>
          <div className="text-muted">
            Connect your first database to access your database objects in your
            GraphQL API.
          </div>
        </div>
      </div>
      <div className="max-w-3xl py-lg w-full">
        <SelectDatabase {...props} />
      </div>
    </div>
  );
};
