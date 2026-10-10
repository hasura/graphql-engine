import { HasuraLogoFull } from '@hasura/shared/ui';

export function TopHeaderBar() {
  return (
    <header className="flex items-center bg-gray-700 p-2">
      <div className="mr-auto ml-auto">
        <HasuraLogoFull size="sm" mode="primary" />
      </div>
    </header>
  );
}
