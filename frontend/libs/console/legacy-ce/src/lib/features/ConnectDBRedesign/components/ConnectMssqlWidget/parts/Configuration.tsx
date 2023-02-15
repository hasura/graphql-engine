import { ConnectionInfo } from './ConnectionInfo';

export const Configuration = ({ name }: { name: string }) => {
  return (
    <div className="my-2">
      <ConnectionInfo name={`${name}.connectionInfo`} />
    </div>
  );
};
