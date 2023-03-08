import { Datasets } from './Datasets';
import { ProjectId } from './ProjectId';
import { ServiceAccount } from './ServiceAccount';

export const Configuration = ({ name }: { name: string }) => {
  return (
    <div className="my-2">
      <div className="my-2">
        <ServiceAccount name={`${name}.serviceAccount`} />
      </div>
      <div className="my-2">
        <ProjectId name={`${name}.projectId`} />
      </div>
      <div className="my-2">
        <Datasets name={`${name}.datasets`} />
      </div>
    </div>
  );
};
