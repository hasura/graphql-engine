export const buildDockerCommand = (containerName: string, port: number) =>
  `docker run -d --name ${containerName} -p 127.0.0.1:${port.toString()}:8081 hasura/graphql-data-connector`;
export const buildAgentPath = (
  path: string,
  port: number,
  protocol: 'http' | 'https'
) => `${protocol}://${path}:${port}`;
