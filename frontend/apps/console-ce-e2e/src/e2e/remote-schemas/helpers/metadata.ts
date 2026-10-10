import { hgeUrl } from '../../../support/endpoints';
export const replaceMetadata = (newMetadata: Record<string, any>) => {
  const postBody = { type: 'replace_metadata', args: newMetadata };
  cy.request('POST', hgeUrl('/v1/metadata'), postBody).then((response) => {
    expect(response.body).to.have.property('message', 'success'); // true
  });
};

export const resetMetadata = () => {
  const postBody = { type: 'clear_metadata', args: {} };
  cy.request('POST', hgeUrl('/v1/metadata'), postBody).then((response) => {
    expect(response.body).to.have.property('message', 'success'); // true
  });
};

// Remove remote schemas a spec added, so they don't leak into later specs
// (e.g. a second schema on the same endpoint fails with duplicate root
// fields). Their remote relationships are dropped first, since HGE refuses to
// remove a schema something still depends on. Missing ones are ignored.
export const removeRemoteSchemas = (names: string[]) => {
  cy.request('POST', hgeUrl('/v1/metadata'), {
    type: 'export_metadata',
    args: {},
  }).then(({ body }) => {
    (body.remote_schemas ?? [])
      .filter((rs: { name: string }) => names.includes(rs.name))
      .forEach(
        (rs: {
          name: string;
          remote_relationships?: {
            type_name: string;
            relationships: { name: string }[];
          }[];
        }) => {
          (rs.remote_relationships ?? []).forEach(
            ({ type_name, relationships }) => {
              relationships.forEach(({ name }) => {
                cy.request('POST', hgeUrl('/v1/metadata'), {
                  type: 'delete_remote_schema_remote_relationship',
                  args: { remote_schema: rs.name, type_name, name },
                });
              });
            },
          );
        },
      );
  });

  names.forEach((name) => {
    cy.request({
      method: 'POST',
      url: hgeUrl('/v1/metadata'),
      failOnStatusCode: false,
      body: { type: 'remove_remote_schema', args: { name } },
    });
  });
};
