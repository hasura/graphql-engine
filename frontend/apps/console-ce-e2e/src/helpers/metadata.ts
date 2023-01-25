export const replaceMetadata = (newMetadata: Record<string, any>) => {
  const postBody = { type: 'replace_metadata', args: newMetadata };
  cy.request('POST', 'http://localhost:8080/v1/metadata', postBody).then(
    response => {
      expect(response.body).to.have.property('message', 'success'); // true
    }
  );
};

export const resetMetadata = () => {
  const postBody = { type: 'clear_metadata', args: {} };
  cy.request('POST', 'http://localhost:8080/v1/metadata', postBody).then(
    response => {
      expect(response.body).to.have.property('message', 'success'); // true
    }
  );
};
