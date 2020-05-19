const getTemplatePath = (framework: string) => `https://raw.githubusercontent.com/hasura/codegen-assets/master/${framework}/actions-codegen.js`;

module.exports.getTemplatePath = getTemplatePath;
