const { defineConfig } = require("cypress");

const defaultEnvUrl = `https://dev-diagram.nationalarchives.gov.uk`
const envPrefix = process.env.environmentPrefix || ''
const executionEnvUrl = envPrefix ? `https://${envPrefix}-diagram.nationalarchives.gov.uk` : defaultEnvUrl

module.exports = defineConfig({
  e2e: {
    baseUrl: executionEnvUrl,
    supportFile: false,
  },
});
