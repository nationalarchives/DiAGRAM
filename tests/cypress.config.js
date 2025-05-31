const { defineConfig } = require("cypress");

const envPrefix = process.env.environment || 'dev'
const executionEnvUrl = `https://${envPrefix}-diagram.nationalarchives.gov.uk`

module.exports = defineConfig({
  e2e: {
    baseUrl: executionEnvUrl,
    supportFile: false,
  },
});
