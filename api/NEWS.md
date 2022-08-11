# diagramAPI 0.0.39 *2022-03-24*

- fix: remove double wrapping bug on validation endpoint

# diagramAPI 0.0.38 *2022-03-19*

- fix: remove double wrapping bug on validation endpoint

# diagramAPI 0.0.37 *2022-03-19*

- refac: extracted responses name -> model_name
    - to make consistent across front and back end, reuse more functionality across endpoints
- feat: integrate plots into pdf

# diagramAPI 0.0.36 *2022-03-11*

- refac: change serializer of score_model for unboxed json

# diagramAPI 0.0.35 *2022-03-08*

- feat: Add validation of json objects

# diagramAPI 0.0.34 *2022-03-04*

- test: Add test case to identify scenario diff bug
- fix: Scenario diff bug

# diagramAPI 0.0.33 *2022-03-04*

- test: Add test cases for requests that were generating errors

# diagramAPI 0.0.32 *2022-03-03*

- feat: Add node json to the response

# diagramAPI 0.0.31 *2022-02-28*

- refac: switch advanced model format to explicit flag in json

# diagramAPI 0.0.30 *2022-02-25*

- fix: Missing deps

# diagramAPI 0.0.29 *2022-02-25*

- better pdf testing and logging

# diagramAPI 0.0.28 *2022-02-21*

- integrate and test plot endpoint in running api


# diagramAPI 0.0.27 *2022-02-20*

- integrate and test csv endpoint in running api

# diagramAPI 0.0.26 *2022-02-17*

- full pre and post route auto logging

# diagramAPI 0.0.25 *2022-02-11*

- replace all occurences of policy with scenario to match front end

# diagramAPI 0.0.24 *2022-02-14*

- start implementing levelled and namespaced logging to package functions
  - implement generic log handler `with_log_handle` which can wrap any
  expression and capture logs for error, warning and success
- example pre-route hook for plumber API for logging incoming requests

# diagramAPI 0.0.23 *2022-02-11*

- fix auto deploy

# diagramAPI 0.0.22 *2022-02-08*

- update connect deployment CI docker image

# diagramAPI 0.0.21 *2022-01-31*

- implement scoring of advanced models
- bring implementation to endpoint
- test running api

# diagramAPI 0.0.20 *2022-01-25*

- plot functions to call from endpoint

# diagramAPI 0.0.19 *2022-01-24*

- Add test cases for csv formatting
- Add test util for generating fake requests for csv formatting
- Functions for building nice csv from request data
- Test that nice table can be built

# diagramAPI 0.0.18 *2022-01-24*

- Add example json files for testing plot endpoints
- Add associated test utility function `test_req_plot`


# diagramAPI 0.0.17 *2022-01-24*

- Refactor test_data set up
    - allows for testing different types of requests more simply

# diagramAPI 0.0.16 *2022-01-18*

- Add templates and assets for pdf report generation
- Add convenience functions for grabbing package resources for pdf templating
- Add functionality to render a report to a temporary location ready for download
- Add convenience wrappers for loading nicely formatted question data
- Add utilities for joining questions and responses into nice tibble structure
- A bunch of other utilities for extracting and formatting content in pdfs
- Updated templates and style for NATA
- Single function for writing pdfs to a temp file `write_temp_pdf(<req_object>)`
- Endpoint updated in API, including serializer for pdf content type

# diagramAPI 0.0.15 *2022-01-13*

- refactor code for parsing simple responses
- add constructuor function for class simple_responses
- addition for class simple_responses
    - bring along name, scenario and notes as part of the object
    - motivation: easier to pass data back to pdf render

# diagramAPI 0.0.14 *2022-01-13*

- Add default model to package and utility to load it
- Add `score_model_` generic for passing the parsed responses to
    - Implement `score_model_.simple_responses`
    - Associated test
- Add functionality for conversion to and checking of appropriate model type ("grain") and associated tests
- convert `numeric_to_probability` to S3 generic to allow single function call for both simple (implemented) and advanced (not yet)
    - update test to suit

# diagramAPI 0.0.13 *2022-01-12*
  
- Add functionality for parsing advanced models sent to endpoint
- `to_numeric` conversion case handled for advanced models

# diagramAPI 0.0.12 *2022-01-05*

- Switch functionality for to_numeric to S3 generic system
- update check to suit
- add get_option_val generic to match where appropriate
- refactor the general response utils and probability conversion utils
- update tests for new function structure

# diagramAPI 0.0.11 *2021-11-26*

- Functions for parsing the model response data for simple model case
- Test simple model data json added to package
- Testing utility for generating fake request object
- Whole set of functions for converting simple model responses to numeric then
probability table objects
    - Tests written and passing for these (97)
    - Test coverage 100%

# diagramAPI 0.0.10 *2021-11-18*

- skeleton route and mount for model scoring
- refactor to make route generation more testable and generalisable
- some basic testing

# diagramAPI 0.0.9 *2021-11-18*

- pipeline devtools

# diagramAPI 0.0.8 *2021-11-18*

- pipeline dependency install

# diagramAPI 0.0.7 *2021-11-18*

- pipeline rsconnect_records force added

# diagramAPI 0.0.6 *2021-11-18*

- pipeline rsconnect dependency install

# diagramAPI 0.0.5 *2021-11-18*

- Get through the CI templating stuff

# diagramAPI 0.0.4 *2021-11-18*

- Add dependency on R >= 3.5.0

# diagramAPI 0.0.3 *2021-11-18*

- start work on default_api to load all routes defined in inst
- tidy up line endings for checks

# diagramAPI 0.0.2 *2021-11-17*

- basic route mounting utilities added
- initial test that main function returns a plumber api object
- pipeline for normal stuff + deployment on main/staging

# diagramAPI 0.0.1 *2021-11-16*

-   Initial commit
