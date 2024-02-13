##Update Infra 13/02/2024

## R in AWS Lambda

AWS Lambda supports a number of different
[_runtimes_](https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html). These runtimes
represent the programming languages which can be used "out-of-the-box" in Lambda. Unfortunately the
R programming language, utilised by the DiAGRAM application's backend, is _not_ one of these.
However, Lambda's 
[custom runtimes](https://docs.aws.amazon.com/lambda/latest/dg/runtimes-custom.html)
may be used to run R with Lambda.


The R package [{lambdr}](https://github.com/mdneuzerling/lambdr) implements an R runtime for Lambda;
providing the necessary functionality for handling the various endpoints required for accepting new
input and sending responses.

The {lambdr} package, along with the backend application code (see [`src/`](src/)) and its
dependencies, are installed into a container image (see `Dockerfile`), which is then deployed as a
Lambda function.

### TinyTeX

The DiAGRAM application requires a backend which is capable of rendering and returning parameterised
PDF documents via [R Markdown](https://rmarkdown.rstudio.com/), with parameter values provided by
the input of a user of the DiAGRAM application.

To render the PDF document required by the DiAGRAM application, the backend compute will need an
installation of the [LaTeX](https://www.latex-project.org/about/) document preparation system.

Typically, a user might install LaTeX via the [TeXLive](https://www.tug.org/texlive/) distribution.
However, a standard TeXLive install takes upwards of 4.5Gb of storage, and although Lambda container
images can be up to 10Gb in size, in the interest of minimising image build and push time, as well
as minimising
[cold-start](https://aws.amazon.com/blogs/compute/operating-lambda-performance-optimization-part-1/)
times, we should be attempting to keep our container image as small as possible.

[TinyTeX](https://yihui.org/tinytex/) is used in the backend to provide a minimal LaTeX
installation. It is necessary to be aware of a 
[known issue](https://github.com/rstudio/rmarkdown/issues/1975) in which {tinytex} ignores the
`intermediate_dir` argument of `rmarkdown::render()`. This is relevant because `/tmp` is the _only
writeable directory in Lambda_, and so we must force {tinytex} to write all files there, including
intermediary files (`.aux`, `.log`, `.synctex.gz`, ...):

```R
# Ephemeral storage
tmp_dir = "/tmp"

# https://github.com/rstudio/rmarkdown/issues/1615
options(
  tinytex.output_dir = tmp_dir,
  tinytex.engine_args = glue::glue("'--output-directory={tmp_dir}'")
)
```

### Multiple endpoints

There are a few different approaches to emulating multiple endpoints with AWS Lambda and AWS API
Gateway. 
[This StackOverflow question](https://stackoverflow.com/questions/41425511/aws-api-gateway-lambda-multiple-endpoint-functions-vs-single-endpoint)
provides a good overview of some of the pros and cons of the different possible approaches.

Here, an 
[API Gateway Lambda proxy integration](https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html)
is used to route all requests to a single Lambda function. Routing is then performed _within R_ by
`diagramLambda::handler()`.

In utilising a single Lambda function for all requests, a request is more likely to hit a "warm"
Lambda, in turn reducing the frequency of
[cold-start](https://aws.amazon.com/blogs/compute/operating-lambda-performance-optimization-part-1/)
events.


## Application code

The application code (the actual R backend), lives in the `src/` directory, and is structured as an
R package "{diagramLambda}".

{digramLambda} contains a `bootstrap.sh` and `runtime.R` file in `inst/extdata/runtime/`. These
files are used to start {lambdr}'s infinite event listening loop. {lambdr} is configured by
`runtime.R` to pass the raw event content through to `diagramLambda::handler()`, and to not
serialise any of the responses:

```R
lambdr::start_lambda(
  config = lambdr::lambda_config(
    serialiser = identity,
    deserialiser = identity
  )
)
```

Configuring {lambdr} to use the `identity()` function for deserialisation allows use of the custom
deserialisation routine `diagramLambda::gateway_payload_to_rook()`. Additionally, using `identity()`
for {lambdr}'s serialiser allows multiple different object types to be returned from the same Lambda
function, and for custom serialisers to be created for returning PDF documents, PNG images, and CSV
data.

## Testing Locally

It is possible to test the DiAGRAM API locally. We can do so by building the Lambda container image.
From the directory holding the `Dockerfile`:

```sh
docker build -t diagram:latest .
```

This image can then be run as:

```sh
docker run -p 9000:8080 --read-only=true --mount type=bind,source="/tmp/",target=/tmp diagram:latest
```

Note that this runs the container in `read-only` mode, but then mounts your local `/tmp/` directory
into the container. This simulates the fact that in AWS Lambda _`/tmp/` is the only writable
directory_.

Now that the Lambda container is running locally, requests can be sent to it. When deployed, Lambda
sits behind API Gateway, and as such expects its requests to be in a particular format. You can see
an example of what this format looks like [here](./src/inst/extdata/test_data/api_gateway_format.json).

Requests can now be sent to your locally running container as, eg.:

```
curl -XPOST "http://localhost:9000/2015-03-31/functions/function/invocations" -d @src/inst/extdata/test_data/api_gateway_format.json
```

## Deployment

Updating of the Docker image used by AWS Lambda is performed _automatically_ by a GitHub Action when
required. As a developer, you should _not_ typically need to manually push any images to AWS ECR.

If you _really must_ push directly to AWS ECR (perhaps for a redeployment to an _entirely new_
environment, or if GitHub Actions is experiencing downtime and you need to apply a hotfix), you will
require write-access to the correct ECR repository, and an AWS Access Key and corresponding AWS
Secret Access Key (see 
[the documentation](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html#Using_CreateAccessKey)
on how to generate these). You should add these keys to a profile by running 
[`aws configure`](https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-quickstart.html#cli-configure-quickstart-config).

Once you have configured your AWS profile, you can authenticate against ECR with:

```sh
aws ecr get-login-password --region <AWS-REGION-HERE> | docker login --username AWS --password-stdin <AWS-ACCOUNT-ID-HERE>.dkr.ecr.<AWS-REGION-HERE>.amazonaws.com
```

Then, from the this directory you can build and push your image as:

```sh
docker build -t <AWS-REPO-NAME-HERE> .
docker tag <AWS-REPO-NAME-HERE>:latest <AWS-ACCOUNT-ID-HERE>.dkr.ecr.<AWS-REGION-HERE>.amazonaws.com/<AWS-REPO-NAME-HERE>:latest
docker push <AWS-ACCOUNT-ID-HERE>.dkr.ecr.<AWS-REGION-HERE>.amazonaws.com/<AWS-REPO-NAME-HERE>:latest
```
