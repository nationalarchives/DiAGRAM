## Frontend

To run frontend code you must have node and npm installed. Then run

``` shell
npm ci && npm run prepare
```

followed by

``` shell
npm run start
```

That should start server running at localhost:1234.

To build for production run

``` shell
npm run build
```

This will place the built files inside a `dist/` directory.
