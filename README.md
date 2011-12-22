# Semantic Versioning Support Library [![travis](https://secure.travis-ci.org/nebularis/semver.png)](http://travis-ci.org/nebularis/semver)

This is a small (one module), well tested library for working with semantic
versions (see [http://semver.org] for more details).

## Building from source

Until this project has been bootstrapped to use
[libconf](https://github.com/hyperthunk/libconf), you will need to use either
the embedded version of rebar, or compile it from source using the fork/branch
[here](https://github.com/hyperthunk/rebar/tree/econf).

First you must bootstrap the build system using the `init.config` settings.

    % ./rebar -C init.config get-deps compile

Then you can fetch and build all the required dependencies:

    % ./rebar get-deps compile

If you want to run the test suites, you can do so any time after after you've
bootstrapped, like so:

    % ./rebar -C test.config test -v

## License

This library is distributed under a permissive, BSD-like license.
