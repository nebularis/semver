%% -----------------------------------------------------------------------------
%% Copyright (c) 2002-2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(semver_tests).
-include("semver.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").

basic_parser_test() ->
    ?assertThat(semver:parse("1.0.0"), 
        is(equal_to(#semver{major=1, minor=0, build=0}))).

patch_test() ->
    ?assertThat(semver:parse("15.21.3-RC2"), 
        is(equal_to(#semver{major=15, minor=21,
                            build=3, patch="-RC2"}))).

invalid_parse_test_() ->
    [{"Non-numeric major version number",
     ?_assertThat(semver:parse("a.2.1"), 
        is(equal_to({error, invalid_version})))},
     {"Non-numeric minor version number",
     ?_assertThat(semver:parse("1.x.1"), 
        is(equal_to({error, invalid_version})))},
     {"Non-numeric build version number",
     ?_assertThat(semver:parse("1.2.r"), 
        is(equal_to({error, invalid_version})))},
     {"Missing build version number",
     ?_assertThat(semver:parse("1.2"), 
        is(equal_to({error, invalid_version})))},
     {"Missing minor version number",
     ?_assertThat(semver:parse("12"), 
        is(equal_to({error, invalid_version})))}].
