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
-module(semver).
-export([parse/1, vsn_string/1, version/3]).

-include("semver.hrl").

vsn_string(#semver{major=Major, minor=Minor, build=Build}) ->
    io_lib:format("~p.~p.~p", [Major, Minor, Build]).

version(Major, Minor, Build) ->
    #semver{major=Major, minor=Minor, build=Build}.

parse(V) ->
    case re:run(V,
        "(?<major>[\\d]+)\\.(?<minor>[\\d]+)\\.(?<build>[\\d]+)(?<patch>.*)",
        [no_auto_capture, anchored, notempty,
            {capture, [major, minor, build, patch], list}]) of
        nomatch ->
            {error, invalid_version};
        {match, [_,_,_,[]]=Matches} ->
            list_to_tuple([semver|[list_to_integer(N) || 
                                                N <- Matches, N /= []]] ++
                                                [undefined]);
        {match, [Maj,Min,Build,Patch]} ->
            Result = #semver{major=list_to_integer(Maj),
                             minor=list_to_integer(Min),
                             build=list_to_integer(Build)},
            case Patch of
                [] ->
                    Result;
                ("-" ++ Path) ->
                    Result#semver{patch=Path};
                _ ->
                    Result#semver{patch=Patch}
            end
    end.
