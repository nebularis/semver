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
-export([patch/1, major_version/1, minor_version/1, build_version/1]).
-export([compare/2, equivalent/2]).

-include("semver.hrl").

-type semver() :: #semver{}.
-export_type([semver/0]).

-spec equivalent(semver(), semver()) -> boolean().
equivalent(A, B) ->
    compare(A, B) =:= 0.

-spec compare(semver(), semver()) -> integer().
compare(#semver{patch=P1}=A, #semver{patch=P2}=B) ->
    case do_compare(unpack(A), unpack(B)) of
        0 -> do_compare(P1, P2);
        N -> N
    end.

-spec major_version(semver()) -> integer().
major_version(#semver{major=M}) -> M.

-spec minor_version(semver()) -> integer().
minor_version(#semver{minor=M}) -> M.

-spec build_version(semver()) -> integer().
build_version(#semver{build=B}) -> B.

-spec patch(semver()) -> string().
patch(#semver{patch=P}) -> P.

-spec vsn_string(semver()) -> string().
vsn_string(#semver{major=Major, minor=Minor, build=Build}) ->
    lists:flatten(io_lib:format("v~p.~p.~p", [Major, Minor, Build])).

-spec version(integer(), integer(), integer()) -> semver().
version(Major, Minor, Build) ->
    #semver{major=Major, minor=Minor, build=Build}.

-spec parse(string()) -> semver() | error.
parse(V) ->
    case re:run(V,
        "[v]{0,1}(?<major>[\\d]+)\\.(?<minor>[\\d]+)\\.(?<build>[\\d]+)(?<patch>.*)",
        [no_auto_capture, anchored, notempty,
            {capture, [major, minor, build, patch], list}]) of
        nomatch ->
            error;
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

unpack(#semver{major=Maj, minor=Min, build=Build}) ->
    list_to_float(lists:concat(
            io_lib:format("~p~s~p~p", [Maj, ".", Min, Build]))).

do_compare(undefined, undefined) -> 0;
do_compare(_A, undefined) -> 1;
do_compare(undefined, _B) -> -1;
do_compare(A, B) when A > B -> 1;
do_compare(A, B) when A < B -> -1;
do_compare(A, B) when A == B -> 0.
