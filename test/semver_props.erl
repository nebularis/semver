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
-module(semver_props).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include("semver.hrl").

-compile(export_all).

%%
%% Properties
%%

prop_all_valid_parse_strings() ->
    ?FORALL({Maj, Min, Build}, {integer(), integer(), integer()},
        ?IMPLIES(Maj > 0 andalso Min > 0 andalso Build > 0,
        ?assertThat(
            semver:parse(semver:vsn_string(semver:version(Maj, Min, Build))),
                is(equal_to(semver:version(Maj, Min, Build)))))).

prop_all_parsed_version_parts() ->
    ?FORALL({Maj, Min, Build}, {integer(), integer(), integer()},
        ?IMPLIES(Maj > 0 andalso Min > 0 andalso Build > 0,
        begin
            Vsn = semver:version(Maj, Min, Build),
            ?assertThat(semver:major_version(Vsn), is(equal_to(Maj))),
            ?assertThat(semver:minor_version(Vsn), is(equal_to(Min))),
            ?assertThat(semver:build_version(Vsn), is(equal_to(Build)))
        end)).

prop_major_version_comparisons() ->
    ?FORALL({VsnA, VsnB}, {integer(), integer()},
        ?assertThat(semver:compare(semver:version(VsnA, 0, 0),
                                   semver:version(VsnB, 0, 0)),
                maintains_equivalence(VsnA, VsnB))).

prop_symetry_of_compare() ->
    ?FORALL({VsnA, VsnB}, {integer(), integer()},
        begin
            CmpA = semver:compare(semver:version(VsnA, 0, 0),
                                       semver:version(VsnB, 0, 0)),
            CmpB = semver:compare(semver:version(VsnB, 0, 0),
                                       semver:version(VsnA, 0, 0)),
            ?assertThat(CmpA, is_reflected_by(CmpB))
        end).

prop_symetry_of_equiv() ->
    ?FORALL({VsnA, VsnB}, {integer(), integer()},
        begin
            A = semver:version(VsnA, 0, 0),
            B = semver:version(VsnB, 0, 0),
            ?assertThat(semver:equivalent(A, B), 
                is(equal_to(semver:equivalent(B, A))))
        end).

prop_major_version_equivalence() ->
    ?FORALL({VsnA, VsnB}, {integer(), integer()},
        begin
            A = semver:version(VsnA, 0, 0),
            B = semver:version(VsnB, 0, 0),
            ?assertThat(semver:equivalent(A, B), 
                is(equal_to(VsnA == VsnB)))
        end).

prop_any_hyphenated_patch_is_allowed() ->
    ?FORALL(Patch, alphanum(), 
        ?IMPLIES(length(Patch) > 1,
        ?assertThat(semver:patch(semver:parse("0.0.0-" ++ Patch)),
            is(equal_to(Patch))))).

prop_any_a2z_patch_without_hypthen_is_allowed() ->
    ?FORALL(Patch, a_to_z(), 
        ?IMPLIES(length(Patch) > 1,
        ?assertThat(semver:parse("0.0.0" ++ Patch),
            is(equal_to(#semver{patch=Patch}))))).    

%%
%% Custom PropEr Type Defs and Hamcrest Matchers
%%

is_reflected_by(CmpB) ->
    fun(0) -> CmpB == 0;
       (1) -> CmpB == -1;
       (-1) -> CmpB == 1
    end.

maintains_equivalence(VsnA, VsnB) ->
    fun(1) ->
        VsnA > VsnB;
       (0) ->
        VsnA == VsnB;
       (-1) ->
        VsnA < VsnB
    end.

a_to_z() ->
    non_empty(list(integer(97, 122))).

alphanum() ->
    %% NOTE: we avoid any encoding length issues in the match specifications
    %% by limiting ourselves to alpha-numeric characters in the latin alphabet 
    union([non_empty(list(integer(97, 122))), 
           non_empty(list(integer(48, 57)))]).
