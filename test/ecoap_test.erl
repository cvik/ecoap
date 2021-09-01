-module(ecoap_test).

-include_lib("eunit/include/eunit.hrl").

config_test_() ->
    {setup,
     fun setup_config/0,
     fun cleanup_config/1,
     {inorder, lists:flatten([run_tests(Data) || Data <- test_data()])}}.

run_tests({Name, Dec, Enc}) ->
    [{timeout, 2, {atom_to_list(Name)++"_encode", fun() -> enc(Dec, Enc) end}},
     {timeout, 2, {atom_to_list(Name)++"_decode", fun() -> dec(Dec, Enc) end}},
     {timeout, 2, {atom_to_list(Name)++"_encode_decode", fun() -> enc_dec(Enc) end}},
     {timeout, 2, {atom_to_list(Name)++"_decode_encode", fun() -> dec_enc(Dec) end}}].


setup_config() -> ok.
cleanup_config(_) -> ok.

test_data() ->
    [{post_packet,
      #{type => confirmable,
        code => post,
        msg_id => 15983,
        token => <<119,104,82,128>>,
        options =>
          [{uri_path,<<"1">>},
           {uri_path,<<"2">>},
           {uri_path,<<"3">>}],
        payload => <<"data">>},
     <<68,2,62,111,119,104,82,128,177,49,1,50,1,51,255,100,97,116,97>>},
     {get_packet,
      #{type=>confirmable,
        code => get,
        msg_id => 47650,
        token => <<12,83,95,185>>,
        options => [{uri_path,<<"check_in">>}],
        payload => <<>>},
      <<68,1,186,34,12,83,95,185,184,99,104,101,99,107,95,105,110>>}
    ].

enc(Dec, Enc) ->
    {ok, Enc} = ecoap:encode(Dec).

dec(Dec, Enc) ->
    {ok, Dec} = ecoap:decode(Enc).

dec_enc(Dec) ->
    {ok, Enc} = ecoap:encode(Dec),
    {ok, Dec} = ecoap:decode(Enc).

enc_dec(Enc) ->
    {ok, Dec} = ecoap:decode(Enc),
    {ok, Enc} = ecoap:encode(Dec).
