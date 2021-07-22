%%% CoAP encode/decode library
%%%
%%%  TODO:
%%%  - [ ] Better error handling

-module(ecoap).

-export([encode/1, decode/1]).

%% Types ----------------------------------------------------------------------

%% -type packet()
-type type()     :: confirmable|non_confirmable|acknowledgement|reset.
-type request()  :: get|post|put|delete.
-type response() :: success()|client_error()|server_error().
-type packet()   :: #{type:=type(),
                      code:=request()|response(),
                      msg_id:=non_neg_integer(),
                      token:=binary(),
                      options:=[option()],
                      payload:=binary()}.
-type option()   :: {option_type(), iodata()}.

-type option_type()  :: if_match|uri_host|etag|if_none_match|uri_port
                       |location_path|uri_path|content_format|max_age|uri_query
                       |accept|location_query|proxy_uri|proxy_scheme|size1.
-type success()      :: created|deleted|valid|changed|content.
-type client_error() :: bad_request|unauthorized|bad_option|forbidden|not_found
                       |method_not_allowed|not_acceptable|precondition_failed
                       |request_entity_too_large|unsupported_content_format.
-type server_error() :: internal_server_error|not_implemented|bad_gateway
                       |service_unavailable|gateway_timeout
                       |proxying_not_supported.

-type error() :: encoding_error|decoding_error|not_implemented.

%% API ------------------------------------------------------------------------

%% Encode ----------

-spec encode(packet()) -> {ok, iodata()} | {error, error()}.
encode(#{type:=Type, code:=Code} = Packet) ->
    EncType = encode_type(Type),
    EncCode = encode_code(Code),
    {Tkl, Token} = encode_token(Packet),
    MsgId = encode_msg_id(Packet),
    Payload = encode_payload(Packet),
    Options = encode_options(Packet),
    {ok, <<1:2, EncType:2, Tkl:4, EncCode/binary,
           MsgId:16, Token/binary, Options/binary, Payload/binary>>};
encode(_) -> {error, badarg}.

encode_type(confirmable) -> 0;
encode_type(non_confirmable) -> 1;
encode_type(acknowledgement) -> 2;
encode_type(reset) -> 3.

encode_code(get) -> <<0:3, 1:5>>;
encode_code(post) -> <<0:3, 2:5>>;
encode_code(put) -> <<0:3, 3:5>>;
encode_code(delete) -> <<0:3, 4:5>>;
encode_code(created) -> <<2:3, 1:5>>;
encode_code(deleted) -> <<2:3, 2:5>>;
encode_code(valid) -> <<2:3, 3:5>>;
encode_code(changed) -> <<2:3, 4:5>>;
encode_code(content) -> <<2:3, 5:5>>;
encode_code(bad_request) -> <<4:3, 0:5>>;
encode_code(unauthorized) -> <<4:3, 1:5>>;
encode_code(bad_option) -> <<4:3, 2:5>>;
encode_code(forbidden) -> <<4:3, 3:5>>;
encode_code(not_found) -> <<4:3, 4:5>>;
encode_code(method_not_allowed) -> <<4:3, 5:5>>;
encode_code(not_acceptable) -> <<4:3, 6:5>>;
encode_code(precondition_failed) -> <<4:3, 12:5>>;
encode_code(request_entity_too_large) -> <<4:3, 13:5>>;
encode_code(unsupported_content_format) -> <<4:3, 15:5>>;
encode_code(internal_server_error) -> <<5:3, 0:5>>;
encode_code(not_implemented) -> <<5:3, 1:5>>;
encode_code(bad_gateway) -> <<5:3, 2:5>>;
encode_code(service_unavailable) -> <<5:3, 3:5>>;
encode_code(gateway_timeout) -> <<5:3, 4:5>>;
encode_code(proxying_not_supported) -> <<5:3, 5:5>>;
encode_code(_) -> {error, code_not_supported}.

encode_token(#{token:=Token}) -> {size(Token), Token};
encode_token(_) -> {0, <<>>}.

encode_msg_id(#{msg_id:=MsgId}) -> MsgId;
encode_msg_id(_) -> 0.

encode_payload(#{payload:=<<>>}) -> <<>>;
encode_payload(#{payload:=Payload}) -> <<255, Payload/binary>>;
encode_payload(_) -> <<>>.

encode_options(#{options:=[]}) -> <<>>;
encode_options(#{options:=Options}) ->
    NewOptions = lists:sort(fun({Opt1, _}, {Opt2, _}) ->
                                    encode_option_num(Opt1) =< encode_option_num(Opt2)
                            end, Options),
    F = fun(Opt, {Bins, PrevNum}) ->
                {Bin, Num} = encode_option(Opt, PrevNum),
                {[Bin|Bins], Num}
        end,
    {Bins, _} = lists:foldl(F, {[], 0}, NewOptions),
    iolist_to_binary(lists:reverse(Bins));
encode_options(_) -> <<>>.

encode_option({Option, Data}, PrevNum) ->
    Num = encode_option_num(Option),
    Sum = Num - PrevNum,
    {Delta, ExtDelta} = encode_pack_opt(Sum),
    {Size, ExtSize} = encode_pack_opt(size(Data)),
    {<<Delta:4, Size:4, ExtDelta/binary, ExtSize/binary, Data/binary>>, Num}.

encode_pack_opt(Size) when Size < 13 -> {Size, <<>>};
encode_pack_opt(Size) when Size < 269 -> {13, <<(Size-13):8>>};
encode_pack_opt(Size) when Size < 65535 -> {14, <<(Size-269):16>>}.

encode_option_num(if_match) -> 1;
encode_option_num(uri_host) -> 3;
encode_option_num(etag) -> 4;
encode_option_num(if_none_match) -> 5;
encode_option_num(observe) -> 6;
encode_option_num(uri_port) -> 7;
encode_option_num(location_path) -> 8;
encode_option_num(uri_path) -> 11;
encode_option_num(content_format) -> 12;
encode_option_num(max_age) -> 14; 
encode_option_num(uri_query) -> 15;
encode_option_num(accept) -> 17;
encode_option_num(location_query) -> 20;
encode_option_num(proxy_uri) -> 35;
encode_option_num(proxy_scheme) -> 39;
encode_option_num(size1) -> 60.

% Decode ----------

-spec decode(binary()) -> {ok, packet()} | {error, error()}.
decode(<<1:2, T:2, Tkl:4, Class:3, Detail:5, MsgID:16, Token:Tkl/binary, Rest/binary>>) ->
    {Options, Payload} = decode_options(Rest),
    {ok, #{type=>decode_type(T),
           token=>Token,
           code=>decode_code(Class, Detail),
           msg_id=>MsgID,
           options=>Options,
           payload=>Payload}};
decode(_) -> {error, badarg}.

decode_options(<<>>) -> {[], <<>>};
decode_options(Bin) -> decode_options(Bin, 0, []).

decode_options(<<>>, _, Opts) ->
    {lists:reverse(Opts), <<>>};
decode_options(<<255, Rest/binary>>, _, Opts) ->
    {lists:reverse(Opts), Rest};
decode_options(<<Delta:4, Len:4, Rest/binary>>, Sum, Opts) ->
    {ExtDelta, Rest2} = decode_ext_delta(Delta, Rest),
    {ExtLen, Rest3} = decode_ext_len(Len, Rest2),
    <<Val:ExtLen/binary, Rest4/binary>> = Rest3,
    decode_options(Rest4, Sum+ExtDelta, [{decode_option(Sum+ExtDelta), Val}|Opts]).

decode_ext_delta(13, <<Delta:8, Rest/binary>>) -> {Delta+13, Rest};
decode_ext_delta(14, <<Delta:16, Rest/binary>>) -> {Delta+269, Rest};
decode_ext_delta(15, Rest) -> {error, {truncated_option, Rest}};
decode_ext_delta(Delta, Rest) when Delta < 13 -> {Delta, Rest}.

decode_ext_len(13, <<Len:8, Rest/binary>>) -> {Len+13, Rest};
decode_ext_len(14, <<Len:16, Rest/binary>>) -> {Len+269, Rest};
decode_ext_len(15, Rest) -> {error, {truncated_option, Rest}};
decode_ext_len(Len, Rest) when Len < 13 -> {Len, Rest}.

decode_type(0) -> confirmable;
decode_type(1) -> non_confirmable;
decode_type(2) -> acknowledegment;
decode_type(3) -> reset.

decode_code(0, 1) -> get;
decode_code(0, 2) -> post;
decode_code(0, 3) -> put;
decode_code(0, 4) -> delete;
decode_code(2, 1) -> created;
decode_code(2, 2) -> deleted;
decode_code(2, 3) -> valid;
decode_code(2, 4) -> changed;
decode_code(2, 5) -> content;
decode_code(4, 0) -> bad_request;
decode_code(4, 1) -> unauthorized;
decode_code(4, 2) -> bad_option;
decode_code(4, 3) -> forbidden;
decode_code(4, 4) -> not_found;
decode_code(4, 5) -> method_not_allowed;
decode_code(4, 6) -> not_acceptable;
decode_code(4, 12) -> precondition_failed;
decode_code(4, 13) -> request_entity_too_large;
decode_code(4, 15) -> unsupported_content_format;
decode_code(5, 0) -> internal_server_ok;
decode_code(5, 1) -> not_implemented;
decode_code(5, 2) -> bad_gateway;
decode_code(5, 3) -> service_unavailable;
decode_code(5, 4) -> gateway_timeout;
decode_code(5, 5) -> proxying_not_supported;
decode_code(C, D) -> {unknown_code, C, D}.

decode_option(1) -> if_match;
decode_option(3) -> uri_host;
decode_option(4) -> etag;
decode_option(5) -> if_none_match;
decode_option(6) -> observe;
decode_option(7) -> uri_port;
decode_option(8) -> location_path;
decode_option(11) -> uri_path;
decode_option(12) -> content_format;
decode_option(14) -> max_age;
decode_option(15) -> uri_query;
decode_option(17) -> accept;
decode_option(20) -> location_query;
decode_option(35) -> proxy_uri;
decode_option(39) -> proxy_scheme;
decode_option(60) -> size1;
decode_option(N) -> {error, {unknown_option, N}}.
