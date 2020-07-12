-module(ecoap).

-export([encode/1, decode/1]).

%% Types ----------------------------------------------------------------------

%% -type packet()
-type request()  :: {get|post|put|delete, confirmable|non_confirmable}.
-type response() :: {ok, success()} | {error, client_error()|server_error()}.
-type packet()   :: #{type:=request()|response()|reset,
                      id:=non_neg_integer(),
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

-spec encode(packet()) -> {ok, iodata()} | {error, error()}.
encode(_) -> {error, not_implemented}.

decode(<<1:2, T:2, Tkl:4, Class:3, Detail:5, MsgID:16, Token:Tkl/binary, Rest/binary>>) ->
    {Options, Payload} = decode_options(Rest),
    #{type=>type(T),
      token=>Token,
      code=>decode_code(Class, Detail),
      msg_id=>MsgID,
      options=>Options,
      payload=>Payload}.

decode_options(<<>>) -> {[], <<>>};
decode_options(Bin) -> decode_options(Bin, 0, []).

decode_options(<<>>, _, Opts) ->
    {lists:reverse(Opts), <<>>};
decode_options(<<255, Rest/binary>>, _, Opts) ->
    {lists:reverse(Opts), Rest};
decode_options(<<Delta:4, Len:4, Rest/binary>>, Sum, Opts) ->
    {ExtDelta, Rest2} = ext_delta(Delta, Rest),
    {ExtLen, Rest3} = ext_len(Len, Rest2),
    <<Val:ExtLen/binary, Rest4/binary>> = Rest3,
    decode_options(Rest4, Sum+ExtDelta, [{decode_option(Sum+ExtDelta), Val}|Opts]).

ext_delta(13, <<Delta:8, Rest/binary>>) -> {Delta+13, Rest};
ext_delta(14, <<Delta:16, Rest/binary>>) -> {Delta+269, Rest};
ext_delta(15, Rest) -> {error, {truncated_option, Rest}};
ext_delta(Delta, Rest) when Delta < 13 -> {Delta, Rest}.

ext_len(13, <<Len:8, Rest/binary>>) -> {Len+13, Rest};
ext_len(14, <<Len:16, Rest/binary>>) -> {Len+269, Rest};
ext_len(15, Rest) -> {error, {truncated_option, Rest}};
ext_len(Len, Rest) when Len < 13 -> {Len, Rest}.

type(0) -> confirmable;
type(1) -> non_confirmable;
type(2) -> acknowledegment;
type(3) -> reset.

decode_code(0, 1) -> get;
decode_code(0, 2) -> post;
decode_code(0, 3) -> put;
decode_code(0, 4) -> delete;
decode_code(2, 1) -> {ok, created};
decode_code(2, 2) -> {ok, deleted};
decode_code(2, 3) -> {ok, valid};
decode_code(2, 4) -> {ok, changed};
decode_code(2, 5) -> {ok, content};
decode_code(4, 0) -> {error, bad_request};
decode_code(4, 1) -> {error, unauthorized};
decode_code(4, 2) -> {error, bad_option};
decode_code(4, 3) -> {error, forbidden};
decode_code(4, 4) -> {error, not_found};
decode_code(4, 5) -> {error, method_not_allowed};
decode_code(4, 6) -> {error, not_acceptable};
decode_code(4, 12) -> {error, precondition_failed};
decode_code(4, 13) -> {error, request_entity_too_large};
decode_code(4, 15) -> {error, unsupported_content_format};
decode_code(5, 0) -> {error, internal_server_error};
decode_code(5, 1) -> {error, not_implemented};
decode_code(5, 2) -> {error, bad_gateway};
decode_code(5, 3) -> {error, service_unavailable};
decode_code(5, 4) -> {error, gateway_timeout};
decode_code(5, 5) -> {error, proxying_not_supported};
decode_code(C, D) -> {error, {unknown_code, C, D}}.

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
