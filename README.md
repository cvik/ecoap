## ecoap - simple coap encode/decode library

CoAP stands for Constrained Application Protocol, which is a Request/Response oriented
binary protocol meant to be used with embedded devices with limited memory (usually KiloBytes,
not Gigabytes).

See [RFC7252](https://datatracker.ietf.org/doc/html/rfc7252) for further details.

## example usage

```erlang
1> {ok, Packet} = ecoap:decode(<<68,2,62,111,119,104,82,128,177,49,1,50,1,51,255,100,97,116,97>>).
{ok,#{code => post,msg_id => 15983,
      options =>
          [{uri_path,<<"1">>},{uri_path,<<"2">>},{uri_path,<<"3">>}], % full path /1/2/3
      payload => <<"data">>,
      token => <<119,104,82,128>>,
      type => confirmable}}

2> ecoap:encode(Packet).
{ok,<<68,2,62,111,119,104,82,128,177,49,1,50,1,51,255,
      100,97,116,97>>}

```
