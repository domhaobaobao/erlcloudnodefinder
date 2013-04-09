%% @doc ec2-describe-instances based nodefinder service.
%% @end

-module (erlcloudnodefinder).
-export ([ discover/0 ]).
-behaviour (application).
-export ([ start/0, start/2, stop/0, stop/1 ]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

%% @spec discover () -> { ok, [ { Node::node (), pong | pang | timeout } ] }
%% @doc Initiate a discovery request.  Discovery is synchronous; the
%% results are returned.
%% @end

discover () ->
  erlcloudnodefindersrv:discover ().

%-=====================================================================-
%-                        application callbacks                        -
%-=====================================================================-

%% @hidden

start () ->
  application:start (erlcloudnodefinder).

%% @hidden

start (_Type, _Args) ->
  Group = case application:get_env (erlcloudnodefinder, group) of
    { ok, G } -> G;
    _ -> first_security_group ()
  end,
  { ok, PingTimeout } = application:get_env (erlcloudnodefinder, ping_timeout_sec),
  { ok, PrivateKey } = application:get_env (erlcloudnodefinder, private_key),
  { ok, Cert } = application:get_env (erlcloudnodefinder, cert),
  { ok, Ec2Home } = application:get_env (erlcloudnodefinder, ec2_home),
  { ok, JavaHome } = application:get_env (erlcloudnodefinder, java_home),

  erlcloudnodefindersup:start_link (Group, 
                               1000 * PingTimeout,
                               PrivateKey,
                               Cert,
                               Ec2Home,
                               JavaHome).

%% @hidden

stop () -> 
  application:stop (erlcloudnodefinder).

%% @hidden

stop (_State) ->
  ok.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

%% @private

first_security_group () ->
  Url = "http://169.254.169.254/2007-08-29/meta-data/security-groups",
  case httpc:request (Url) of
    { ok, { { _HttpVersion, 200, _Reason }, _Headers, Body } } ->
      string:substr (Body, 1, string:cspan (Body, "\n"));
    BadResult ->
      erlang:error ({ http_request_failed, Url, BadResult })
  end.
