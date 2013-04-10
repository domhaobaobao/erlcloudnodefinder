%% @doc ec2-describe-instances based nodefinder service.
%% @end

-module (erlcloudnodefinder).
-export ([ discover/0 ]).
-behaviour (application).
-export ([ start/0, start/2, stop/0, stop/1 ]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

%% @spec discover () -> { ok, [ { Node::node (), boolean() | ignored } ] }
%% @doc Initiate a discovery request.  Discovery is synchronous; the
%% results are returned.
%% @end

discover () ->
	{ ok, Mode } = application:get_env (erlcloudnodefinder, mode),
	case Mode of
		ec2 ->
			erlcloudnodefinder_ec2_srv:discover ()
	end.

%-=====================================================================-
%-                        application callbacks                        -
%-=====================================================================-

%% @hidden

start () ->
	application:start (erlcloud),
	application:start (erlcloudnodefinder).

%% @hidden

start (_Type, _Args) ->
	% mode = ec2 is currently the only supported option
	{ ok, Mode } = application:get_env (erlcloudnodefinder, mode),
	Group = case application:get_env (erlcloudnodefinder, group) of
				{ ok, G } -> G;
				_ -> get_group (Mode)
			end,
	
	erlcloudnodefindersup:start_link (Group, Mode).

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

get_group(ec2) ->
	ec2_first_security_group ().

%% @private

ec2_first_security_group () ->
	Url = "http://169.254.169.254/2007-08-29/meta-data/security-groups",
	case httpc:request (Url) of
		{ ok, { { _HttpVersion, 200, _Reason }, _Headers, Body } } ->
			string:substr (Body, 1, string:cspan (Body, "\n"));
		BadResult ->
			erlang:error ({ http_request_failed, Url, BadResult })
	end.
