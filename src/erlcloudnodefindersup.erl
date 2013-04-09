%% @hidden

-module (erlcloudnodefindersup).
-behaviour (supervisor).

-export ([ start_link/2, init/1 ]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

start_link (Group, Mode) ->
  supervisor:start_link 
    (?MODULE, [ Group, Mode ]).

init ([ Group, Mode ]) ->
	io:format("erlcloudnodefindersup init"),

	ChildSpecs = case Mode of
					 ec2 ->
						 { erlcloudnodefinder_ec2_srv,
					          { erlcloudnodefinder_ec2_srv, start_link, [ Group ] },
					          permanent,
					          10000,
					          worker,
					          [ erlcloudnodefinder_ec2_srv ]
					     }
				 end,
	
	
  { ok, { { one_for_one, 3, 10 }, [ ChildSpecs ] } }.
