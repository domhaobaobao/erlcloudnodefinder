%% @hidden
%% @doc ec2-describe-instances based node discovery service.
%% @end

-module (erlcloudnodefinder_ec2_srv).
-behaviour (gen_server).
-export ([ start_link/1, discover/0 ]).
-export ([ init/1,
		   handle_call/3,
		   handle_cast/2,
		   handle_info/2,
		   terminate/2,
		   code_change/3]).

-record (state, { group, aws_config, ping_timeout }).

-include("../deps/erlcloud/include/erlcloud_ec2.hrl").

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

start_link (Group)
  when is_list (Group) ->
	gen_server:start_link 
	  ({ local, ?MODULE }, 
	   ?MODULE, 
	   [ Group ], 
	   []).

discover () ->
	gen_server:call (?MODULE, discover, 120000).

%-=====================================================================-
%-                         gen_server callbacks                        -
%-=====================================================================-

init ([ Group ]) ->
	io:format("erlcloudnodefinder_ec2_srv init 1~n"),
	pong = net_adm:ping (node ()), % don't startup unless distributed
	{ ok, AccessKeyID } = application:get_env (erlcloudnodefinder, ec2_access_key_id), 
	{ ok, SecretAccessKey } = application:get_env (erlcloudnodefinder, ec2_secret_access_key), 
	{ ok, Host } = application:get_env (erlcloudnodefinder, ec2_host),
	Timeout = 3000,
	
	io:format("erlcloudnodefinder_ec2_srv init 2~n"),
	AWSConfig = erlcloud_ec2:new(AccessKeyID, SecretAccessKey, Host),
	
	process_flag (trap_exit, true),
	io:format("erlcloudnodefinder_ec2_srv init 3~n"),
	State = #state{ group = Group, aws_config = AWSConfig, ping_timeout = Timeout },
	io:format("erlcloudnodefinder_ec2_srv init 4~n"),
	discover (State),
	io:format("erlcloudnodefinder_ec2_srv init 5~n"),
	{ ok, State }.

handle_call (discover, _From, State) -> 
	{ reply, { ok, discover (State) }, State };
handle_call (_Request, _From, State) -> 
	{ noreply, State }.

handle_cast (_Request, State) -> { noreply, State }.

handle_info (_Msg, State) -> { noreply, State }.

terminate (_Reason, _State) -> ok.

code_change (_OldVsn, State, _Extra) -> { ok, State }.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

async (Fun, Timeout) ->
	Me = self (),
	Ref = make_ref (),
	spawn (fun () ->
					{ ok, _ } = timer:kill_after (Timeout),
					Me ! { Ref, Fun () }
		   end),
	
	Ref.

collect (Key, Timeout) ->
	receive
		{ Key, Status } -> Status
		after Timeout ->
			timeout
	end.

discover (State) ->
	
	Group = State#state.group,
	Timeout = State#state.ping_timeout,
	AWSConfig = State#state.aws_config,
	
	[ { Node, collect (Key2, Timeout) } ||
	  { Node, Key2 } <- 
		  [ { Node, connect (Node, Timeout) } ||
			{ Host, { ok, NamesAndPorts } } <- 
				[ { Host, collect (Key, Timeout) } ||
				  { Host, Key } <- [ { Host, start_names (Host, Timeout) } 
									 || Host <- get_erlcloud_list (AWSConfig, Group) ] ],
			{ Name, _ } <- NamesAndPorts,
			Node <- [ list_to_atom (Name ++ "@" ++ Host) ] ] ].


get_erlcloud_list (AWSConfig, Group) ->
% describe_instances response = 
% Reservation = {reservation_id, ReservationId},  {owner_id, OwnerId}, {group_set, GroupSet}, {instances_set, Instances}
%
	ReservationsList = erlcloud_ec2:describe_instances(AWSConfig),
	lists:foldl(fun(ReservationPropList, CurrentList) ->
					GroupList = proplists:get_value(group_set, ReservationPropList),
					InstanceList = proplists:get_value(instances_set, ReservationPropList),

					case lists:member(Group, GroupList) of 
						true ->
							lists:foldl(fun(InstancePropList, CurrentListInner) ->
								[proplists:get_value(private_ip_address, InstancePropList) | CurrentListInner]
							end, CurrentList, InstanceList);
						false ->
							CurrentList
					end
				end, [], ReservationsList).
	
start_names (Host, Timeout) ->
	async (fun () -> net_adm:names (Host) end, Timeout).

connect (Node, Timeout) ->
	async (fun () -> net_kernel:connect_node(Node) end, Timeout).
