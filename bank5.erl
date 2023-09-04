-module(bank5).
-export([start/0, init/1, balance/2, deposit/3, handle_call/3, withdraw/3, lend/4]).

start() ->
    gen_server:start(?MODULE, [], []).

init(_Args) ->
    {ok, #{}}.

balance(Server, Who) when is_pid (Server) ->
    gen_server:call(Server, {balance, Who}).

deposit(Server, Who, Amount) when is_pid (Server) -> 
    gen_server:call(Server, {deposit, Who, Amount}).

withdraw(Server, Who, Amount) when is_pid (Server) ->
    gen_server:call(Server, {withdraw, Who, Amount}).

lend(Server, From, To, Amount) when is_pid (Server) ->
    gen_server:call(Server, {lend, From, To, Amount}).


handle_call({balance, Name}, _From, Bank_map) ->
    case is_map_key(Name, Bank_map) of
        true ->
            {reply, {ok, maps:get(Name, Bank_map)}, Bank_map};
        false ->
            {reply, {no_account, Name}, Bank_map}
    end; 

handle_call({deposit, Who, Deposit_amount}, _From, Bank_map) ->
    case is_map_key(Who,Bank_map) of 
                true -> 
                    Current_amount = maps:get(Who, Bank_map),
                    NewMap = maps:put(Who, Current_amount + Deposit_amount, Bank_map),
                    {reply, {ok, maps:get(Who, NewMap)}, NewMap};
                false -> 
                    NewMap = maps:put(Who, Deposit_amount, Bank_map),
                    {reply, {ok, Deposit_amount}, NewMap}
            end;


handle_call({withdraw, Who, Withdraw_amount}, _From, Bank_map) ->
            case is_map_key(Who,Bank_map) of 
                true -> 
                    Current_amount = maps:get(Who, Bank_map),
                    if 
                        Current_amount - Withdraw_amount > 0 -> 
                            NewMap = maps:put(Who, Current_amount - Withdraw_amount, Bank_map),
                            {reply, {ok, maps:get(Who, NewMap)}, NewMap};
                        Current_amount - Withdraw_amount < 0 -> 
                            {reply, {insufficient_funds}, Bank_map}
                    end;
                        % true -> ? %
                false -> 
                    {reply, {no_account}, Bank_map}
            end;


handle_call({lend, From, To, Amount}, _From, Bank_map) ->
            case not is_map_key(From,Bank_map) of
                true -> 
                    case not is_map_key(To,Bank_map)  of
                        true ->  
                            {reply,{no_account, both}, Bank_map};
                        false -> 
                            {reply,{no_account, From}, Bank_map}
                    end;
                false -> 
                    case not is_map_key(To, Bank_map) of
                        true ->
                            {reply, {no_account, To}, Bank_map};
                        false ->
                            NewMap = maps:put(From,maps:get(From, Bank_map) - Amount,Bank_map),
                            NewMap2 = maps:put(To,maps:get(To, NewMap) + Amount, NewMap),
                            {reply, {ok}, NewMap2}
                    end
            end. 
