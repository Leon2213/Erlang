-module(bank).
-export([start/0, balance/2, deposit/3, withdraw/3, lend/4]).


start() -> 
    spawn(fun() -> loop(#{}) end).

balance(Server, Who) ->
    monitor(process, Server),
    Ref = make_ref(),
    Server ! {balance, Who, self(), Ref},
        receive
            {ok, Value} ->
                {ok, Value};
            {no_account, Who} ->
                no_account;
            {'DOWN', Reference, process, Pid, Reason} ->
                no_bank
        end.

deposit(Server, Who, Amount) -> 
    monitor(process, Server),
    Ref = make_ref(),
    Server ! {deposit, Who, Amount, self(), Ref},
    receive 
        {ok, Value, Ref} ->
            {ok, Value};
        {'DOWN', Reference, process, Pid, Reason} ->
                no_bank
    end. 

withdraw(Server, Who, Amount) -> 
    monitor(process, Server),
    Ref = make_ref(),
    Server ! {withdraw, Who, Amount, self(), Ref},
    receive 
        {ok, Value, Ref} ->
            {ok, Value};
        {insufficient_funds, Ref} ->
            insufficient_funds;
        {no_account, Who, Ref} ->
            {no_account, Who};
        {'DOWN', Reference, process, Pid, Reason} ->
                    no_bank
    end. 

lend(Server, From, To, Amount) ->
    monitor(process, Server),
    Ref = make_ref(), 
    Server ! {lend, From, To, Amount, self(), Ref},
    receive 
        {ok, Ref} ->
            ok;
        {no_account, Who, Ref} -> 
            {no_account, Who};
        {no_account, both, Ref} -> 
            {no_account, both};
        {'DOWN', Reference, process, Pid, Reason} ->
                    no_bank
    end.
        
            

loop(Bank_map) ->
    receive
        % kolla balansen på kontot %
        {balance, Who, Requester_pid, Ref} ->
            case is_map_key(Who,Bank_map) of 
                true -> 
                    Requester_pid ! {ok, maps:get(Who, Bank_map)};
                false -> 
                    Requester_pid ! {no_account, Who}
            end,
            loop(Bank_map);
    

    % sätta in pengar på kontot %
        {deposit, Who, Deposit_amount, Requester_pid, Ref} ->
            case is_map_key(Who,Bank_map) of 
                true -> 
                    Current_amount = maps:get(Who, Bank_map),
                    NewMap = maps:put(Who, Current_amount + Deposit_amount, Bank_map),
                    Requester_pid ! {ok, maps:get(Who, NewMap), Ref},  
                    loop(NewMap);
                false -> 
                    NewMap = maps:put(Who, Deposit_amount, Bank_map),
                    Requester_pid ! {ok, Deposit_amount, Ref},                
                    loop(NewMap)
            end;

         % ta ut pengar från kontot %
        {withdraw, Who, Withdraw_amount, Requester_pid, Ref} ->
            case is_map_key(Who,Bank_map) of 
                true -> 
                    Current_amount = maps:get(Who, Bank_map),
                    if 
                        Current_amount - Withdraw_amount > 0 -> 
                            NewMap = maps:put(Who, Current_amount - Withdraw_amount, Bank_map),
                            Requester_pid ! {ok, maps:get(Who, NewMap), Ref},
                            loop(NewMap);
                        Current_amount - Withdraw_amount < 0 -> 
                            Requester_pid ! {insufficient_funds, Ref},
                            loop(Bank_map)
                    end;
                        % true -> ? %
                false -> 
                    Requester_pid ! {no_account, Who, Ref}
            end;

        % låna pengar till någon annan % 
        {lend, From, To, Amount, Requester_pid, Ref} ->
            case not is_map_key(From,Bank_map) of
                true -> 
                    case not is_map_key(To,Bank_map)  of
                        true ->  
                            {no_account, both, Ref},
                            loop(Bank_map);
                        false -> 
                            {no_account, From, Ref},
                            loop(Bank_map)
                    end;
                false -> 
                    case not is_map_key(To, Bank_map) of
                        true ->
                            {no_account, To, Ref},
                            loop(Bank_map);
                        false ->
                            io:write(self()),
                           % Requester_pid ! {withdraw, From, Amount, Requester_pid, Ref},
                            NewMap = maps:put(From,maps:get(From, Bank_map) - Amount,Bank_map),
                            NewMap2 = maps:put(To,maps:get(To, NewMap) + Amount, NewMap),
                            Requester_pid ! {ok, Ref},
                            loop(NewMap2)
                    end
            end % avslutar true false - rad 96
    end,
    loop(Bank_map).


