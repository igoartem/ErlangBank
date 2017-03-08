%%%-------------------------------------------------------------------
%%% @author nekrasov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(bank).
-author("nekrasov").

%% API
-export([main/0,
  getCurrency/0,
  getBalance/2,
  getMoney/3,
  putMoney/3,
  sendMoney/4,
  giveMoney/3,
  getLimit/0]).

getCurrency() ->
  ConverterPid = global:whereis_name(converter),
  ConverterPid ! {get_currency, self()},
  receive
    {currency, Euro, Dollar} -> {Euro, Dollar}
  end.

getBalance(Deposit, Pin) ->
  BankPid = global:whereis_name(bank),
  BankPid ! {get_clients, self()},
  receive
    {clients, Clients} ->
      getBalanceProcess(Clients, Deposit, Pin)
  end.

getMoney(Deposit, Pin, Sum) ->
  BankPid = global:whereis_name(bank),
  BankPid ! {get_clients, self()},
  receive
    {clients, Clients} -> getMoneyProcess(Clients, Deposit, Pin, Sum)
  end.

putMoney(Deposit, Pin, Sum) ->
  BankPid = global:whereis_name(bank),
  BankPid ! {get_clients, self()},
  receive
    {clients, Clients} -> putMoneyProcess(Clients, Deposit, Pin, Sum)
  end.

sendMoney(Deposit, Pin, DepositTo, Sum) ->
  BankPid = global:whereis_name(bank),
  BankPid ! {get_clients, self()},
  receive
    {clients, Clients} -> sendMoneyProcess(Clients, Deposit, Pin, DepositTo, Sum)
  end.

getBalanceProcess(Clients, Deposit, Pin) ->
  ClientPid = findClient(Clients, Deposit),
  ClientPid ! {get_card, Deposit, self()},
  receive
    {card, Card} ->
      GoodPin = checkPin(Pin, Card),
      if
        GoodPin -> {getBalance(Card), getCurrency(Card)};
        true -> false
      end
  end.

getMoneyProcess(Clients, Deposit, Pin, Sum) ->
  ClientPid = findClient(Clients, Deposit),
  ClientPid ! {get_card, Deposit, self()},
  receive
    {card, Card} ->
      GoodPin = checkPin(Pin, Card),
      GoodSum = checkSum(Sum, Card),
      if
        (GoodPin and GoodSum) ->
          ClientPid ! {get_money, Deposit, Sum},
          Card;
        true -> false
      end
  end.

putMoneyProcess(Clients, Deposit, Pin, Sum) ->
  ClientPid = findClient(Clients, Deposit),
  ClientPid ! {get_card, Deposit, self()},
  receive
    {card, Card} ->
      GoodPin = checkPin(Pin, Card),
      if
        GoodPin ->
          ClientPid ! {put_money, Deposit, Sum},
          true;
        true -> false
      end
  end.

sendMoneyProcess(Clients, Deposit, Pin, DepositTo, Sum) ->
  Card = getMoneyProcess(Clients, Deposit, Pin, Sum),
  if
    Card =/= false ->
      putMoneyWithoutPinProcess(Clients, DepositTo, Sum, getCurrency(Card)),
      true;
    true -> false
  end.

putMoneyWithoutPinProcess(Clients, Deposit, Sum, CurrencyFrom) ->
  ClientPid = findClient(Clients, Deposit),
  ClientPid ! {get_card, Deposit, self()},
  receive
    {card, Card} ->
      CurrencyTo = getCurrency(Card),
      ConverterPid = global:whereis_name(converter),
      ConverterPid ! {convert, Sum, CurrencyFrom, CurrencyTo, self()},
      receive
        {convert_result, NewValue} -> ClientPid ! {put_money, Deposit, NewValue}
      end
  end.

findClient([], _) -> false;
findClient([ClientPid | T], Deposit) ->
  ClientPid ! {has_card, Deposit, self()},
  receive
    {have, ClientPid, true} -> ClientPid;
    {have, _, false} -> findClient(T, Deposit)
  end.

getBalance({_, _, _, Sum, _}) -> Sum.

getCurrency({_, _, _, _, Currency}) -> Currency.

checkSum(Sum, {_, _, _, RealSum, _}) -> (RealSum >= Sum).

checkPin(Pin, {_, Pin, _, _, _}) -> true;
checkPin(_, {_, _, _, _, _}) -> false.

main() ->
  ClientsCount = 3,
  CreditPid = spawn(fun() -> credit(10000, 100) end),
  global:re_register_name(credit, CreditPid),
  spawn(fun() -> updateLimitCredit(CreditPid) end),
  BankPid = spawn(fun() -> bank(ClientsCount) end),
  global:register_name(bank, BankPid),
  ConverterPid = spawn(fun() -> converter(70, 60) end),
  global:register_name(converter, ConverterPid),
  spawn(fun() -> centralBank(ConverterPid) end).

centralBank(PidConverter) ->
  Time = rand:uniform(10000),
  timer:sleep(Time),
  NewEuro = 60 + rand:uniform(20),
  NewDollar = 50 + rand:uniform(20),
  PidConverter ! {update, NewEuro, NewDollar},
  centralBank(PidConverter).

converter(Euro, Dollar) ->
  receive
    {convert, Value, C1, C2, Pid} ->
      NewValue = convert(Value, Euro, Dollar, C1, C2),
      Pid ! {convert_result, NewValue},
      converter(Euro, Dollar);
    {update, NewEuro, NewDollar} ->
      converter(NewEuro, NewDollar);
    {get_currency, Pid} ->
      Pid ! {currency, Euro, Dollar},
      converter(Euro, Dollar)
  end.

credit(Limit, Summa) ->
  receive
    {credit_sum, Pid} ->
      Pid ! {credit_sum_otv, Limit, self()},
      credit(Limit, Summa);
    {credit_yes, Value, Pid} ->
      NewValue = Limit - Value,
      NewSumma = Summa + Value,
      credit(NewValue, NewSumma);
    {credit_update, New} ->
      credit(Limit + New, Summa - New)
  end.

giveMoney(Deposit, Pin, SumTo) ->
  CreditPid = global:whereis_name(credit),
  CreditPid ! {credit_sum, self()},
  receive
    {credit_sum_otv, Sum, CreditPid} ->
      BankCredit = Sum - SumTo,
      if
        BankCredit > 0 ->
          CreditPid ! {credit_yes, SumTo, self()},
          putMoney(Deposit, Pin, SumTo);
        true -> false
      end
  end.

updateLimitCredit(CreditPid) ->
  Time = rand:uniform(10000),
  timer:sleep(Time),
  NewLimit = rand:uniform(2000),
  CreditPid ! {credit_update, NewLimit },
  updateLimitCredit(CreditPid).


getLimit() ->
  CreditPid = global:whereis_name(credit),
  CreditPid ! {credit_sum, self()},
  receive
    {credit_sum_otv, Sum, CreditPid} -> Sum
  end.

convert(Value, Euro, _Dollar, rub, eur) -> Value / Euro;
convert(Value, Euro, _Dollar, eur, rub) -> Value * Euro;
convert(Value, _Euro, Dollar, rub, usd) -> Value / Dollar;
convert(Value, _Euro, Dollar, usd, rub) -> Value * Dollar;
convert(Value, Euro, Dollar, eur, usd) -> (Value * Euro) / Dollar;
convert(Value, Euro, Dollar, usd, eur) -> (Value * Dollar) / Euro;
convert(Value, _Euro, _Dollar, C, C) -> Value.

bank(Clients) when is_list(Clients) ->
  erlang:display(node()),
  receive
    {get_clients, Pid} ->
      Pid ! {clients, Clients},
      bank(Clients);
    {add_client, ClientId} ->
      NewClientPid = spawn(fun() -> bankClient(ClientId, 3) end),
      NewClientsList = [NewClientPid | Clients],
      bank(NewClientsList)
  end;
bank(ClientsCount) ->
  Clients = createClients(ClientsCount),
  bank(Clients).

createClients(0) -> [];
createClients(ClientsCount) ->
  ClientId = generateClientId(ClientsCount),
  CardsCount = generateCardsCount(),
  ClientPid = spawn(fun() -> bankClient(ClientId, CardsCount) end),
  [ClientPid | createClients(ClientsCount - 1)].

generateClientId(X) -> X * 3.

generateCardsCount() -> 5.

bankClient(ClientId, Cards) when is_list(Cards) ->
  receive
    {has_card, Deposit, Pid} ->
      HasCard = hasCard(Cards, Deposit),
      Pid ! {have, self(), HasCard},
      bankClient(ClientId, Cards);
    {get_card, Deposit, Pid} ->
      Card = getCard(Cards, Deposit),
      Pid ! {card, Card},
      bankClient(ClientId, Cards);
    {put_money, Deposit, Sum} ->
      UpdateCards = putMoneyToDeposit(Cards, Deposit, Sum),
      bankClient(ClientId, UpdateCards);
    {get_money, Deposit, Sum} ->
      UpdateCards = getMoneyFromDeposit(Cards, Deposit, Sum),
      bankClient(ClientId, UpdateCards);
    {has_credit, Deposit, Pid} ->
      HasCredit = hasCredit(Cards, Deposit),
      Pid ! {credit, self(), HasCredit},
      bankClient(ClientId, Cards)

  end;
bankClient(ClientId, CardsCount) ->
  Cards = createCards(ClientId, CardsCount),
  bankClient(ClientId, Cards).

hasCard([], _) -> false;
hasCard([{Deposit, _, _, _, _} | _], Deposit) -> true;
hasCard([_ | T], Deposit) -> hasCard(T, Deposit).

hasCredit([], _) -> 0;
hasCredit([{Deposit, Credit, _, _, _} | _], Deposit) -> Credit;
hasCredit([_ | T], Deposit) -> hasCredit(T, Deposit).

getCard([], _) -> {};
getCard([{Deposit, Pin, Date, Sum, Currency} | _], Deposit) -> {Deposit, Pin, Date, Sum, Currency};
getCard([_ | T], Deposit) -> getCard(T, Deposit).

putMoneyToDeposit([], _, _) -> [];
putMoneyToDeposit([{Deposit, Pin, Date, OldSum, Currency} | T], Deposit, Sum) ->
  [{Deposit, Pin, Date, OldSum + Sum, Currency} | T];
putMoneyToDeposit([H | T], Deposit, Sum) ->
  [H | putMoneyToDeposit(T, Deposit, Sum)].

getMoneyFromDeposit([], _, _) -> [];
getMoneyFromDeposit([{Deposit, Pin, Date, OldSum, Currency} | T], Deposit, Sum) ->
  [{Deposit, Pin, Date, OldSum - Sum, Currency} | T];
getMoneyFromDeposit([H | T], Deposit, Sum) ->
  [H | getMoneyFromDeposit(T, Deposit, Sum)].

createCards(_, 0) -> [];
createCards(ClientId, CardsCount) ->
  Deposit = generateDeposit(ClientId, CardsCount),
  Pin = generatePin(ClientId, CardsCount),
  Date = generateDate(ClientId, CardsCount),
  Sum = generateSum(),
  Currency = generateCurrency(),
  Card = {Deposit, Pin, Date, Sum, Currency},
  erlang:display(Card),
  timer:sleep(100),
  [Card | createCards(ClientId, CardsCount - 1)].

generateDeposit(X, Y) -> 10 * X + Y.

generatePin(X, Y) -> 2 * X + Y.

generateDate(X, Y) -> 5 * X + Y.

generateSum() -> rand:uniform(1000).

generateCurrency() ->
  Rand = rand:uniform(3),
  if
    Rand == 1 -> usd;
    Rand == 2 -> eur;
    true -> rub
  end.