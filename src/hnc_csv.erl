%% Copyright (c) 2024, Maria Scott <maria-12648430@hnc-agency.org>
%% Copyright (c) 2024, Jan Uhlig <juhlig@hnc-agency.org>
%% 
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(hnc_csv).

-export([parse/1, parse/2]).
-export([parse_init/0, parse_init/1, parse_init/2]).
-export([parse_add_data/2]).
-export([parse_next_line/1]).
-export([parse_flush/1]).
-export([default_parse_options/0]).

-export([write/1, write/2]).
-export([write_line/1, write_line/2]).
-export([default_write_options/0]).

-type data() :: binary().

-type csv_field() :: binary().

-type csv_line() :: [csv_field()].

-type parse_options() :: #{'separator' := $, | $; | $\t,
			   'enclosure' := 'undefined' | $" | $',
			   'quote' := 'undefined' | $\\ | $" | $'}.

-type write_options() :: #{'separator' := $, | $; | $\t,
			   'enclosure' := 'undefined' | $" | $',
			   'quote' := 'undefined' | $\\ | $" | $',
			   'enclose' := 'never' | 'always' | 'optional',
			   'end_of_line' := binary()}.

-opaque state() :: fun(('flush' | data()) -> {'end_of_data' | csv_line(), state()}).

-export_type([state/0]).

-spec parse(RawData :: data()) -> {Lines :: [csv_line()], Rest :: data()}.
parse(Data) ->
	parse(Data, default_parse_options()).

-spec parse(RawData :: data(), Options :: parse_options()) -> Lines :: [csv_line()].
parse(Data, Opts) when is_binary(Data), is_map(Opts) ->
	parse1(parse_init(Data, Opts), []).

parse1(Cont, Acc) ->
	parse2(parse_next_line(Cont), Acc).

parse2({end_of_data, Cont}, Acc) ->
	parse3(parse_flush(Cont), Acc);
parse2({Line, Cont}, Acc) ->
	parse1(Cont, [Line|Acc]).

parse3({undefined, _}, Acc) ->
	lists:reverse(Acc);
parse3({LastLine, _}, Acc) ->
	lists:reverse([LastLine|Acc]).

-spec parse_init() -> State :: state().
parse_init() ->
	parse_init(<<>>, default_parse_options()).

-spec parse_init(Data :: data()) -> State :: state();
                (Options :: parse_options()) -> State :: state().
parse_init(Data) when is_binary(Data) ->
	parse_init(Data, default_parse_options());
parse_init(Opts) when is_map(Opts) ->
	parse_init(<<>>, Opts).

-spec parse_init(RawData :: data(), Options :: parse_options()) -> State :: state().
parse_init(Data, Opts) when is_binary(Data), is_map(Opts) ->
	ok = validate_parse_opts(Opts),
	fun
		(flush) ->
			{undefined, Data};
		(MoreData) ->
			do_parse(undefined, <<Data/binary, MoreData/binary>>, Opts, <<>>, [])
	end.

-spec parse_next_line(State0 :: state()) -> {Result :: ('end_of_data' | csv_line()), State1 :: state()}.
parse_next_line(Cont) when is_function(Cont, 1) ->
	Cont(<<>>).

-spec parse_add_data(State0 :: state(), RawData :: data()) -> State1 :: state().
parse_add_data(Cont, Data) ->
	fun(MoreData) -> Cont(<<Data/binary, MoreData/binary>>) end.

-spec parse_flush(State :: state()) -> {Line :: ('undefined' | csv_line()), Rest :: data()}.
parse_flush(Cont) when is_function(Cont, 1) ->
	Cont(flush).

do_parse(Mode, More, #{quote:=Quot}=Opts, FieldAcc, LineAcc) when More =:= <<>>;
								  Mode =:= enclosed_field, More =:= <<Quot>> ->
	{end_of_data,
	 fun
		(flush) when Mode=:=undefined ->
			{undefined, More};
		(flush) ->
			{lists:reverse([<<FieldAcc/binary, More/binary>>|LineAcc]), <<>>};
		(Data) ->
			do_parse(Mode, <<More/binary, Data/binary>>, Opts, FieldAcc, LineAcc)
	 end};
do_parse(undefined, <<$\r, More/binary>>, Opts, FieldAcc, LineAcc) ->
	do_parse(undefined, More, Opts, FieldAcc, LineAcc);
do_parse(undefined, <<$\n, More/binary>>, Opts, FieldAcc, LineAcc) ->
	do_parse(undefined, More, Opts, FieldAcc, LineAcc);
do_parse(line, <<$\r, More/binary>>, Opts, FieldAcc, LineAcc) ->
	do_parse_eol(More, Opts, FieldAcc, LineAcc);
do_parse(line, <<$\n, More/binary>>, Opts, FieldAcc, LineAcc) ->
	do_parse_eol(More, Opts, FieldAcc, LineAcc);
do_parse(field, <<$\r, More/binary>>, Opts, FieldAcc, LineAcc) ->
	do_parse_eol(More, Opts, FieldAcc, LineAcc);
do_parse(field, <<$\n, More/binary>>, Opts, FieldAcc, LineAcc) ->
	do_parse_eol(More, Opts, FieldAcc, LineAcc);
do_parse(undefined, More, Opts, FieldAcc, LineAcc) ->
	do_parse(line, More, Opts, FieldAcc, LineAcc);
do_parse(line, <<Sep, More/binary>>, #{separator:=Sep}=Opts, _FieldAcc, LineAcc) ->
	do_parse(line, More, Opts, <<>>, [<<>>|LineAcc]);
do_parse(field, <<Sep, More/binary>>, #{separator:=Sep}=Opts, FieldAcc, LineAcc) ->
	do_parse(line, More, Opts, <<>>, [FieldAcc|LineAcc]);
do_parse(line, More, Opts, FieldAcc, LineAcc) ->
	do_parse(field, More, Opts, FieldAcc, LineAcc);
do_parse(field, <<Enc, More/binary>>, #{enclosure:=Enc}=Opts, <<>>, LineAcc) ->
	do_parse(enclosed_field, More, Opts, <<>>, LineAcc);
do_parse(field, <<C, More/binary>>, #{enclosure:=Enc}=Opts, FieldAcc, LineAcc) when C=/=Enc ->
	do_parse(field, More, Opts, <<FieldAcc/binary, C>>, LineAcc);
do_parse(enclosed_field, <<Quot, Quot, More/binary>>, #{enclosure:=Enc, quote:=Quot}=Opts, FieldAcc, LineAcc) when Quot=/=Enc ->
	do_parse(enclosed_field, More, Opts, <<FieldAcc/binary, Quot>>, LineAcc);
do_parse(enclosed_field, <<Quot, Enc, More/binary>>, #{enclosure:=Enc, quote:=Quot}=Opts, FieldAcc, LineAcc) ->
	do_parse(enclosed_field, More, Opts, <<FieldAcc/binary, Enc>>, LineAcc);
do_parse(enclosed_field, <<Enc, More/binary>>, #{enclosure:=Enc}=Opts, FieldAcc, LineAcc) ->
	do_parse(field, More, Opts, FieldAcc, LineAcc);
do_parse(enclosed_field, <<C, More/binary>>, Opts, FieldAcc, LineAcc) ->
	do_parse(enclosed_field, More, Opts, <<FieldAcc/binary, C>>, LineAcc).

do_parse_eol(More, Opts, FieldAcc, LineAcc) ->
	{lists:reverse([FieldAcc|LineAcc]),
	 fun
		(flush) ->
			{undefined, More};
		(Data) ->
			do_parse(undefined, <<More/binary, Data/binary>>, Opts, <<>>, [])
	 end}.

-spec write(Lines :: [csv_line()]) -> RawData :: data().
write(Lines) ->
	write(Lines, default_write_options()).

-spec write(Lines :: [csv_line()], Options :: write_options()) -> RawData :: data().
write(Lines, Opts) ->
	ok = validate_write_opts(Opts),
	write1(Lines, Opts, undefined).

write1([], _Opts, undefined) ->
	<<>>;
write1([], _Opts, Acc) ->
	Acc;
write1([Line|More], Opts, undefined) ->
	write1(More, Opts, write_line1(Line, Opts));
write1([Line|More], Opts, Acc) ->
	write1(More, Opts, <<Acc/binary, (write_line1(Line, Opts))/binary>>).

-spec write_line(Line :: csv_line()) -> RawData :: data().
write_line(Fields) ->
	write_line(Fields, default_write_options()).

-spec write_line(Line :: csv_line(), Options :: write_options()) -> RawData :: data().
write_line(Fields, Opts) ->
	ok = validate_write_opts(Opts),
	write_line1(Fields, Opts).

write_line1(Fields, Opts) ->
	write_line1(Fields, Opts, undefined).

write_line1([], _Opts, undefined) ->
	<<>>;
write_line1([], #{end_of_line:=EOL}, Acc) ->
	<<Acc/binary, EOL/binary>>;
write_line1([Field|More], Opts, undefined) ->
	write_line1(More, Opts, write_field1(Field, Opts));
write_line1([Field|More], #{separator:=Sep}=Opts, Acc) ->
	write_line1(More, Opts, <<Acc/binary, Sep, (write_field1(Field, Opts))/binary>>).

write_field1(<<>>, #{enclosure:=Enc, enclose:=always}) ->
	<<Enc, Enc>>;
write_field1(<<_/binary>>=Field, #{enclosure:=Enc, quote:=Quot, enclose:=always}) ->
	<<Enc, (binary:replace(Field, [<<Enc>>, <<Quot>>], <<Quot>>, [global, {insert_replaced, 1}]))/binary, Enc>>;
write_field1(<<>>, #{enclose:=never}) ->
	<<>>;
write_field1(<<_/binary>>=Field, #{separator:=Sep, enclose:=never}) ->
	case nomatch =:= binary:match(Field, [<<Sep>>, <<$\r>>, <<$\n>>]) of
		true -> Field;
		false -> error(special_char_in_field)
	end;
write_field1(<<>>, #{enclose:=optional}) ->
	<<>>;
write_field1(<<_/binary>>=Field, #{separator:=Sep, enclosure:=Enc, quote:=Quot, enclose:=optional}) ->
	case nomatch =:= binary:match(Field, [<<Sep>>, <<Enc>>, <<$\r>>, <<$\n>>]) of
		true -> Field;
		false -> <<Enc, (binary:replace(Field, [<<Enc>>, <<Quot>>], <<Quot>>, [global, {insert_replaced, 1}]))/binary, Enc>>
	end.

-spec default_parse_options() -> parse_options().
default_parse_options() ->
	#{separator => $,,
	  enclosure => $",
	  quote => $"}.

-spec default_write_options() -> write_options().
default_write_options() ->
	#{separator => $,,
	  enclosure => $",
	  quote => $",
	  enclose => optional,
	  end_of_line => <<$\r, $\n>>}.

validate_parse_opts(#{separator:=Sep, enclosure:=Enc, quote:=Quot}=Opts) ->
	case
       			validate_separator(Sep)
		andalso validate_enclosure(Enc, Quot)
	of
		true ->
			ok;
		false ->
			error({badopts, Opts})
	end;
validate_parse_opts(Opts) ->
       	error({badopts, Opts}).

validate_write_opts(#{separator:=Sep, enclosure:=Enc, quote:=Quot, enclose:=EncPolicy, end_of_line:=EOL}=Opts) ->
	case
			validate_separator(Sep)
		andalso validate_enclosure(EncPolicy, Enc, Quot)
		andalso validate_end_of_line(EOL)
	of
		true ->
			ok;
		false ->
			error({badopts, Opts})
	end;
validate_write_opts(Opts) ->
	error({badopts, Opts}).

validate_separator($,) -> true;
validate_separator($;) -> true;
validate_separator($\t) -> true;
validate_separator(_) -> false.

validate_enclosure(undefined, undefined) -> true;
validate_enclosure($", $") -> true;
validate_enclosure($', $') -> true;
validate_enclosure($", $\\) -> true;
validate_enclosure($', $\\) -> true;
validate_enclosure(_, _) -> false.

validate_enclosure(never, undefined, undefined) -> true;
validate_enclosure(optional, $", $") -> true;
validate_enclosure(optional, $', $') -> true;
validate_enclosure(optional, $", $\\) -> true;
validate_enclosure(optional, $', $\\) -> true;
validate_enclosure(always, $", $") -> true;
validate_enclosure(always, $', $') -> true;
validate_enclosure(always, $", $\\) -> true;
validate_enclosure(always, $', $\\) -> true;
validate_enclosure(_, _, _) -> false.

validate_end_of_line(<<$\r, $\n>>) -> true;
validate_end_of_line(<<$\r>>) -> true;
validate_end_of_line(<<$\n>>) -> true;
validate_end_of_line(_) -> false.
