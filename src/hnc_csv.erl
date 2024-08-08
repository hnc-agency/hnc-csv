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

%% @doc CSV Decoder/Encoder
%%
%% @author Maria Scott <maria-12648430@hnc-agency.org>
%% @author Jan Uhlig <juhlig@hnc-agency.org>
%% @copyright 2024 Maria Scott, Jan Uhlig
-module(hnc_csv).

-export([decode/1, decode/2]).
-export([decode_init/0, decode_init/1, decode_init/2]).
-export([decode_add_data/2]).
-export([decode_next_line/1]).
-export([decode_flush/1]).
-export([decode_fold/3, decode_fold/4]).
-export([decode_foreach/2, decode_foreach/3]).
-export([decode_filter/2, decode_filter/3]).
-export([decode_map/2, decode_map/3]).
-export([decode_filtermap/2, decode_filtermap/3]).
-export([default_decode_options/0]).

-export([encode/1, encode/2]).
-export([default_encode_options/0]).

-export([get_binary_provider/1, get_binary_provider/2]).
-export([get_file_provider/1, get_file_provider/2]).

-type data() :: binary().

-type csv_field() :: binary().

-type csv_line() :: [csv_field()].

-type decode_options() :: #{'separator' := $, | $; | $\t,
			    'enclosure' := 'undefined' | $" | $',
			    'quote' := 'undefined' | $\\ | $" | $'}.

-type encode_options() :: #{'separator' := $, | $; | $\t,
			   'enclosure' := 'undefined' | $" | $',
			   'quote' := 'undefined' | $\\ | $" | $',
			   'enclose' := 'never' | 'always' | 'optional',
			   'end_of_line' := binary()}.

-type provider() :: fun(() -> 'end_of_data' | {data(), provider()}).

-opaque state() :: fun(('flush' | data()) -> {'end_of_data' | csv_line(), state()}).

-export_type([state/0]).

%% @equiv decode(RawData, default_decode_options())
-spec decode(RawData :: data()) -> Lines :: [csv_line()].
decode(Data) ->
	decode(Data, default_decode_options()).

%% @doc Decodes the raw CSV document given in `RawData', using the given `Options',
%%      into a CSV data structure.
%%
%%      The return value is a list of CSV lines, which in turn are lists of CSV fields,
%%      which in turn are binaries representing the CSV field values.
-spec decode(RawData :: data(), Options :: decode_options()) -> Lines :: [csv_line()].
decode(Data, Opts) when is_binary(Data), is_map(Opts) ->
	decode1(decode_init(Data, Opts), []).

decode1(Cont, Acc) ->
	decode2(decode_next_line(Cont), Acc).

decode2({end_of_data, Cont}, Acc) ->
	decode3(decode_flush(Cont), Acc);
decode2({Line, Cont}, Acc) ->
	decode1(Cont, [Line|Acc]).

decode3({undefined, _}, Acc) ->
	lists:reverse(Acc);
decode3({LastLine, _}, Acc) ->
	lists:reverse([LastLine|Acc]).

%% @equiv decode_init(<<>>, default_decode_options())
-spec decode_init() -> State :: state().
decode_init() ->
	decode_init(<<>>, default_decode_options()).

%% @doc Equivalent to {@link decode_init/2. `decode_init(Data, default_decode_options())'}
%%      or {@link decode_init/2. `decode_init(<<>>, Options)'}, respectively.
-spec decode_init(DataOrOptions :: (data() | decode_options())) -> State :: state().
decode_init(Data) when is_binary(Data) ->
	decode_init(Data, default_decode_options());
decode_init(Opts) when is_map(Opts) ->
	decode_init(<<>>, Opts).

%% @doc Creates a CSV decoder state, prepopulated with the given `RawData' and using
%%      the given `Options'.
%%
%%      The return value can be used in the functions {@link decode_add_data/2},
%%      {@link decode_next_line/1} and {@link decode_flush/1} to incrementally
%%      decode a CSV document.
%%
%% @see decode_add_data/2
%% @see decode_next_line/1
%% @see decode_flush/1
-spec decode_init(RawData :: data(), Options :: decode_options()) -> State :: state().
decode_init(Data, Opts) when is_binary(Data), is_map(Opts) ->
	ok = validate_decode_opts(Opts),
	fun
		(flush) ->
			{undefined, Data};
		(MoreData) ->
			do_decode(undefined, <<Data/binary, MoreData/binary>>, Opts, <<>>, [])
	end.

%% @doc Decodes and returns the next CSV line in the given decoder state, together with an updated
%%      state which can be used for further incremental decoding.
%%
%%      If the decoder state is exhausted, the atom `end_of_data' is returned instead of a line. In
%%      this case, the function {@link decode_add_data/2} can be used to add more data to the state,
%%      or {@link decode_flush/1} can be used to flush a possibly unfinished line together with any
%%      yet unprocessed data from the state.
%%
%% @see decode_add_data/2
%% @see decode_flush/1
-spec decode_next_line(State0 :: state()) -> {Result :: ('end_of_data' | csv_line()), State1 :: state()}.
decode_next_line(Cont) when is_function(Cont, 1) ->
	Cont(<<>>).

%% @doc Adds another chunk of unprocessed `RawData' to the given decoder `State'.
%%
%%      Returns an updated state with the given data added.
-spec decode_add_data(State0 :: state(), RawData :: data()) -> State1 :: state().
decode_add_data(Cont, Data) ->
	fun(MoreData) -> Cont(<<Data/binary, MoreData/binary>>) end.

%% @doc Flushes a possibly unfinished line together with any yet unprocessed data from the state.
%%
%%      If there is no possibly unfinished line in the state, the atom `undefined' is returned
%%      instead of a line.
-spec decode_flush(State :: state()) -> {Line :: ('undefined' | csv_line()), Rest :: data()}.
decode_flush(Cont) when is_function(Cont, 1) ->
	Cont(flush).

do_decode(Mode, More, #{quote:=Quot}=Opts, FieldAcc, LineAcc) when More =:= <<>>;
								   Mode =:= enclosed_field, More =:= <<Quot>> ->
	{end_of_data,
	 fun
		(flush) when Mode=:=undefined ->
			{undefined, More};
		(flush) ->
			{lists:reverse([<<FieldAcc/binary, More/binary>>|LineAcc]), <<>>};
		(Data) ->
			do_decode(Mode, <<More/binary, Data/binary>>, Opts, FieldAcc, LineAcc)
	 end};
do_decode(undefined, <<$\r, More/binary>>, Opts, FieldAcc, LineAcc) ->
	do_decode(undefined, More, Opts, FieldAcc, LineAcc);
do_decode(undefined, <<$\n, More/binary>>, Opts, FieldAcc, LineAcc) ->
	do_decode(undefined, More, Opts, FieldAcc, LineAcc);
do_decode(line, <<$\r, More/binary>>, Opts, FieldAcc, LineAcc) ->
	do_decode_eol(More, Opts, FieldAcc, LineAcc);
do_decode(line, <<$\n, More/binary>>, Opts, FieldAcc, LineAcc) ->
	do_decode_eol(More, Opts, FieldAcc, LineAcc);
do_decode(field, <<$\r, More/binary>>, Opts, FieldAcc, LineAcc) ->
	do_decode_eol(More, Opts, FieldAcc, LineAcc);
do_decode(field, <<$\n, More/binary>>, Opts, FieldAcc, LineAcc) ->
	do_decode_eol(More, Opts, FieldAcc, LineAcc);
do_decode(undefined, More, Opts, FieldAcc, LineAcc) ->
	do_decode(line, More, Opts, FieldAcc, LineAcc);
do_decode(line, <<Sep, More/binary>>, #{separator:=Sep}=Opts, _FieldAcc, LineAcc) ->
	do_decode(line, More, Opts, <<>>, [<<>>|LineAcc]);
do_decode(field, <<Sep, More/binary>>, #{separator:=Sep}=Opts, FieldAcc, LineAcc) ->
	do_decode(line, More, Opts, <<>>, [FieldAcc|LineAcc]);
do_decode(line, More, Opts, FieldAcc, LineAcc) ->
	do_decode(field, More, Opts, FieldAcc, LineAcc);
do_decode(field, <<Enc, More/binary>>, #{enclosure:=Enc}=Opts, <<>>, LineAcc) ->
	do_decode(enclosed_field, More, Opts, <<>>, LineAcc);
do_decode(field, <<C, More/binary>>, #{enclosure:=Enc}=Opts, FieldAcc, LineAcc) when C=/=Enc ->
	do_decode(field, More, Opts, <<FieldAcc/binary, C>>, LineAcc);
do_decode(enclosed_field, <<Quot, Quot, More/binary>>, #{enclosure:=Enc, quote:=Quot}=Opts, FieldAcc, LineAcc) when Quot=/=Enc ->
	do_decode(enclosed_field, More, Opts, <<FieldAcc/binary, Quot>>, LineAcc);
do_decode(enclosed_field, <<Quot, Enc, More/binary>>, #{enclosure:=Enc, quote:=Quot}=Opts, FieldAcc, LineAcc) ->
	do_decode(enclosed_field, More, Opts, <<FieldAcc/binary, Enc>>, LineAcc);
do_decode(enclosed_field, <<Enc, More/binary>>, #{enclosure:=Enc}=Opts, FieldAcc, LineAcc) ->
	do_decode(field, More, Opts, FieldAcc, LineAcc);
do_decode(enclosed_field, <<C, More/binary>>, Opts, FieldAcc, LineAcc) ->
	do_decode(enclosed_field, More, Opts, <<FieldAcc/binary, C>>, LineAcc).

do_decode_eol(More, Opts, FieldAcc, LineAcc) ->
	{lists:reverse([FieldAcc|LineAcc]),
	 fun
		(flush) ->
			{undefined, More};
		(Data) ->
			do_decode(undefined, <<More/binary, Data/binary>>, Opts, <<>>, [])
	 end}.

decode_fold(Provider, FoldFun, Acc0) ->
	decode_fold(Provider, default_decode_options(), FoldFun, Acc0).

decode_fold(Provider, Opts, FoldFun, Acc0) when is_function(Provider, 0),
						is_function(FoldFun, 2) ->
	ok = validate_decode_opts(Opts),
	decode_fold1(Provider(), decode_init(Opts), FoldFun, Acc0).

decode_fold1(end_of_data, State, FoldFun, Acc) ->
	case decode_flush(State) of
		{undefined, _} ->
			Acc;
		{Line, _} ->
			FoldFun(Line, Acc)
	end;
decode_fold1({MoreData, Provider}, State, FoldFun, Acc) ->
	decode_fold2(Provider, decode_add_data(State, MoreData), FoldFun, Acc).

decode_fold2(Provider, State0, FoldFun, Acc0) ->
	case decode_next_line(State0) of
		{end_of_data, State1} ->
			decode_fold1(Provider(), State1, FoldFun, Acc0);
		{Line, State1} ->
			decode_fold2(Provider, State1, FoldFun, FoldFun(Line, Acc0))
	end.

decode_foreach(Provider, Fun) ->
	decode_foreach(Provider, default_decode_options(), Fun).

decode_foreach(Provider, Opts, Fun) ->
	decode_fold(Provider, Opts, fun(Line, ok) -> Fun(Line), ok end, ok).

decode_filter(Provider, FilterFun) ->
	decode_filter(Provider, default_decode_options(), FilterFun).

decode_filter(Provider, Opts, FilterFun) ->
	FoldFun = fun(Line, Acc) ->
		case FilterFun(Line) of
			true ->
				[Line|Acc];
			false ->
				Acc
		end
	end,
	lists:reverse(decode_fold(Provider, Opts, FoldFun, [])).

decode_map(Provider, FilterFun) ->
	decode_map(Provider, default_decode_options(), FilterFun).

decode_map(Provider, Opts, MapFun) ->
	lists:reverse(decode_fold(Provider, Opts, fun(Line, Acc) -> [MapFun(Line)|Acc] end, [])).

decode_filtermap(Provider, FilterMapFun) ->
	decode_filtermap(Provider, default_decode_options(), FilterMapFun).

decode_filtermap(Provider, Opts, FilterMapFun) ->
	FoldFun = fun(Line, Acc) ->
		case FilterMapFun(Line) of
			true ->
				[Line|Acc];
			{true, V} ->
				[V|Acc];
			false ->
				Acc
		end
	end,
	lists:reverse(decode_fold(Provider, Opts, FoldFun, [])).

-spec get_binary_provider(Bin :: binary()) -> provider().
get_binary_provider(Bin) when is_binary(Bin) ->
	fun() -> {Bin, fun() -> end_of_data end} end.

-spec get_binary_provider(Bin :: binary(), ChunkSize :: pos_integer()) -> provider().
get_binary_provider(Bin, ChunkSize) when is_binary(Bin),
					 is_integer(ChunkSize), ChunkSize > 0 ->
	fun() -> binary_provider(Bin, ChunkSize) end.

binary_provider(<<>>, _ChunkSize) ->
	end_of_data;
binary_provider(Bin, ChunkSize) ->
	case Bin of
		<<Data:ChunkSize/binary, More/binary>> ->
			{Data, fun() -> binary_provider(More, ChunkSize) end};
		Data ->
			{Data, fun() -> end_of_data end}
	end.

%% @equiv get_file_provider(Filename, 1024)
-spec get_file_provider(Filename :: file:name_all()) -> provider().
get_file_provider(Filename) ->
	get_file_provider(Filename, 1024).

-spec get_file_provider(Filename :: file:name_all(), ChunkSize :: pos_integer()) -> provider().
get_file_provider(Filename, ChunkSize) when is_integer(ChunkSize), ChunkSize > 0 ->
	{ok, Io} = file:open(Filename, [read, binary]),
	fun() -> file_provider(Io, ChunkSize) end.

file_provider(Io, ChunkSize) ->
	try
		file:read(Io, ChunkSize)
	of
		eof ->
			_ = file:close(Io),
			end_of_data;
		{ok, Data} ->
			{Data, fun() -> file_provider(Io, ChunkSize) end};
		{error, Reason} ->
			_ = file:close(Io),
			error(Reason)
	catch
		C:E:ST ->
			_ = file:close(Io),
			erlang:raise(C, E, ST)
	end.

%% @equiv encode(Lines, default_encode_options())
-spec encode(Lines :: [csv_line()]) -> RawData :: data().
encode(Lines) ->
	encode(Lines, default_encode_options()).

%% @doc Encodes the given CSV data structure into a CSV binary, using the given `Options'.
-spec encode(Lines :: [csv_line()], Options :: encode_options()) -> RawData :: data().
encode(Lines, Opts) ->
	ok = validate_encode_opts(Opts),
	encode(Lines, Opts, undefined).

encode([], _Opts, undefined) ->
	<<>>;
encode([], _Opts, Acc) ->
	Acc;
encode([Line|More], Opts, undefined) ->
	encode(More, Opts, encode_line(Line, Opts));
encode([Line|More], Opts, Acc) ->
	encode(More, Opts, <<Acc/binary, (encode_line(Line, Opts))/binary>>).

encode_line(Fields, Opts) ->
	encode_line(Fields, Opts, undefined).

encode_line([], _Opts, undefined) ->
	<<>>;
encode_line([], #{end_of_line:=EOL}, Acc) ->
	<<Acc/binary, EOL/binary>>;
encode_line([Field|More], Opts, undefined) ->
	encode_line(More, Opts, encode_field(Field, Opts));
encode_line([Field|More], #{separator:=Sep}=Opts, Acc) ->
	encode_line(More, Opts, <<Acc/binary, Sep, (encode_field(Field, Opts))/binary>>).

encode_field(<<>>, #{enclosure:=Enc, enclose:=always}) ->
	<<Enc, Enc>>;
encode_field(<<_/binary>>=Field, #{enclosure:=Enc, quote:=Quot, enclose:=always}) ->
	<<Enc, (binary:replace(Field, [<<Enc>>, <<Quot>>], <<Quot>>, [global, {insert_replaced, 1}]))/binary, Enc>>;
encode_field(<<>>, #{enclose:=never}) ->
	<<>>;
encode_field(<<_/binary>>=Field, #{separator:=Sep, enclose:=never}) ->
	case nomatch =:= binary:match(Field, [<<Sep>>, <<$\r>>, <<$\n>>]) of
		true -> Field;
		false -> error(special_char_in_field)
	end;
encode_field(<<>>, #{enclose:=optional}) ->
	<<>>;
encode_field(<<_/binary>>=Field, #{separator:=Sep, enclosure:=Enc, quote:=Quot, enclose:=optional}) ->
	case nomatch =:= binary:match(Field, [<<Sep>>, <<Enc>>, <<$\r>>, <<$\n>>]) of
		true -> Field;
		false -> <<Enc, (binary:replace(Field, [<<Enc>>, <<Quot>>], <<Quot>>, [global, {insert_replaced, 1}]))/binary, Enc>>
	end.

%% @doc Returns the default decode options.
%%
%% <ul>
%%   <li>`separator': `$,'</li>
%%   <li>`enclosure': `$"'</li>
%%   <li>`quote': `$"'</li>
%% </ul>
-spec default_decode_options() -> decode_options().
default_decode_options() ->
	#{separator => $,,
	  enclosure => $",
	  quote => $"}.

%% @doc Returns the default encode options.
%%
%% <ul>
%%   <li>`separator': `$,'</li>
%%   <li>`enclosure': `$"'</li>
%%   <li>`quote': `$"'</li>
%%   <li>`encode': `optionally'</li>
%%   <li>`end_of_line': `<<"\r\n">>'</li>
%% </ul>
-spec default_encode_options() -> encode_options().
default_encode_options() ->
	#{separator => $,,
	  enclosure => $",
	  quote => $",
	  enclose => optional,
	  end_of_line => <<$\r, $\n>>}.

validate_decode_opts(#{separator:=Sep, enclosure:=Enc, quote:=Quot}=Opts) ->
	case
       			validate_separator(Sep)
		andalso validate_enclosure(Enc, Quot)
	of
		true ->
			ok;
		false ->
			error({badopts, Opts})
	end;
validate_decode_opts(Opts) ->
       	error({badopts, Opts}).

validate_encode_opts(#{separator:=Sep, enclosure:=Enc, quote:=Quot, enclose:=EncPolicy, end_of_line:=EOL}=Opts) ->
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
validate_encode_opts(Opts) ->
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
