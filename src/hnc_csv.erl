%% Copyright (c) 2024-2025, Maria Scott <maria-12648430@hnc-agency.org>
%% Copyright (c) 2024-2025, Jan Uhlig <juhlig@hnc-agency.org>
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
%% @copyright 2024-2025 Maria Scott, Jan Uhlig
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

-type decode_options() :: #{'separator' := byte(),
			    'enclosure' := 'undefined' | byte(),
			    'quote' := 'undefined' | 'enclosure' | byte()}.

-type encode_options() :: #{'separator' := byte(),
			    'enclosure' := 'undefined' | byte(),
			    'quote' := 'undefined' | 'enclosure' | byte(),
			    'enclose' := 'never' | 'always' | 'optional',
			    'end_of_line' := binary()}.

-type provider() :: fun(() -> 'end_of_data' | {data(), provider()}).

-opaque state() :: fun(('flush' | data()) -> {'end_of_data' | csv_line(), state()}).

-export_type([state/0]).

%% @equiv decode(ProviderOrRawData, default_decode_options())
-spec decode(ProviderOrRawData) -> Lines when
	  ProviderOrRawData :: Provider | RawData,
	  Provider :: provider(),
	  RawData :: data(),
	  Lines :: [Line],
	  Line :: csv_line().
decode(ProviderOrData) ->
	decode(ProviderOrData, default_decode_options()).

%% @doc Decodes the raw CSV document in `RawData' or provided by `Provider',
%%      using the given `Options', into a CSV data structure.
%%
%%      The return value is a list of CSV lines, which in turn are lists of CSV fields,
%%      which in turn are binaries representing the CSV field values.
-spec decode(ProviderOrRawData, Options) -> Lines when
	  ProviderOrRawData :: Provider | RawData,
	  Provider :: provider(),
	  RawData :: data(),
	  Options :: decode_options(),
	  Lines :: [Line],
	  Line :: csv_line().
decode(ProviderOrData, Opts) ->
	lists:reverse(
		decode_fold(ProviderOrData,
			    Opts,
			    fun(Line, Acc) -> [Line|Acc] end,
			    [])).

%% @equiv decode_init(<<>>, default_decode_options())
-spec decode_init() -> State when
	  State :: state().
decode_init() ->
	decode_init(<<>>, default_decode_options()).

%% @doc Equivalent to {@link decode_init/2. decode_init(RawData, default_decode_options())}
%%      or {@link decode_init/2. decode_init(&lt;&lt;&gt;&gt;, Options)}, respectively.
-spec decode_init(RawDataOrOptions) -> State when
	  RawDataOrOptions :: RawData | Options,
	  RawData :: data(),
	  Options :: decode_options(),
	  State :: state().
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
-spec decode_init(RawData, Options) -> State when
	  RawData :: data(),
	  Options :: decode_options(),
	  State :: state().
decode_init(Data, Opts0) when is_binary(Data), is_map(Opts0) ->
	Opts = validate_decode_opts(Opts0),
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
-spec decode_next_line(State0) -> {Result, State1} when
	  State0 :: state(),
	  Result :: 'end_of_data' | Line,
	  Line :: csv_line(),
	  State1 :: state().
decode_next_line(Cont) when is_function(Cont, 1) ->
	Cont(<<>>).

%% @doc Adds another chunk of unprocessed `RawData' to the given decoder `State'.
%%
%%      Returns an updated state with the given data added.
-spec decode_add_data(State0, RawData) -> State1 when
	  State0 :: state(),
	  RawData :: data(),
	  State1 :: state().
decode_add_data(Cont, Data) ->
	fun(MoreData) -> Cont(<<Data/binary, MoreData/binary>>) end.

%% @doc Flushes a possibly unfinished line together with any yet unprocessed data from the state.
%%
%%      If there is no possibly unfinished line in the state, the atom `undefined' is returned
%%      instead of a line.
-spec decode_flush(State) -> {Result, Rest} when
	  State :: state(),
	  Result :: 'undefined' | Line,
	  Line :: csv_line(),
	  Rest :: data().
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

%% @equiv decode_fold(ProviderOrRawData, default_decode_options(), Fun, Acc0)
-spec decode_fold(ProviderOrRawData, Fun, Acc0) -> Acc1 when
	  ProviderOrRawData :: Provider | RawData,
	  Provider :: provider(),
	  RawData :: data(),
	  Fun :: fun((Line, AccIn) -> AccOut),
	  Line :: csv_line(),
	  Acc0 :: term(),
	  Acc1 :: term(),
	  AccIn :: term(),
	  AccOut :: term().
decode_fold(Provider, Fun, Acc0) ->
	decode_fold(Provider, default_decode_options(), Fun, Acc0).

%% @doc Successively calls the given function `Fun' with each
%%      CSV line decoded from the raw binary data in `RawData' or
%%      provided by `Provider' as first and an accumulator as second
%%      arguments.
%%
%%      Returns the final accumulator.
-spec decode_fold(ProviderOrRawData, Options, Fun, Acc0) -> Acc1 when
	  ProviderOrRawData :: Provider | RawData,
	  Provider :: provider(),
	  RawData :: data(),
	  Options :: decode_options(),
	  Fun :: fun((Line, AccIn) -> AccOut),
	  Line :: csv_line(),
	  Acc0 :: term(),
	  Acc1 :: term(),
	  AccIn :: term(),
	  AccOut :: term().
decode_fold(Data, Opts, Fun, Acc0) when is_binary(Data) ->
	decode_fold(get_binary_provider(Data), Opts, Fun, Acc0);
decode_fold(Provider, Opts, Fun, Acc0) when is_function(Provider, 0),
					    is_function(Fun, 2) ->
	decode_fold1(Provider(), decode_init(Opts), Fun, Acc0).

decode_fold1(end_of_data, State, Fun, Acc) ->
	case decode_flush(State) of
		{undefined, _} ->
			Acc;
		{Line, _} ->
			Fun(Line, Acc)
	end;
decode_fold1({MoreData, Provider}, State, Fun, Acc) ->
	decode_fold2(Provider, decode_add_data(State, MoreData), Fun, Acc).

decode_fold2(Provider, State0, Fun, Acc0) ->
	case decode_next_line(State0) of
		{end_of_data, State1} ->
			decode_fold1(Provider(), State1, Fun, Acc0);
		{Line, State1} ->
			decode_fold2(Provider, State1, Fun, Fun(Line, Acc0))
	end.

%% @equiv decode_foreach(ProviderOrRawData, default_decode_options(), Fun)
-spec decode_foreach(ProviderOrRawData, Fun) -> ok when
	  ProviderOrRawData :: Provider | RawData,
	  Provider :: provider(),
	  RawData :: data(),
	  Fun :: fun((Line) -> _),
	  Line :: csv_line().
decode_foreach(ProviderOrData, Fun) ->
	decode_foreach(ProviderOrData, default_decode_options(), Fun).

%% @doc Successively calls the given function `Fun' with each
%%      CSV line decoded from the raw binary data in `RawData' or provided
%%      by `Provider' as argument.
-spec decode_foreach(ProviderOrRawData, Options, Fun) -> ok when
	  ProviderOrRawData :: Provider | RawData,
	  Provider :: provider(),
	  RawData :: data(),
	  Options :: decode_options(),
	  Fun :: fun((Line) -> _),
	  Line :: csv_line().
decode_foreach(ProviderOrData, Opts, Fun) ->
	decode_fold(ProviderOrData,
		    Opts,
		    fun(Line, ok) -> Fun(Line), ok end,
		    ok).

%% @equiv decode_filter(ProviderOrRawData, default_decode_options(), Fun)
-spec decode_filter(ProviderOrRawData, Fun) -> Lines when
	  ProviderOrRawData :: Provider | RawData,
	  Provider :: provider(),
	  RawData :: data(),
	  Fun :: fun((Line) -> boolean()),
	  Line :: csv_line(),
	  Lines :: [csv_line()].
decode_filter(ProviderOrData, Fun) ->
	decode_filter(ProviderOrData, default_decode_options(), Fun).

%% @doc Successively calls the given function `Fun' with each
%%      CSV line decoded from the raw binary data in `RawData' or
%%      provided by `Provider' as argument.
%%
%%      Returns a list of CSV lines for which `Fun' returned
%%      `true'.
-spec decode_filter(ProviderOrRawData, Options, Fun) -> Lines when
	  ProviderOrRawData :: Provider | RawData,
	  Provider :: provider(),
	  RawData :: data(),
	  Options :: decode_options(),
	  Fun :: fun((Line) -> boolean()),
	  Line :: csv_line(),
	  Lines :: [csv_line()].
decode_filter(ProviderOrData, Opts, Fun) ->
	FoldFun = fun(Line, Acc) ->
			  case Fun(Line) of
				  true ->
					  [Line|Acc];
				  false ->
					  Acc
			  end
		  end,
	lists:reverse(
		decode_fold(ProviderOrData,
			    Opts,
			    FoldFun,
			    [])).

%% @equiv decode_map(ProviderOrRawData, default_decode_options(), Fun)
-spec decode_map(ProviderOrRawData, Fun) -> Result when
	  ProviderOrRawData :: Provider | RawData,
	  Provider :: provider(),
	  RawData :: data(),
	  Fun :: fun((Line) -> Mapped),
	  Line :: csv_line(),
	  Mapped :: term(),
	  Result :: [Mapped].
decode_map(ProviderOrData, Fun) ->
	decode_map(ProviderOrData, default_decode_options(), Fun).

%% @doc Successively calls the given function `Fun' with each
%%      CSV line decoded from the raw binary data in `RawData' or
%%      provided by `Provider' as argument.
%%
%%      Returns a list of the values returned by `Fun'.
-spec decode_map(ProviderOrRawData, Options, Fun) -> Result when
	  ProviderOrRawData :: Provider | RawData,
	  Provider :: provider(),
	  RawData :: data(),
	  Options :: decode_options(),
	  Fun :: fun((Line) -> Mapped),
	  Line :: csv_line(),
	  Mapped :: term(),
	  Result :: [Mapped].
decode_map(ProviderOrData, Opts, Fun) ->
	lists:reverse(
		decode_fold(ProviderOrData,
			    Opts,
			    fun(Line, Acc) -> [Fun(Line)|Acc] end,
			    [])).

%% @equiv decode_filtermap(ProviderOrRawData, default_decode_options(), Fun)
-spec decode_filtermap(ProviderOrRawData, Fun) -> Result when
	  ProviderOrRawData :: Provider | RawData,
	  Provider :: provider(),
	  RawData :: data(),
	  Fun :: fun((Line) -> boolean() | {'true', Mapped}),
	  Line :: csv_line(),
	  Mapped :: term(),
	  Result :: [Line | Mapped].
decode_filtermap(ProviderOrData, Fun) ->
	decode_filtermap(ProviderOrData, default_decode_options(), Fun).

%% @doc Combines the functionality of `decode_filter' and `decode_map'.
-spec decode_filtermap(ProviderOrRawData, Options, Fun) -> Result when
	  ProviderOrRawData :: Provider | RawData,
	  Provider :: provider(),
	  RawData :: data(),
	  Options :: decode_options(),
	  Fun :: fun((Line) -> boolean() | {'true', Mapped}),
	  Line :: csv_line(),
	  Mapped :: term(),
	  Result :: [Line | Mapped].
decode_filtermap(ProviderOrData, Opts, Fun) ->
	FoldFun = fun(Line, Acc) ->
			  case Fun(Line) of
				  true ->
					  [Line|Acc];
				  {true, V} ->
					  [V|Acc];
				  false ->
					  Acc
			  end
		  end,
	lists:reverse(
		decode_fold(ProviderOrData,
			    Opts,
			    FoldFun,
			    [])).

%% @equiv get_binary_provider(Binary, 1024)
-spec get_binary_provider(Binary) -> Provider when
	  Binary :: binary(),
	  Provider ::provider().
get_binary_provider(Bin) when is_binary(Bin) ->
	get_binary_provider(Bin, 1024).

%% @doc Creates a data provider from the given `Binary' to supply
%%      data in chunks of the given `ChunkSize'.
%%
%%      This provider can be used in
%%      {@link decode_fold/4. `decode_fold/3,4'},
%%      {@link decode_foreach/3. `decode_foreach/2,3'},
%%      {@link decode_filter/3. `decode_filter/2,3'},
%%      {@link decode_map/3. `decode_map/2,3'} and
%%      {@link decode_filtermap/3. `decode_filtermap/2,3'}.
-spec get_binary_provider(Binary, ChunkSize) -> Provider when
	  Binary :: binary(),
	  ChunkSize :: pos_integer(),
	  Provider :: provider().
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

%% @equiv get_file_provider(IoDevice, 1024)
-spec get_file_provider(IoDevice) -> Provider when
	  IoDevice :: file:io_device() | io:device(),
	  Provider :: provider().
get_file_provider(Filename) ->
	get_file_provider(Filename, 1024).

%% @doc Creates a data provider from an open file given in
%%      `IoDevice' to supply data in chunks of the given
%%      `ChunkSize'.
%%
%%      The file must have been opened with modes `read' and `binary'.
%%
%%      When the provider is exhausted, the position of the `IoDevice'
%%      is at `eof'.
%%
%%      The `IoDevice' is not closed implicitly by the provider, instead
%%      the code using this provider is responsible for closing it.
%%
%%      This provider can be used in
%%      {@link decode_fold/4. `decode_fold/3,4'},
%%      {@link decode_foreach/3. `decode_foreach/2,3'},
%%      {@link decode_filter/3. `decode_filter/2,3'},
%%      {@link decode_map/3. `decode_map/2,3'} and
%%      {@link decode_filtermap/3. `decode_filtermap/2,3'}.
-spec get_file_provider(IoDevice, ChunkSize) -> Provider when
	  IoDevice :: file:io_device() | io:device(),
	  ChunkSize :: pos_integer(),
	  Provider :: provider().
get_file_provider(IoDevice, ChunkSize) when is_integer(ChunkSize), ChunkSize > 0 ->
	fun() -> file_provider(IoDevice, ChunkSize) end.

file_provider(Io, ChunkSize) ->
	case file:read(Io, ChunkSize) of
		eof ->
			end_of_data;
		{ok, Data} when is_binary(Data) ->
			{Data, fun() -> file_provider(Io, ChunkSize) end}
	end.

%% @equiv encode(Lines, default_encode_options())
-spec encode(Lines) -> RawData when
	  Lines :: [Line],
	  Line :: csv_line(),
	  RawData :: data().
encode(Lines) ->
	encode(Lines, default_encode_options()).

%% @doc Encodes the given CSV data structure into a CSV binary,
%%      using the given `Options'.
-spec encode(Lines, Options) -> RawData when
	  Lines :: [Line],
	  Line :: csv_line(),
	  Options :: encode_options(),
	  RawData :: data().
encode(Lines, Opts0) ->
	Opts = validate_encode_opts(Opts0),
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
%%   <li>`quote': `enclosure'</li>
%% </ul>
-spec default_decode_options() -> decode_options().
default_decode_options() ->
	#{separator => $,,
	  enclosure => $",
	  quote => enclosure}.

%% @doc Returns the default encode options.
%%
%% <ul>
%%   <li>`separator': `$,'</li>
%%   <li>`enclosure': `$"'</li>
%%   <li>`quote': `enclosure'</li>
%%   <li>`enclose': `optional'</li>
%%   <li>`end_of_line': `<<"\r\n">>'</li>
%% </ul>
-spec default_encode_options() -> encode_options().
default_encode_options() ->
	#{separator => $,,
	  enclosure => $",
	  quote => enclosure,
	  enclose => optional,
	  end_of_line => <<$\r, $\n>>}.

validate_decode_opts(Opts) when not is_map_key(separator, Opts) ->
	validate_decode_opts(Opts#{separator => $,});
validate_decode_opts(Opts) when not is_map_key(enclosure, Opts) ->
	validate_decode_opts(Opts#{enclosure => $"});
validate_decode_opts(#{enclosure:=Enc}=Opts) when not is_map_key(quote, Opts) ->
	validate_decode_opts(Opts#{quote => Enc});
validate_decode_opts(#{enclosure:=Enc, quote:=enclosure}=Opts) ->
	validate_decode_opts(Opts#{quote => Enc});
validate_decode_opts(#{separator:=Sep, enclosure:=Enc, quote:=Quot}=Opts) ->
	case
       			validate_separator(Sep)
		andalso validate_enclosure(Enc, Quot)
		andalso Sep=/=Enc
	of
		true ->
			Opts;
		false ->
			error({badopts, Opts})
	end;
validate_decode_opts(Opts) ->
       	error({badopts, Opts}).

validate_encode_opts(Opts) when not is_map_key(separator, Opts) ->
	validate_encode_opts(Opts#{separator => $,});
validate_encode_opts(Opts) when not is_map_key(enclosure, Opts) ->
	validate_encode_opts(Opts#{enclosure => $"});
validate_encode_opts(#{enclosure:=Enc}=Opts) when not is_map_key(quote, Opts) ->
	validate_encode_opts(Opts#{quote => Enc});
validate_encode_opts(#{enclosure:=Enc, quote:=enclosure}=Opts) ->
	validate_encode_opts(Opts#{quote => Enc});
validate_encode_opts(Opts) when not is_map_key(enclose, Opts) ->
	validate_encode_opts(Opts#{enclose => optional});
validate_encode_opts(Opts) when not is_map_key(end_of_line, Opts) ->
	validate_encode_opts(Opts#{end_of_line => <<$\r, $\n>>});
validate_encode_opts(#{separator:=Sep, enclosure:=Enc, quote:=Quot, enclose:=EncPolicy, end_of_line:=EOL}=Opts) ->
	case
			validate_separator(Sep)
		andalso validate_enclosure(EncPolicy, Enc, Quot)
		andalso validate_end_of_line(EOL)
		andalso Sep=/=Enc
	of
		true ->
			Opts;
		false ->
			error({badopts, Opts})
	end;
validate_encode_opts(Opts) ->
	error({badopts, Opts}).

validate_separator($\r) -> false;
validate_separator($\n) -> false;
validate_separator(C) when is_integer(C), C>=16#00, C=<16#FF -> true;
validate_separator(_) -> false.

validate_enclosure(undefined, undefined) -> true;
validate_enclosure($\r, _) -> false;
validate_enclosure($\n, _) -> false;
validate_enclosure(_, $\r) -> false;
validate_enclosure(_, $\n) -> false;
validate_enclosure(Enc, Quot) when is_integer(Enc), Enc>=16#00, Enc=<16#FF,
				   is_integer(Quot), Quot>=16#00, Quot=<16#FF -> true;
validate_enclosure(_, _) -> false.

validate_enclosure(never, undefined, undefined) -> true;
validate_enclosure(_, $\r, _) -> false;
validate_enclosure(_, $\n, _) -> false;
validate_enclosure(_, _, $\r) -> false;
validate_enclosure(_, _, $\n) -> false;
validate_enclosure(optional, Enc, Quot) when is_integer(Enc), Enc>=16#00, Enc=<16#FF,
					     is_integer(Quot), Quot>=16#00, Quot=<16#FF -> true;
validate_enclosure(always, Enc, Quot) when is_integer(Enc), Enc>=16#00, Enc=<16#FF,
					   is_integer(Quot), Quot>=16#00, Quot=<16#FF -> true;
validate_enclosure(_, _, _) -> false.

validate_end_of_line(<<$\r, $\n>>) -> true;
validate_end_of_line(<<$\r>>) -> true;
validate_end_of_line(<<$\n>>) -> true;
validate_end_of_line(_) -> false.
