# hnc-csv - CSV Decoder/Encoder

## Decoding

Whole CSV binary documents can be decoded with `decode/1,2`.

`decode/1` assumes default [RFC4180](https://www.ietf.org/rfc/rfc4180.txt)-style
options, that is:

* Fields are separated by commas.
* Fields are optionally enclosed in double quotes.
* Double quotes in enclosed fields are quoted by another double quote.

`decode/2` allows using custom options:
```erlang
#{separator => Separator, % any byte except $\r or $\n (defaul $,)
  enclosure => Enclosure, % 'undefined' or any byte except $\r or $\n (default $")
  quote     => Quote}     % 'undefined', 'enclosure', or any byte except $\r or $\n (defaults 'enclosure')
```
_Restrictions for option combinations:_
* If `Enclosure` is `undefined` (ie, no enclosing), `Quote` must be either `enclosure` or `undefined`.
* If `Enclosure` is _not_ `undefined`, `Quote` must also not be `undefined`.
* If `Enclosure` is _not_ `undefined`, it must _not_ be the same as `Separator`.

Lines are separated by `\r`, `\n` or `\r\n`. Empty lines are ignored by the decoder.

The result of decoding is a list of CSV lines, which are lists of CSV fields,
which are in turn binaries representing the field values on the respective line.

#### Example

Assume the following CSV data:
```text
a,b,c
"d,d","e""e","f
f"
```

In an Erlang binary, this will look like:
```erlang
1> CsvBinary = <<"a,b,c\r\n\"d,d\",\"e\"\"e\",\"f\r\nf\"\r\n">>.
<<"a,b,c\r\n\"d,d\",\"e\"\"e\",\"f\r\nf\"\r\n">>
```

Decoded with `decode/1`, this will become:
```erlang
2> hnc_csv:decode(CsvBinary).
[[<<"a">>,<<"b">>,<<"c">>],
 [<<"d,d">>,<<"e\"e">>,<<"f\r\nf">>]]
```

### Higher Order Functions for Decoding

`hnc_csv` provides the functions `decode_fold/3,4`, `decode_filter/2,3`,
`decode_map/2,3`, `decode_filtermap/2,3` and `decode_foreach/2,3` which
allow decoding and processing decoded lines in one operation, much
like the `lists` functions `foldl/3`, `filter/2`, `map/2`, `filtermap/2`
and `foreach/2`.

In fact, `decode/1,2` is implemented via `decode_fold/3,4`.

### Providers

The `decode` family of functions accepts both a raw binary as well as a
`Provider` that delivers chunks of raw binary. When given a raw binary,
it is converted into a binary provider for further processing.

A provider is a 0-arity function which, when called, returns either a
tuple where the first element is a chunk of binary data and the second
is a new provider function for the next chunk of data, or the atom
`end_of_data` to indicate that the provider has delivered all data.

Providers can be implemented stateless of stateful, usually depending
on the characteristics of the underlying data source.

A stateless provider does not change and is not susceptible to external
changes to the state of the underlying data source.

A stateful provider on the other hand may change or be susceptible to
changes to the state of the underlying data source or both. It is recommended
to not (re-)use stateful providers or their underlying data source before, while
or after being used in decoding functions, except for any necessary setup before or
cleanup after being used.

`hnc_csv` comes with two convenience functions, `get_binary_provider/1,2`
(stateless) and `get_file_provider/1,2` (stateful) which return providers for
binaries or files, respectively.

##### Example

The following is an implementation of a (stateless) custom provider which delivers
data taken from a given list of binaries:
```erlang
-module(example_provider).
-export([get_list_provider/1]).

get_list_provider(L) ->
    fun() -> list_provider(L) end.

list_provider([]) ->
    end_of_data;
list_provider([Bin|More]) when is_binary(Bin) ->
    {Bin, fun() -> list_provider(More) end}.
```
* `get_list_provider/1` creates the initial provider, which is a call
  to `list_provider/1` wrapped in a 0-arity function.
* `list_provider/1` is the actual implementation of the provider, which
  returns either `end_of_data` when the list given as argument is exhausted,
  or otherwise a tuple with the head element of the list as first
  and a call to itself with the tail of the list wrapped in a 0-arity
  function as second element.

This provider can then be used as follows, for example to count the lines
and fields in the CSV data which the provider delivers:
```erlang
1> Provider = example_provider:get_list_provider([<<"a,b">>, <<",c\r">>,
                                                  <<"\nd,">>, <<"e,f">>,
                                                  <<"\r\n">>]).
#Fun<example_provider.0.64990923>
2> hnc_csv:decode_fold(Provider,
                       fun(Line, {LCnt, FCnt}) -> {LCnt+1, FCnt+length(Line)} end,
                       {0, 0}).
{2,6}
```

### Advanced Usage

For more complex scenarios than what the built-in functions provide
for, the functions `decode_init/0,1,2`, `decode_next_line/1` and
`decode_flush/1` can be used together to decode and process CSV
documents incrementally.

* `decode_init/0,1,2` creates a decoder state to be used in the
  other functions listed above.
* `decode_next_line/1` decodes and returns the next line, together with
  an updated state. If the data in the provider backing the state is exhausted,
  the atom `end_of_data` is returned instead of a line.
* `decode_flush/1` returns all as by then unread lines in the given state.

In fact, `decode_fold/4` is implemented using those functions.

## Encoding

CSV documents can be encoded with `encode/1,2`.

`encode/1` assumes default [RFC4180](https://www.ietf.org/rfc/rfc4180.txt)-style
options, that is:

* Fields are separated by commas
* Fields are optionally enclosed in double quotes
* Double quotes in enclosed fields are quoted by another double quote
* Lines are separated by `\r\n`

`encode/2` allows using custom options:
```erlang
#{separator   => Separator, % any byte except $\r and $\n (default $,)
  enclosure   => Enclosure, % 'undefined' or any byte except $\r or $\n (default $")
  quote       => Quote,     % 'undefined', 'enclosure', or any byte except $\r or $\n (default 'enclosure')
  enclose     => Enclose,   % 'optional' (default), 'never' or 'always'
  end_of_line => EndOfLine} % `<<"\r\n">> (default), <<"\n">> or <<"\r">>
```
_Restrictions for option combinations:_
* If `Enclose` is `never` (ie, no enclosing), `Enclosure` must be `undefined` and `Quote` must be `undefined` or `enclosure`.
* If `Enclose` is `optional` or `always`, `Enclosure` and `Quote` must _not_ be `undefined`.
* If `Enclosure` is _not_ `undefined`, it must not be the same as `Separator`.

The input for encoding is a list of CSV lines, which are in turn lists of CSV fields,
which are in turn binaries representing the field values.

The result is a CSV binary document consisting of the given CSV lines, in turn consisting of the given CSV fields of a line.

#### Example

Assume the following CSV structure:
```erlang
1> Csv = [[<<"a">>,<<"b">>,<<"c">>],
          [<<"d,d">>,<<"e\"e">>,<<"f\r\nf">>]].
```

Encoded with `encode/1`, this will become:
```erlang
2> hnc_csv:encode(Csv).
<<"a,b,c\r\n"
  "\"d,d\",\"e\"\"e\",\"f\r\nf\"\r\n">>
```

# Authors

* Maria Scott (Maria-12648430)
* Jan Uhlig (juhlig)
