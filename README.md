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
#{separator => Separator, % $, (default), $; or $\t
  enclosure => Enclosure, % $" (default), $' or 'undefined'
  quote     => Quote}     % $" (default), $', $\\ or 'undefined'
```
_Restrictions for option combinations:_
* If `Enclosure` is `undefined` (ie, no enclosing), `Quote` must also be `undefined`.
* If `Enclosure` is `$"`, `Quote` can be `$"` or `$\\`.
* If `Enclosure` is `$'`, `Quote` can be `$'` or `$\\`.

Lines are separated by `\r`, `\n` or `\r\n`. Empty lines are ignored by the decoder.

The result of decoding is a list of CSV lines, which are in turn lists of CSV fields,
which are in turn binaries representing the field values.

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

#### Providers

Those functions take a `provider` as their first parameter. A provider
here means a 0-arity function which, when called, returns either a tuple
where the first element is a chunk of binary data and the second is
a new provider function for the next chunk of data, or the atom
`end_of_data` to indicate that the provider has delivered all data.

`hnc_csv` comes with two convenience functions, `get_binary_provider/1,2`
and `get_file_provider/1,2` which return providers for binaries or
files, respectively.

##### Example

The following is an implementation of a provider which delivers data
taken from a given list of binaries:
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
for, the functions `decode_init/0,1,2`, `decode_add_data/2`,
`decode_next_line/1` and `decode_flush/1` can be used together to
decode and process CSV documents.

* `decode_init/0,1,2` creates a decoder state to be used in the
  other functions listed above.
* `decode_add_data/2` adds another chunk of unprocessed data to the
  state and returns an updated state.
* `decode_next_line/1` decodes and returns the next line, together with
  an updated state. If the data in the state is exhausted, the atom
  `end_of_data` is returned instead of a line.
* `decode_flush/1` returns any as yet unfinished line in the given state,
  together with any yet unprocessed data. If there is no unfinished line
  in the state, the atom `undefined` is returned instead of a line.

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
#{separator   => Separator, % $, (default), $; or $\t
  enclosure   => Enclosure, % $" (default), $' or 'undefined'
  quote       => Quote,     % $" (default), $', $\\ or 'undefined'
  enclose     => Enclose,   % 'optionally' (default), 'never' or 'always'
  end_of_line => EndOfLine} % `<<"\r\n">> (default), <<"\n">> or <<"\r">>
```
_Restrictions for option combinations:_
* If `Enclose` is `never` (ie, no enclosing), both `Enclosure` and `Quote` must be `undefined`.
* If `Enclose` is `optionally` or `always`, `Enclosure` and `Quote` must not be `undefined`.
* If `Enclosure` is `$"`, `Quote` can be `$"` or `$\\`.
* If `Enclosure` is `$'`, `Quote` can be `$'` or `$\\`.

The input for encoding is a list of CSV lines, which are in turn lists of CSV fields,
which are in turn binaries representing the field values.

The result is a CSV binary document.

#### Example

Assume the following CSV structure:
```erlang
1> Csv = [[<<"a">>,<<"b">>,<<"c">>],[<<"d,d">>,<<"e\"e">>,<<"f\r\nf">>]].
```

Encoded with `encode/1`, this will become:
```erlang
2> hnc_csv:encode(Csv).
<<"a,b,c\r\n\"d,d\",\"e\"\"e\",\"f\r\nf\"\r\n">>
```

# Authors

* Maria Scott (Maria-12648430)
* Jan Uhlig (juhlig)
