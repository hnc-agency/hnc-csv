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

### Incremental Decoding

CSV documents may be huge, or of unknown size, making decoding them
in entirety in one operation unfeasible or dangerous.

The functions `decode_init/0,1,2`, `decode_add_data/2`, `decode_next_line/1`
and `decode_flush/1` can be used together to decode CSV documents in
smaller chunks.

#### Example

The following module implements a function `fold_csv_file/4` to fold over
the CSV lines in a file, which is read in chunks:
```erlang
-module(example_fold).

-export([fold_csv_file/4]).

fold_csv_file(File, ChunkSize, Fun, Init) when is_integer(ChunkSize),
                                               ChunkSize > 0,
                                               is_function(Fun, 2) ->
    {ok, IoDevice} = file:open(File, [read, binary]),
    State = hnc_csv:decode_init(),
    fold_csv_file(hnc_csv:decode_next_line(State),
                  IoDevice, ChunkSize,
                  Fun, Init).

fold_csv_file({end_of_data, State}, IoDevice, ChunkSize, Fun, Acc) ->
    case file:read(IoDevice, ChunkSize) of
        eof ->
            ok = file:close(IoDevice),
            case hnc_csv:decode_flush(State) of
                {undefined, _} -> Acc;
                {Line, _} -> Fun(Line, Acc)
            end;
        {ok, Data} ->
            NewState = hnc_csv:decode_add_data(State, Data),
            fold_csv_file(hnc_csv:decode_next_line(NewState),
                          IoDevice, ChunkSize,
                          Fun, Acc)
    end;
fold_csv_file({Line, State}, IoDevice, ChunkSize, Fun, Acc) ->
    fold_csv_file(hnc_csv:decode_next_line(State),
                  IoDevice, ChunkSize,
                  Fun, Fun(Line, Acc)).
```
* The first argument is the path to the CSV file.
* The second argument is the number of bytes to read from the file
  whenever the decoder state is exhausted.
* The third argument is the folding function, which will receive the
  current line and an accumulator.
* The fourth argument is the initial accumulator.

Assuming the CSV data shown above in a file `"example.csv"`, this module
can then be used as follows, for example to count the number of CSV lines
in the file, reading it 2 bytes at a time:
```erlang
3> example_fold:fold_csv_file("example.csv", 2, fun(L, A) -> A+1 end, 0).
2
```

You may want to play around with the `ChunkSize` parameter to find a good
value.
* When set low, many iterations are needed to just fill the state up to a
  point when it can return a line.
  Using the value of `2` as in the example above, the state needs to be
  filled 3 times before it can return the first line, and another 11 times
  before it can return the second line.
* When set high, memory consumption will be higher as the state then carries
  larger amounts of data around.

## Encoding

Whole CSV documents can be encoded with `encode/1,2`.

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

### Incremental Encoding

A CSV structure may be huge, and encoding it in entirety will result in
a huge binary.

Incremental encoding can be achieved by subsequently providing a sublist
of the lines of the CSV structure.

#### Example

The `example_fold` module shown above can be used to turn one CSV file
(assuming the CSV data as shown above in a file `"example.csv"`) into
another with different options:
```erlang
3> TargetOpts = #{separator => $;, enclosure => $', quote => $\\, enclose => always, end_of_line => <<"\n">>}.
#{end_of_line => <<"\n">>,separator => 59,enclosure => 39,
  quote => 92,enclose => always}
4> {ok, Target} = file:open("example2.csv", [write]).
{ok,<0.99.0>}
5> example_fold:fold_csv_file("example.csv", 2, fun(L, ok) -> file:write(Target, hnc_csv:encode([L], TargetOpts)) end, ok).
ok
6> file:close(Target).
ok
7> file:read_file("example2.csv").
{ok,<<"'a';'b';'c'\n'd,d';'e\"e';'f\r\nf'\n">>}
```
