-module(tempconverter).
-export([convert/0]).

convert() ->
	TempConvert = fun({c, C}) -> {f, 32 + C*9/5};
									 ({f, F}) -> {c, (F - 32)*5/9}
								end,
	TempConvert({c, 100}),
	TempConvert({f, 200}).
