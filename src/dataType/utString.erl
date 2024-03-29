-module(utString).

-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([
	toLowerStr/1
	, toUpperStr/1
	, isAlphaNum/1
	, isNum/1
	, isAlpha/1
]).

toLowerStr(ListStr) when is_list(ListStr) ->
	[
		begin
			case C >= $A andalso C =< $Z of
				true ->
					C + 32;
				_ ->
					C
			end
		end || C <- ListStr
	];
toLowerStr(BinStr) when is_binary(BinStr) ->
	<<
		begin
			case C >= $A andalso C =< $Z of
				true ->
					<<(C + 32)>>;
				_ ->
					<<C>>
			end
		end || <<C:8>> <= BinStr
	>>.

toUpperStr(ListStr) when is_list(ListStr) ->
	[
		begin
			case C >= $a andalso C =< $z of
				true ->
					C - 32;
				_ ->
					C
			end
		end || C <- ListStr
	];
toUpperStr(BinStr) when is_binary(BinStr) ->
	<<
		begin
			case C >= $a andalso C =< $z of
				true ->
					<<(C - 32)>>;
				_ ->
					<<C>>
			end
		end || <<C:8>> <= BinStr
	>>.

-spec isAlpha(Char :: char()) -> boolean().
isAlpha(Char) ->
	$a =< Char andalso Char =< $z orelse $A =< Char andalso Char =< $Z.

-spec isNum(Char :: char()) -> boolean().
isNum(Char) ->
	$0 =< Char andalso Char =< $9.


-spec isAlphaNum(Char :: char()) -> boolean().
isAlphaNum(Char) ->
	isAlpha(Char) orelse isNum(Char).

%% 字符加密
checkEncrypt(Id, Time, TK) ->
	TICKET = "7YnELt8MmA4jVED7",
	Hex = utMd5:getMd5HexBin(lists:concat([Time, Id, TICKET])),
	NowTime = utTime:now(),
	Hex =:= TK andalso NowTime - Time >= -10 andalso NowTime - Time < 300.

%% Function: 检查客户端发过来的内容,false为不合法,true为合法
%% @param: String: 客户端发来的字符串 格式为binary
%% @param: Length: 服务端限制的字符串长度
checkValid(BinStr, Length) ->
	case checkLen(BinStr, Length) of
		true ->
			not checkKeyword(BinStr, ["'", "/", "\"", "_", "<", ">"]);
		_ ->
			false
	end.

%% 检查关键字，存在非法字符返回true，否则false
%% @spec check_keyword(Text, Words) -> false | true
%% @param Text : 需要检查的字符串（或字符串的二进制形式）
%% @param Words: 非法字符列表
checkKeyword(_, []) ->
	false;
checkKeyword(Text, [Word | Words]) ->
	case re:run(Text, Word, [{capture, none}]) of
		nomatch ->
			checkKeyword(Text, Words);
		_ ->
			true
	end.

%% 长度合法性检查
checkLen(BinStr, LenLimit) ->
	checkLen(BinStr, 1, LenLimit).

checkLen(BinStr, MinLen, MaxLen) ->
	% case asn1rt:utf8_binary_to_list(list_to_binary(Item)) of
	%     {ok, UnicodeList} ->
	%         Len = string_width(UnicodeList),
	%         Len =< MaxLen andalso Len >= MinLen;
	%     {error, _Reason} ->
	%         false
	% end.
	% case unicode:characters_to_list(list_to_binary(BinStr)) of
	case unicode:characters_to_list(BinStr) of
		UnicodeList when is_list(UnicodeList) ->
			Len = string_width(UnicodeList),
			Len =< MaxLen andalso Len >= MinLen;
		_ ->
			false
	end.

%% 字符宽度，1汉字=2单位长度，1数字字母=1单位长度
string_width(String) ->
	string_width(String, 0).
string_width([], Len) ->
	Len;
string_width([H | T], Len) ->
	case H > 255 of
		true ->
			string_width(T, Len + 2);
		false ->
			string_width(T, Len + 1)
	end.

%% 过滤掉字符串中的特殊字符
filter_string(String, CharList) ->
	case is_list(String) of
		true ->
			filter_string_helper(String, CharList, []);
		false when is_binary(String) ->
			ResultString = filter_string_helper(binary_to_list(String), CharList, []),
			list_to_binary(ResultString);
		false ->
			String
	end.

filter_string_helper([], _CharList, ResultString) ->
	ResultString;
filter_string_helper([H | T], CharList, ResultString) ->
	case lists:member(H, CharList) of
		true -> filter_string_helper(T, CharList, ResultString);
		false -> filter_string_helper(T, CharList, ResultString ++ [H])
	end.

%% 列表转utf8编码的列表
listToUtfString(List) ->
	unicode:characters_to_list(erlang:list_to_binary(List), utf8).

%%汉字unicode编码范围 0x4e00 - 0x9fa5
% (4 * 16 * 16 * 16 + 14 * 16 * 16)
-define(UNICODE_CHINESE_BEGIN, 16#4e00).
% (9 * 16 * 16 * 16 + 15 * 16 * 16 + 10 * 16 + 5)
-define(UNICODE_CHINESE_END, 16#9fa5).

%  中文字符范围
%  双字节字符编码范围：
%
%  1. GBK (GB2312/GB18030)
%
%  \x00-\xff GBK双字节编码范围
%  \x20-\x7f ASCII
%  \xa1-\xff 中文gb2312
%  \x80-\xff 中文 gbk
%
%  2. UTF-8 (Unicode)
%
%  \u4e00-\u9fa5 (中文)
%  \x3130-\x318F (韩文)
%  \xAC00-\xD7A3 (韩文)
%  \u0800-\u4e00 (日文)

cn(Str) ->
	re:run(Str, "[\x{4e00}-\x{9fa5}]+", [unicode]).

jp(Str) ->
	re:run(Str, "[\x{0800}-\x{4e00}]+", [unicode]).

sk1(Str) ->
	re:run(Str, "[\x{3130}-\x{318F}]+", [unicode]).

sk2(Str) ->
	re:run(Str, "[\x{AC00}-\x{D7A3}]+", [unicode]).

%% desc   获取字符串汉字和非汉字的个数
%% parm   UTF8String  			UTF8编码的字符串
%% return {汉字个数,非汉字个数}
getChineseCnt(UTF8String) ->
	UnicodeList = unicode:characters_to_list(list_to_binary(UTF8String)),
	Fun = fun(Num, Sum) ->
		case Num >= ?UNICODE_CHINESE_BEGIN andalso Num =< ?UNICODE_CHINESE_END of
			true ->
				Sum + 1;
			false ->
				Sum
		end
			end,
	ChineseCount = lists:foldl(Fun, 0, UnicodeList),
	OtherCount = length(UnicodeList) - ChineseCount,
	{ChineseCount, OtherCount}.
