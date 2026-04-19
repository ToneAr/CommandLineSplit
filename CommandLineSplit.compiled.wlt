Get @ FileNameJoin[DirectoryName @ $TestFileName, "CommandLineSplit.compiled.wl"];

(* Basic splitting *)
VerificationTest[
	CommandLineSplit["foo bar baz"],
	{"foo", "bar", "baz"},
	TestID -> "BasicSpaceSplit"
]

VerificationTest[
	CommandLineSplit[""],
	{},
	TestID -> "EmptyString"
]

VerificationTest[
	CommandLineSplit["single"],
	{"single"},
	TestID -> "SingleToken"
]

(* Multiple consecutive delimiters are collapsed *)
VerificationTest[
	CommandLineSplit["foo  bar"],
	{"foo", "bar"},
	TestID -> "MultipleSpaces"
]

(* Leading and trailing delimiters *)
VerificationTest[
	CommandLineSplit[" foo bar "],
	{"foo", "bar"},
	TestID -> "LeadingTrailingSpaces"
]

(* Tab is also a default delimiter (POSIX IFS = space + tab) *)
VerificationTest[
	CommandLineSplit["foo\tbar"],
	{"foo", "bar"},
	TestID -> "TabDelimiter"
]

VerificationTest[
	CommandLineSplit["foo \t bar"],
	{"foo", "bar"},
	TestID -> "MixedSpaceTabDelimiters"
]

(* Equals sign is NOT a delimiter by default under POSIX *)
VerificationTest[
	CommandLineSplit["--opt=value"],
	{"--opt=value"},
	TestID -> "EqualsNotDefaultDelimiter"
]

(* Custom delimiter option *)
VerificationTest[
	CommandLineSplit["--opt=value", "TokenDelimiters" -> "="],
	{"--opt", "value"},
	TestID -> "EqualsDelimiter"
]

VerificationTest[
	CommandLineSplit["a,b,c", "TokenDelimiters" -> ","],
	{"a", "b", "c"},
	TestID -> "CustomDelimiter"
]

(* Single-quoted strings preserve spaces *)
VerificationTest[
	CommandLineSplit["foo 'bar baz'"],
	{"foo", "bar baz"},
	TestID -> "SingleQuotedWithSpace"
]

(* Double-quoted strings preserve spaces *)
VerificationTest[
	CommandLineSplit["foo \"bar baz\""],
	{"foo", "bar baz"},
	TestID -> "DoubleQuotedWithSpace"
]

(* Single quotes inside double quotes are literal *)
VerificationTest[
	CommandLineSplit["\"it's fine\""],
	{"it's fine"},
	TestID -> "SingleQuoteInsideDouble"
]

(* Double quotes inside single quotes are literal *)
VerificationTest[
	CommandLineSplit["'say \"hello\"'"],
	{"say \"hello\""},
	TestID -> "DoubleQuoteInsideSingle"
]

(* Adjacent quoted tokens merge into one token *)
VerificationTest[
	CommandLineSplit["'foo''bar'"],
	{"foobar"},
	TestID -> "AdjacentSingleQuotes"
]

VerificationTest[
	CommandLineSplit["\"foo\"\"bar\""],
	{"foobar"},
	TestID -> "AdjacentDoubleQuotes"
]

(* Escape character (backslash) suppresses delimiter outside quotes (POSIX §2.2.1) *)
VerificationTest[
	CommandLineSplit["foo\\ bar"],
	{"foo bar"},
	TestID -> "EscapedSpace"
]

(* Escaped backslash outside quotes produces a literal backslash *)
VerificationTest[
	CommandLineSplit["foo\\\\bar"],
	{"foo\\bar"},
	TestID -> "EscapedBackslash"
]

(* Backslash inside single quotes is literal — not special (POSIX §2.2.2) *)
VerificationTest[
	CommandLineSplit["'foo\\ bar'"],
	{"foo\\ bar"},
	TestID -> "EscapeInsideSingleQuote"
]

(* Backslash inside double-quotes is only special before $ ` " \ and newline (POSIX §2.2.3) *)

(* \\ inside double-quotes -> literal \ *)
VerificationTest[
	CommandLineSplit["\"a\\\\b\""],
	{"a\\b"},
	TestID -> "EscapedBackslashInDouble"
]

(* \" inside double-quotes -> literal " *)
VerificationTest[
	CommandLineSplit["\"say \\\"hi\\\"\""],
	{"say \"hi\""},
	TestID -> "EscapedDoubleQuoteInDouble"
]

(* \n (backslash + letter n) inside double-quotes: backslash is literal, both chars retained *)
VerificationTest[
	CommandLineSplit["\"a\\nb\""],
	{"a\\nb"},
	TestID -> "NonEscapeSeqInDouble"
]

(* \<newline> outside quotes = line continuation: both chars removed (POSIX §2.2.1) *)
VerificationTest[
	CommandLineSplit["foo\\\nbar"],
	{"foobar"},
	TestID -> "LineContinuationOutsideQuotes"
]

(* \<newline> inside double-quotes = line continuation (POSIX §2.2.3) *)
VerificationTest[
	CommandLineSplit["\"foo\\\nbar\""],
	{"foobar"},
	TestID -> "LineContinuationInsideDouble"
]

(* Mixed quoting and plain tokens — no = splitting by default *)
VerificationTest[
	CommandLineSplit["-v --output=\"some file.txt\" --flag"],
	{"-v", "--output=some file.txt", "--flag"},
	TestID -> "MixedRealWorldCommand"
]

(* Empty quoted string produces an empty-string token *)
VerificationTest[
	CommandLineSplit["foo '' bar"],
	{"foo", "", "bar"},
	TestID -> "EmptyQuotedString"
]

VerificationTest[
	CommandLineSplit["cmd '' \"\" a"],
	{"cmd", "", "", "a"},
	TestID -> "EmptyQuotedTokens"
]

(* Custom escape character option *)
VerificationTest[
	CommandLineSplit["foo^ bar", "EscapeCharacter" -> "^"],
	{"foo bar"},
	TestID -> "CustomEscapeCharacter"
]

(* Quoted string adjacent to unquoted chars merges *)
VerificationTest[
	CommandLineSplit["pre'mid'suf"],
	{"premidsuf"},
	TestID -> "QuotedAdjacentToUnquoted"
]

(* Unterminated escape sequence at end of input *)
VerificationTest[
	CommandLineSplit["foo\\"],
	_Failure,
	{CommandLineSplit::uesc},
	TestID -> "UnterminatedEscape",
	SameTest -> MatchQ
]

(* Unterminated escape inside double-quoted string *)
VerificationTest[
	CommandLineSplit["\"foo\\"],
	_Failure,
	{CommandLineSplit::uesc},
	TestID -> "UnterminatedEscapeInDouble",
	SameTest -> MatchQ
]

(* Unterminated single-quoted string *)
VerificationTest[
	CommandLineSplit["foo 'bar"],
	_Failure,
	{CommandLineSplit::usngq},
	TestID -> "UnterminatedSingleQuote",
	SameTest -> MatchQ
]

(* Unterminated double-quoted string *)
VerificationTest[
	CommandLineSplit["foo \"bar"],
	_Failure,
	{CommandLineSplit::udblq},
	TestID -> "UnterminatedDoubleQuote",
	SameTest -> MatchQ
]

(* Unterminated custom escape character *)
VerificationTest[
	CommandLineSplit["foo^", "EscapeCharacter" -> "^"],
	_Failure,
	{CommandLineSplit::uesc},
	TestID -> "UnterminatedCustomEscape",
	SameTest -> MatchQ
]
