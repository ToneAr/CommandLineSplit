posixCommandSeparators = {"||", "&&", "|", "&", ";", "\n", "\r"};

posixCommandSeparatorsP = Alternatives @@ posixCommandSeparators;

multiCommandQ // Options = {"EscapeCharacter" -> "\\"};
multiCommandQ[s_String, OptionsPattern[]] :=
	With[{esc = OptionValue["EscapeCharacter"]},
		StringContainsQ[s, Except[esc] ~~ posixCommandSeparators]
	];

ThrowCommandLineSplitError[tag_String, meta_Association : <||>] :=
	(
		Message[MessageName[CommandLineSplit, tag]];
		Throw[
			Failure[
				"ConfirmationFailed",
				<|
					"MessageTemplate" -> MessageName[CommandLineSplit, tag],
					meta
				|>
			],
			"CommandLineSplitError"
		]
	);

decode[raw_, n_Integer, pos_Integer] :=
	Module[{
		status,
		numTokens,
		totalChars,
		lens,
		flatChars,
		offset,
		tokens
	},
		status = raw[[1]];
		Switch[status,
			1,
				ThrowCommandLineSplitError["uesc", <|"Position" -> pos|>],
			2,
				ThrowCommandLineSplitError["usngq", <|"Position" -> pos|>],
			3,
				ThrowCommandLineSplitError["udblq", <|"Position" -> pos|>]
		];
		numTokens = raw[[2]];
		If[numTokens == 0, Return[{}]];
		totalChars = raw[[2 * n + 3]];
		lens = raw[[3;;2 + numTokens]];
		flatChars = raw[[n + 3;;n + 2 + totalChars]];
		offset = 1;
		tokens =
			Table[
				With[{l = lens[[k]]},
					offset += l;
					If[l == 0,
						"",
						FromCharacterCode[flatChars[[offset - l;;offset - 1]]]
					]
				],
				{k, numTokens}
			];
		tokens
	];

CommandLineSplit::"uesc" = "Escape character at end of input";
CommandLineSplit::"usngq" = "Unterminated single-quoted string";
CommandLineSplit::"udblq" = "Unterminated double-quoted string";
CommandLineSplit::"noplatform" = "Unsupported platform: `1`";
CommandLineSplit // Options =
	{"TokenDelimiters" -> {" ", "\t"}, "EscapeCharacter" -> "\\"};
CommandLineSplit[""] = {};
CommandLineSplit[sep : posixCommandSeparatorsP, OptionsPattern[]] := sep;
CommandLineSplit[s_String, opts : OptionsPattern[]] /;
	multiCommandQ[s, FilterRules[{opts}, Options[multiCommandQ]]] :=            (* wl-disable-line DocCommentArityMismatch*)
	With[{esc = OptionValue["EscapeCharacter"]},
		CommandLineSplit[#, opts]& /@ StringSplit[
			s,
			(
				a : ___ ~~ b : Except[esc] ~~ c : posixCommandSeparatorsP
			) :> Sequence[a <> b, c]
		]
	];
CommandLineSplit[s_String, OptionsPattern[]] :=
	Catch[
		Module[
			{delimStr, escStr, raw},
			(* Fast path: no quoting or escaping needed *)
			If[StringFreeQ[s, {"\"", "'", OptionValue["EscapeCharacter"]}],
				Return @
				StringSplit[s, Whitespace | OptionValue["TokenDelimiters"]]
			];
			delimStr = StringJoin @ Flatten @ {OptionValue["TokenDelimiters"]};
			escStr = OptionValue["EscapeCharacter"];
			(* wl-disable-next-line UndefinedSymbol *)
			raw = scan[s, delimStr, escStr];
			decode[raw, StringLength[s], StringLength[s]]
		],
		"CommandLineSplitError"
	];
