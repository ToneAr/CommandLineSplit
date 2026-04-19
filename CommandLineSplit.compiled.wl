CommandLineSplit // Options = {
	"TokenDelimiters" -> {" ", "\t"},
	"EscapeCharacter" -> "\\"
};


CommandLineSplit::uesc  = "Escape character at end of input";
CommandLineSplit::usngq = "Unterminated single-quoted string";
CommandLineSplit::udblq = "Unterminated double-quoted string";
CommandLineSplit::noplatform = "Unsupported platform: `1`";

ThrowCommandLineSplitError[tag_String, meta_Association: <||> ] := (
	Message[MessageName[CommandLineSplit, tag]];
	Throw[
		Failure["ConfirmationFailed", <|
			"MessageTemplate" -> MessageName[CommandLineSplit, tag],
			meta
		|>],
		"CommandLineSplitError"
	]
);

(*
 * Return value - a flat integer array of length 2n+3 (n = StringLength[s]):
 *   out[[1]]           status           (0=ok, 1=uesc, 2=usngq, 3=udblq)
 *   out[[2]]           numTokens        (valid when status=0)
 *   out[[3;;2+K]]      token lengths    (K = numTokens)
 *   out[[n+3;;n+2+M]]  flat char codes  (M = totalChars)
 *   out[[2n+3]]        totalChars
 *)
scan = Once[
	FunctionCompile[
		Function[
			{
				Typed[s, "String"],
				Typed[delimStr, "String"],
				Typed[escStr, "String"]
			},
			Module[{
				sCodes, dCodes, out, j, c, esc,
				n, nd, i = 1,
				inSingle = False, inDouble = False, inToken = False, escSkip = False,
				numTokens = 0, totalChars = 0, tokenLen = 0,
				lenIdx, charIdx,
				isDelim
			},
				sCodes = ToCharacterCode[s];
				dCodes = ToCharacterCode[delimStr];
				n = Length[sCodes];
				nd = Length[dCodes];
				esc = ToCharacterCode[escStr][[1]];
				out = Table[0, {2*n + 3}];
				lenIdx  = 3;      (* token lengths stored at indices 3 .. 2+numTokens *)
				charIdx = n + 3;  (* char codes stored at indices n+3 .. n+2+totalChars *)

				While[i <= n,
					c = sCodes[[i]];

					If[escSkip,
						Which[
							(* \<newline> line continuation *)
							esc == 92 && c == 10,
								Null,
							(* Inside double-quotes: escape is special only before $ ` " \ *)
							inDouble && (c == esc || c == 36 || c == 96 || c == 34),
								out[[charIdx]] = c;
								charIdx += 1;
								tokenLen += 1;
								totalChars += 1;
								inToken = True,
							inDouble,
								(* non-special sequence inside double-quotes: keep \ + char *)
								out[[charIdx]] = esc;
								charIdx += 1;
								out[[charIdx]] = c;
								charIdx += 1;
								tokenLen += 2;
								totalChars += 2;
								inToken = True,
							True,
								(* outside quotes: escaped character is literal *)
								out[[charIdx]] = c;
								charIdx += 1;
								tokenLen += 1;
								totalChars += 1;
								inToken = True
						];
						escSkip = False;
						i = i + 1;
						Continue[]
					];

					Which[
						(* Single-quote mode (POSIX 2.2.2) *)
						inSingle,
							If[c == 39,
								(* closing single-quote *)
								inSingle = False;
								inToken = True,
								out[[charIdx]] = c;
								charIdx += 1;
								tokenLen += 1;
								totalChars += 1;
								inToken = True
							],

						(* Double-quote mode (POSIX 2.2.3) *)
						inDouble,
							Which[
								c == esc,
									If[i < n,
										escSkip = True,
										(* escape at end of input inside double-quotes *)
										out[[1]] = 1;
										i = n + 1  (* break out of While *)
									],
								c == 34,
									(* closing double-quote *)
									inDouble = False;
									inToken = True,
								True,
									out[[charIdx]] = c;
									charIdx += 1;
									tokenLen += 1;
									totalChars += 1;
									inToken = True
							],

						(* Normal (unquoted) mode (POSIX 2.2.1) *)
						c == esc,
							If[i < n,
								escSkip = True,
								out[[1]] = 1;
								i = n + 1
							],
						c == 39,
							inSingle = True;
							inToken = True,
						c == 34,
							inDouble = True;
							inToken = True,
						True,
							(* check delimiter *)
							isDelim = False;
							j = 1;
							While[j <= nd,
								If[dCodes[[j]] == c,
									isDelim = True;
									j = nd + 1
									,
									j = j + 1
								]
							];
							If[isDelim,
								If[inToken,
									out[[lenIdx]] = tokenLen;
									lenIdx += 1;
									numTokens += 1;
									tokenLen = 0;
									inToken = False
								],
								out[[charIdx]] = c;
								charIdx += 1;
								tokenLen += 1;
								totalChars += 1;
								inToken = True
							]
					];
					i = i + 1
				];  (* end While *)

				(* Post-loop: handle unterminated constructs or flush last token *)
				If[out[[1]] == 0,
					Which[
						inSingle, out[[1]] = 2,  (* usngq *)
						inDouble, out[[1]] = 3,  (* udblq *)
						inToken,
							out[[lenIdx]] = tokenLen;
							numTokens = numTokens + 1
					]
				];
				out[[2]]       = numTokens;
				out[[2*n + 3]] = totalChars;
				out
			]
		]
	],
	PersistenceLocation["Local"]
];

(* Decode the raw integer array returned by CompiledScan into a list of strings,
 * or call ThrowCommandLineSplitError on error codes. *)
decode[raw_, n_Integer, pos_Integer] :=
	Module[{status, numTokens, totalChars, lens, flatChars, offset, tokens},
		status = raw[[1]];
		Switch[status,
			1, ThrowCommandLineSplitError["uesc",  <|"Position" -> pos|>],
			2, ThrowCommandLineSplitError["usngq", <|"Position" -> pos|>],
			3, ThrowCommandLineSplitError["udblq", <|"Position" -> pos|>]
		];
		numTokens  = raw[[2]];
		If[numTokens == 0, Return[{}]];
		totalChars = raw[[2*n + 3]];
		lens       = raw[[3 ;; 2 + numTokens]];
		flatChars  = raw[[n + 3 ;; n + 2 + totalChars]];
		offset = 1;
		tokens = Table[
			With[{l = lens[[k]]},
				offset += l;
				If[l == 0,
					"",
					FromCharacterCode[flatChars[[offset - l ;; offset - 1]]]
				]
			],
			{k, numTokens}
		];
		tokens
	];
CommandLineSplit[""] = {};
CommandLineSplit[s_String, opts : OptionsPattern[]] := Catch[
	Module[{delimStr, escStr, raw},
		(* Fast path: no quoting or escaping needed *)
		If[StringFreeQ[s, {"\"", "'", OptionValue["EscapeCharacter"]}],
			Return @ StringSplit[s, Whitespace | OptionValue["TokenDelimiters"]]
		];
		delimStr = StringJoin @ Flatten @ {OptionValue["TokenDelimiters"]};
		escStr   = OptionValue["EscapeCharacter"];
		raw      = scan[s, delimStr, escStr];
		decode[raw, StringLength[s], StringLength[s]]
	],
	"CommandLineSplitError"
];
