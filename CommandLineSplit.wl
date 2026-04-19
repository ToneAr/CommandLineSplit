
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

CommandLineSplit::uesc  = "Escape character at end of input";
CommandLineSplit::usngq = "Unterminated single-quoted string";
CommandLineSplit::udblq = "Unterminated double-quoted string";

CommandLineSplit // Options = {
	"TokenDelimiters" -> {" ", "\t"},
	"EscapeCharacter" -> "\\"
};
CommandLineSplit[""] = {};
CommandLineSplit[s_String, OptionsPattern[]] := Catch[
	Module[{
			n, chars, delims, esc, posixSpecials, currentBag,
			tokensBag,
			i = 1,
			escSkip = False,
			inSingle = False,
			inDouble = False,
			inToken = False
		},
		If[StringFreeQ[s, {"\"", "'", OptionValue["EscapeCharacter"]}],
			Return @ StringSplit[s, Whitespace | OptionValue["TokenDelimiters"]]
		];
		currentBag = Internal`Bag[];
		tokensBag = Internal`Bag[];
		esc    = First @ ToCharacterCode @ OptionValue["EscapeCharacter"];
		delims = Flatten @ ToCharacterCode @ OptionValue["TokenDelimiters"];
		posixSpecials = {esc, 36, 96, 34};
		chars = ToCharacterCode[s];
		n = Length[chars];
		Do[
			If[escSkip,
				Which[
					(* Escaped new line *)
					esc == 92 && c == 10,
						Null,
					(* POSIX special characters *)
					inDouble && MemberQ[posixSpecials, c],
						Internal`StuffBag[currentBag, c];
						inToken = True,
					inDouble,
						Internal`StuffBag[currentBag, {esc, c}, 1];
						inToken = True,
					(* Non-special escape sequence *)
					True,
						Internal`StuffBag[currentBag, c];
						inToken = True
				];
				escSkip = False;
				Continue[];
			];
			Which[
				(* Single-quote mode
				 * (POSIX 2.2.2)
				 * - Escape character has NO special meaning inside
				 *   single-quotes
				 *)

				inSingle,
					If[c == 39,
						(* Toggle single-quote mode OFF *)
						inSingle = False;
						inToken = True,
						(* Add character to current token bag *)
						Internal`StuffBag[currentBag, c];
						inToken = True
					],

				(* Double-quote mode
				 * (POSIX 2.2.3)
				 * - Escape character is special ONLY before $ ` " itself and
				 *   newline.
				 * - \<newline> line continuation is always honoured when esc
				 *   is the POSIX backslash.
				 *)

				inDouble,
					Which[
						(* Escape character inside double quotes *)
						c == esc,
							If[i < n,
								escSkip = True,
								ThrowCommandLineSplitError[
									"uesc",
									<|"Position" -> i|>
								]
							],
						(* Toggle double-quote mode OFF *)
						c == 34,
							inDouble = False;
							inToken = True,
						(* Add character to current token bag *)
						True,
							Internal`StuffBag[currentBag, c];
							inToken = True
					],

				(* Normal (unquoted) mode
				 * (POSIX 2.2.1)
				 *)

				(* Non double-quoted string escape *)
				c == esc,
					If[i < n,
						escSkip = True,
						ThrowCommandLineSplitError[
							"uesc",
							<|"Position" -> i|>
						]
					],
				(* Toggle single-quote mode ON *)
				c == 39,
					inSingle = True;
					inToken = True,
				(* Toggle double-quote mode ON *)
				c == 34,
					inDouble = True;
					inToken = True,
				(* Flush current token at delimiter *)
				MemberQ[delims, c],
					If[inToken,
						Internal`StuffBag[
							tokensBag,
							FromCharacterCode @ Internal`BagPart[
								currentBag,
								All
							]
						];
						currentBag = Internal`Bag[];
						inToken = False
					],
				(* Add character to current token bag *)
				True,
					Internal`StuffBag[currentBag, c];
					inToken = True
			];
			i++
			,
			{c, chars}
		];
		Which[
			inSingle,
				ThrowCommandLineSplitError["usngq",
					<|"Position" -> n|>
				],
			inDouble,
				ThrowCommandLineSplitError["udblq",
					<|"Position" -> n|>
				],
			inToken,
				Internal`StuffBag[
					tokensBag,
					FromCharacterCode @ Internal`BagPart[currentBag, All]
				]
		];
		Internal`BagPart[
			tokensBag,
			All
		]
	],
	"CommandLineSplitError"
];
