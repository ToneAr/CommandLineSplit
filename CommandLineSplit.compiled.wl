(*
 * Return value - a flat integer array of length 2n+3 (n = StringLength[s]):
 *   out[[1]]           status           (0=ok, 1=uesc, 2=usngq, 3=udblq)
 *   out[[2]]           numTokens        (valid when status=0)
 *   out[[3;;2+K]]      token lengths    (K = numTokens)
 *   out[[n+3;;n+2+M]]  flat char codes  (M = totalChars)
 *   out[[2n+3]]        totalChars
 *)
Once[
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
	],
	TargetSystem -> {
		"Linux-x86-64", "MacOSX-x86-64", "Windows-x86-64",
		"Linux-ARM64", "MacOSX-ARM64"
	}
],
"Local"
]
