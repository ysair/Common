unit QStrings;

interface

type
  TCharSet = set of Char;

{ Characters by default used as separators of words.}

const
  Q_StdDelimsSet = [#0..#32,'!','"','(',')','*','+',',','-','.','/',':', 
     ';','<','=','>','?','[','\',']','^','{','}','|']; 

{ Function for matching strings.}

{ Q_CompStr compares two strings to case sensitivity. Reset result
  It is less than zero, if S1 < S2; it is more than zero, if S1 > S2, and is equal to zero, if
  S1 = S2. This function ceases matching, when the discrepancy of strings is revealed
  Or when the character with the code 0 is detected. If this character can meet in
  To middle of string, use instead of Q_CompStr the function Q_CompStrL. Second
  The function Q_PCompStr is similar Q_CompStr for PChar and Pointer (String). If
  It is necessary to clarify only, two strings are equal or are not equal, take advantage
  Instead of Q_CompStr by the function Q_SameStr.}

function Q_CompStr (const S1, S2: string): Integer;
function Q_PCompStr (P1, P2: PChar): Integer;

{ Q_CompStrL compares two strings on MaxL to the first characters to case sensitivity.
  The reset result is less than zero, if Copy (S1,1, MaxL) < Copy (S2,1, MaxL);
  The result is more than zero, if Copy (S1,1, MaxL) > Copy (S2,1, MaxL), differently
  The result is equal to zero (if Copy (S1,1, MaxL) = Copy (S2,1, MaxL)). If to you
  It is necessary to clarify only, two strings are equal or are not equal, take advantage
  Instead of Q_CompStrL by the function Q_SameStrL.}

function Q_CompStrL (const S1, S2: string; MaxL: Cardinal = MaxInt): Integer;

{ Q_CompText compares two strings without case sensitivity. Reset result
  It is less than zero, if S1 < S2; it is more than zero, if S1 > S2, and is equal to zero, if
  S1 = S2. This function ceases matching, when the discrepancy of strings is revealed
  Or when the character with the code 0 is detected. If this character can meet in
  To middle of string, use instead of Q_CompText the function Q_CompTextL. Second
  The function Q_PCompText is similar Q_CompText for PChar and Pointer (String). If
  It is necessary to clarify only, whether two strings whether or not are equal, take advantage
  Instead of Q_CompText by the function Q_SameText.}

function Q_CompText (const S1, S2: string): Integer;
function Q_PCompText (P1, P2: PChar): Integer;

{ Q_CompTextL compares two strings on MaxL to the first characters without case sensitivity.
  If the fragment of the first string is more (in alphabetic order), than fragment
  The second string the reset value is more than zero. If a fragment of the first string
  It is less, than the fragment of the second string reset value is less than zero, differently
  The result is equal to zero. If it is necessary to clarify only, whether two strings are equal
  Or are not equal, use instead of Q_CompTextL the function Q_SameTextL.}

function Q_CompTextL (const S1, S2: string; MaxL: Cardinal = MaxInt): Integer;

{ Q_SameStr compares two strings to case sensitivity and resets True, if
  The strings are equal, differently resets False. The function Q_PSameStr is similar Q_SameStr
  For Pointer (String).}

function Q_SameStr (const S1, S2: string): Boolean;
function Q_PSameStr (P1, P2: Pointer): Boolean;

{ Q_SameStrL compares two strings on MaxL to the first characters to case sensitivity.
  Resets True, if the strings are equal, differently resets False.}

function Q_SameStrL (const S1, S2: string; MaxL: Cardinal): Boolean;

{ Q_SameText compares two strings without case sensitivity and resets True, if
  The strings are equal, differently - False. Function Q_PSameText is similar Q_SameStr for
  Pointer (String).}

function Q_SameText (const S1, S2: string): Boolean;
function Q_PSameText (P1, P2: Pointer): Boolean;

{ Q_SameTextL compares two strings on MaxL to the first characters without case sensitivity.
  Resets True, if the strings are equal, differently resets False.}

function Q_SameTextL (const S1, S2: string; MaxL: Cardinal): Boolean;

{ Q_MatchStr checks, whether has a place log-on of a substring SubStr in string S,
  Since the character S [Pos]. At matching the register of characters is taken into account. If
  The log-on takes place, the function resets True, differently - False. This function
  Realizes by itself fast variant of check Q_SameStr (X, Copy (S, Pos, Length (X))).
  As against other similar functions, substring of zero length enters in
  Any string. The idea of this function is borrowed from the unit cStrings.pas, writer
  Which is David Butler (david@e.co.za). }

function Q_MatchStr (const SubStr, S: string; Pos: Integer = 1): Boolean;

{ Q_MatchText checks, whether has a place log-on of a substring SubStr in string S,
  Since the character S [Pos]. At matching the register of characters is not taken into account. If
  The log-on takes place, the function resets True, differently - False. This function
  Realizes by itself fast variant of check Q_SameText (X, Copy (S, Pos, Length (X))).
  As against other similar functions, substring of zero length enters in
  Any string. The idea of this function is borrowed from the unit cStrings.pas, writer
  Which is David Butler (david@e.co.za). }

function Q_MatchText (const SubStr, S: string; Pos: Integer = 1): Boolean;

{ Q_TestByMask checks, whether the string S to a mask Mask satisfies, reputing,
  That the characters MaskChar from string Mask can be substituted in string S by anyone
  By other characters. At matching the register of characters is considered.
  If the string S satisfies to a mask, the function resets True, differently False.
  For example, Q_TestMask('ISBN 5-09-007017-2','ISBN ?-??-??????-?','?') will return
  Value True. }

function Q_TestByMask (const S, Mask: string; MaskChar: Char = 'X'): Boolean;

{ Q_TestWildStr checks, whether the string S to a mask Mask satisfies, reputing,
  That the characters MaskChar from string Mask can be substituted in string S by anyone
  By other characters, and the characters WildCard can be substituted by any quantity
  Other characters. At matching the large and small characters differ. The character
  WildCard should be distinct from #0. If the string S satisfies to a mask,
  The function resets True, differently False. For example, following function call
  Will return True: Q_TestWildStr('abc12345_infQ_XL.dat','abc*_???Q_*.d*at'). }

function Q_TestWildStr (const S, Mask: string; MaskChar: Char = '?';
  WildCard: Char = '*'): Boolean;

{ Q_TestWildText is similar to the function Q_TestWildStr, but register of characters
  Is not considered (large and small characters do not differ).}

function Q_TestWildText (const S, Mask: string; MaskChar: Char = '?';
  WildCard: Char = '*'): Boolean;


{ Function for change of the register of characters.}

{ Q_CharUpper translates the character Ch in the upper case (in the large character).}

function Q_CharUpper (Ch: Char): Char;

{ Q_CharLower translates the character Ch in lowercase (in the small character).}

function Q_CharLower (Ch: Char): Char;

{ Q_StrUpper translates string S in the upper case (in the large characters). At
  It the initial string varies. The function Q_PStrUpper is similar to the procedure
  Q_StrUpper For PChar and Pointer (String), except that she(it)
  In addition resets the pointer by the beginning of string.}

procedure Q_StrUpper (var S: string);
function Q_PStrUpper (P: PChar): PChar;

{ Q_StrLower translates string S in lowercase (in small characters). At
  It the initial string varies. The function Q_PStrLower is similar to the procedure
  Q_StrLower For PChar and Pointer (String), except that she(it)
  In addition resets the pointer by the beginning of string.}

procedure Q_StrLower (var S: string);
function Q_PStrLower (P: PChar): PChar;

{ Q_StrUpperMoveL copies contents of string Source in string Dest. Thus
  The characters are translated in the upper case. Maximum number copied
  Characters is equal MaxL. Length of string Dest is installed equal to number
  The copied characters. The memory for string Dest should be distributed
  Beforehand by function call SetString (or SetLength) (size
  Not less MaxL of characters).}

procedure Q_StrUpperMoveL (const Source: string; var Dest: string; MaxL: Cardinal);

{ Q_StrLowerMoveL copies contents of string Source in string Dest. Thus
  The characters are translated in lowercase. Maximum number copied
  Characters is equal MaxL. Length of string Dest is installed equal to number
  The copied characters. The memory for string Dest should be distributed
  Previously by function call SetString (or SetLength) (size
  Not less MaxL of characters).}

procedure Q_StrLowerMoveL (const Source: string; var Dest: string; MaxL: Cardinal);

{ Q_UpperCase translates string S in the upper case (in the large characters). Initial
  The string thus does not vary. This function works more slowly, than Q_StrUpper
  Or Q_StrUpperMoveL.}

function Q_UpperCase (const S: string): string;

{ Q_LowerCase translates string S in lowercase (in small characters). Initial
  The string thus does not vary. This function works more slowly, than Q_StrLower
  Or Q_StrLowerMoveL.}

function Q_LowerCase (const S: string): string;

{ Q_UpLowerInPlace will convert the first character of string to the upper case, and all
  Other characters - to lowercase. The initial string varies.}

procedure Q_UpLowerInPlace (var S: string);

{ Q_UpLowerStr will convert the first character of string to the upper case, and all
  Other characters - to lowercase. The initial string does not vary.
  For example: 'rЫРaблКА bsDSFc' - > 'Rырбaлка bsdsfc'.}

function Q_UpLowerStr (const S: string): string;

{ Q_ProperCaseInPlace changes string S in such a manner that each word will be
  To start with large, and to keep by small characters (first character everyone
  Words will be converted to the upper case, and other characters - to lower
  To the register). The initial string S varies. In string Delimiters are transmitted
  The characters, which are considered as separators of words in string S. If
  Delimiters is equal to empty string, the list of separators, which is used
  Were preset during the previous call of one from the following functions Q_StrTok,
  Q_StrSpn, Q_StrCSpn, Q_ProperCase, Q_ProperCaseInPlace, Q_WordAtPos,
  Q_GetWordN, Q_SetDelimiters, Q_CountOfWords. If the separators are transmitted
  As set, they are not stored for the subsequent calls of functions.}

procedure Q_ProperCaseInPlace (var S: string; const Delimiters: string); overload;
procedure Q_ProperCaseInPlace (var S: string;
  const Delimiters: TCharSet = Q_StdDelimsSet); overload;

{ Q_ProperCase resets string S, transformed in such a manner that everyone
  Word starts with large character (first character of each word is translated in
  The upper case, and other characters - in lowercase). In string Delimiters
  The characters are transmitted which are considered as separators of words in string S.
  For example, the function call Q_ProperCase (' resources of the INTERNET ', ') will return string
  ' Resources of the Internet '. If Delimiters is equal to empty string, is used
  List of separators, which were preset during previous call of one of
  The following functions: Q_ProperCaseInPlace, Q_ProperCase, Q_StrTok, Q_StrSpn,
  Q_StrCSpn, Q_WordAtPos, Q_GetWordN, Q_SetDelimiters, Q_CountOfWords. If
  The separators are transmitted as set, they are not stored for subsequent
  Calls of functions.}

function Q_ProperCase (const S, Delimiters: string): string; overload;
function Q_ProperCase (const S: string;
  const Delimiters: TCharSet = Q_StdDelimsSet): string; overload;


{ Function of code conversion of strings: from DOS in Windows and on the contrary.}

{ Q_StrToAnsi translates string S from the coding DOS in the coding Windows. At
  It the initial string varies. Q_PStrToAnsi is similar to the procedure
  Q_StrToAnsi For PChar and Pointer (String), except that she(it)
  In addition resets the pointer by the beginning of string. }

procedure Q_StrToAnsi (var S: string);
function Q_PStrToAnsi (P: PChar): PChar;

{ Q_StrToOem translates string S from the coding Windows in the coding DOS. At
  It the initial string varies. The function Q_PStrToOem is similar to the procedure
  Q_StrToOem For PChar and Pointer (String), except that she(it)
  In addition resets the pointer by the beginning of string. }

procedure Q_StrToOem (var S: string);
function Q_PStrToOem (P: PChar): PChar;

{ Q_PStrToAnsiL translates L of the first characters of string pointed by the parameter P,
  From the coding DOS in the coding Windows. Thus the initial string varies.
  The function resets the pointer by the beginning of string. }

function Q_PStrToAnsiL (P: PChar; L: Cardinal): PChar;

{ Q_PStrToOemL translates L of the first characters of string pointed by the parameter P,
  From the coding Windows in the coding DOS. Thus the initial string varies.
  The function resets the pointer by the beginning of string. }

function Q_PStrToOemL (P: PChar; L: Cardinal): PChar;

{ Q_Str2ToAnsi translates string Source from the coding DOS in the coding Windows.
  The result is saved in string Dest. Q_PStr2ToAnsi is similar to the procedure
  Q_Str2ToAnsi For PChar and Pointer (String), for exception that she(it)
  In addition resets the pointer by the beginning of the string - receiver Dest.}

procedure Q_Str2ToAnsi (const Source: string; var Dest: string);
function Q_PStr2ToAnsi (Source, Dest: PChar): PChar;

{ Q_Str2ToOem translates string Source from the coding Windows in the coding DOS.
  The result is saved in string Dest. Q_PStr2ToOem is similar to the procedure
  Q_Str2ToOem For PChar and Pointer (String), for exception that she(it)
  In addition resets the pointer by the beginning of the string - receiver Dest.}

procedure Q_Str2ToOem (const Source: string; var Dest: string);
function Q_PStr2ToOem (Source, Dest: PChar): PChar;

{ Q_PStr2ToAnsiL translates L of the first characters of string Source from the coding
  DOS in the coding Windows. The result is saved in string Dest. The function
  Resets the pointer by the beginning of the string - receiver Dest.}

function Q_PStr2ToAnsiL (Source, Dest: PChar; L: Cardinal): PChar;

{ Q_PStr2ToOemL translates L of the first characters of string Source from the coding Windows
  In the coding DOS. The result is saved in string Dest. The function resets
  The pointer by the beginning of the string - receiver Dest.}

function Q_PStr2ToOemL (Source, Dest: PChar; L: Cardinal): PChar;

{ Q_ToAnsi translates string OemStr from the coding DOS in the coding Windows. At
  It the initial string does not vary. This function works more slowly, than
  Other similar functions and procedures from this unit.}

function Q_ToAnsi (const OemStr: string): string;

{ Q_ToOem translates string AnsiStr from the coding Windows in the coding DOS. At
  It the initial string does not vary. This function works more slowly, than
  Other similar functions and procedures from this unit.}

function Q_ToOem (const AnsiStr: string): string;


{ Search, replacement and deleting of substrings and separate characters.}

{ Q_PosStr finds first входение of a substring FindString in string SourceString,
  Since a position StartPos. Number of the character is reset, with which starts
  Log-on or 0, if the substring FindString is not retrieved in string SourceString.
  The search of a substring is made with case sensitivity (large and small characters
  Differ). The writer of algorithm - Peter Morris (UK) (unit FastStrings).}

function Q_PosStr (const FindString, SourceString: string;
  StartPos: Integer = 1): Integer;

{ Q_PosText finds first входение of a substring FindString in string SourceString,
  Since a position StartPos. Number of the character is reset, with which starts
  Log-on or 0, if the substring FindString is not retrieved in string SourceString.
  The search of a substring is made without case sensitivity (large and small characters
  Do not differ). The writer of algorithm - Peter Morris (UK) (unit FastStrings).}

function Q_PosText (const FindString, SourceString: string;
  StartPos: Integer = 1): Integer;

{ Q_PosLastStr finds the latter входение of a substring FindString in string
  SourceString, believing, that the following log-on was retrieved in a position
  LastPos. Number of the character is reset, with which the required substring starts
  Or 0, if the given substring is not retrieved earlier (more to the left) of indicated position.
  It is supposed, that the separate log-ons of a substring do not overlap each other.
  The search of a substring is made with case sensitivity (large and small characters
  Differ). If LastPos exceeds length of string SourceString, is searched
  The latest log-on of a substring FindString.}

function Q_PosLastStr (const FindString, SourceString: string;
  LastPos: Integer = MaxInt): Integer;

{ Q_PosLastText finds the latter входение of a substring FindString in string
  SourceString, believing, that the following log-on was retrieved in a position
  LastPos. Number of the character is reset, with which the required substring starts
  Or 0, if the given substring is not retrieved earlier (more to the left) of indicated position.
  It is supposed, that the separate log-ons of a substring do not overlap each other.
  The search of a substring is made without case sensitivity (large and small characters
  Do not differ). If LastPos exceeds length of string SourceString,
  The latest log-on of a substring FindString is searched. }

function Q_PosLastText (const FindString, SourceString: string;
  LastPos: Integer = MaxInt): Integer;

{ The functions Q_TablePosXXX can be used for multiple search of a substring
  In string (one or many), when the substring consists of several characters.
  These functions represent one of implementations of a search algorithm of substrings,
  Called " Boyer-Moore pattern searching algorithm ". At first is called(caused)
  The procedure such as Q_InitTablePosXXX for the job(definition) of a required substring, and then
  Appropriate function Q_TablePosXXX for actual search of a substring in
  To string. This second function can be called(caused) multiply for search some
  Fragment in many strings or for search of several log-ons of a substring in
  One string. All functions and procedures are threadsafe, i.e.
  Q_TablePosXXX Works only with a substring, which was indicated by call
  Q_InitTablePosXXX In the same stream. Simultaneously search can be conducted
  In the other stream, that will not affect in any way current stream. It is necessary to mark,
  That Q_InitTablePosStr and Q_InitTablePosText use to the same area
  Memories for storage of a required substring. Thus, it is impossible to mix search
  Substrings with the registration and without case sensitivity. The speed of operation of these functions can
  To be various depending on length of a substring and set included in it(her)
  Characters. In many cases these functions work faster, than Q_PosStr and
  Q_PosText, but more often it happens on the contrary.}

{ Q_InitTablePosStr initiates search of a substring in string with case sensitivity
  Characters. This procedure should be called(caused) for the job(definition) of a required substring
 , how the function Q_TablePosStr will be called(caused). }

procedure Q_InitTablePosStr (const FindString: string);

{ Q_TablePosStr finds in string SourceString a substring defined at
  Call Q_InitTablePosStr. In the parameter LastPos number of the character is transmitted,
  With which started previous retrieved log-on of this substring in
  To string SourceString. In the same parameter the position new is reset
  The next log-on of a required substring. Originally, value of the parameter
  LastPos should be equal to zero. If the substring is retrieved, function
  Q_TablePosStr Resets True, differently - False. If the substring is not retrieved,
  The value of the parameter LastPos does not vary.}

function Q_TablePosStr (const SourceString: string; var LastPos: Integer): Boolean;

{ Q_InitTablePosText initiates search of a substring in string without the registration
  The register of characters. This procedure should be called(caused) for the job(definition) required
  Substrings, how the function Q_TablePosText will be called(caused). }

procedure Q_InitTablePosText (const FindString: string);

{ Q_TablePosText finds in string SourceString a substring defined at
  Call Q_InitTablePosText. In the parameter LastPos number of the character is transmitted,
  With which started previous retrieved log-on of this substring in
  To string SourceString. In the same parameter the position new is reset
  The next log-on of a required substring. Originally, value of the parameter
  LastPos should be equal to zero. If the substring is retrieved, function
  Q_TablePosText Resets True, differently - False. If the substring is not retrieved,
  The value of the parameter LastPos does not vary.}

function Q_TablePosText (const SourceString: string; var LastPos: Integer): Boolean;

{ Q_ReplaceStr substitutes all входения of a substring FindString in string SourceString
  By substring ReplaceString. The search of a substring is made with case sensitivity
  (The large and small characters differ). The function resets string - result.
  If the substring FindString is absent in string SourceString, is reset
  The initial string SourceString.}

function Q_ReplaceStr (const SourceString, FindString, ReplaceString: string): string;      //Зш·ЦґуРЎРґ

{ Q_ReplaceText substitutes all входения of a substring FindString in string
  SourceString by a substring ReplaceString. The search of a substring is made without the registration
  The register (the large and small characters do not differ). The function resets
  String - result. If the substring FindString is absent in string SourceString,
  That is reset the initial string SourceString.}

function Q_ReplaceText (const SourceString, FindString, ReplaceString: string): string;   //І»Зш·ЦґуРЎРґ

{ Q_ReplaceFirstStr substitutes the first log-on of a substring FindString in string
  SourceString by a substring ReplaceString. The substring FindString is searched, starting
  From the character S [StartPos]. Number of the character, with which starts retrieved вхож-
  дение of a substring, is reset as result of the function (or zero, if a substring
  Is not retrieved). The search of a substring FindString is fulfilled with case sensitivity
  Characters (the large and small characters differ).}

function Q_ReplaceFirstStr (var S: string; const FindString, ReplaceString: string;
  StartPos: Integer = 1): Integer;

{ Q_ReplaceFirstText substitutes the first log-on of a substring FindString in string
  SourceString by a substring ReplaceString. The substring FindString is searched, starting
  From the character S [StartPos]. Number of the character, with which starts retrieved вхож-
  дение of a substring, is reset as result of the function (or zero, if a substring
  Is not retrieved). The search of a substring FindString is fulfilled without case sensitivity
  Characters (the large and small characters do not differ).}

function Q_ReplaceFirstText (var S: string; const FindString, ReplaceString: string;
  StartPos: Integer = 1): Integer;

{ Q_ReplaceLastStr substitutes the last log-on of a substring FindString in string
  SourceString by a substring ReplaceString. Substring FindString is searched to the left of
  The character S [LastPos] (i.e. in a fragment Copy (S, 1, LastPos-1)). Number of the character,
  With which the retrieved log-on starts, is reset as result of the function
  (Or zero, if the substring is not retrieved). The search of a substring FindString is fulfilled
  With case sensitivity of characters (the large and small characters differ). If
  LastPos exceeds length of string S, the latest log-on is substituted
  Required substring.}

function Q_ReplaceLastStr (var S: string; const FindString, ReplaceString: string;
  LastPos: Integer = MaxInt): Integer;

{ Q_ReplaceLastText substitutes the last log-on of a substring FindString in string
  SourceString by a substring ReplaceString. Substring FindString is searched to the left of
  The character S [LastPos] (i.e. in a fragment Copy (S, 1, LastPos-1)). Number of the character,
  With which the retrieved log-on starts, is reset as result of the function
  (Or zero, if the substring is not retrieved). The search of a substring FindString is fulfilled
  Without case sensitivity of characters (the large and small characters do not differ). If
  LastPos exceeds length of string S, the latest log-on is substituted
  Required substring.}

function Q_ReplaceLastText (var S: string; const FindString, ReplaceString: string;
  LastPos: Integer = MaxInt): Integer;

{ Q_DeleteStr deletes from string S all log-ons of a substring SubStrToDel. Search
  Substrings is carried on with case sensitivity of characters. The function resets quantity
  Retrieved (and remote) fragments.}

function Q_DeleteStr (var S: string; const SubStrToDel: string): Integer;

{ Q_DeleteText deletes from string S all log-ons of a substring SubStrToDel. Search
  Substrings is carried on without case sensitivity of characters. The function resets quantity
  Retrieved (and remote) fragments.}

function Q_DeleteText (var S: string; const SubStrToDel: string): Integer;

{ Q_DeleteFirstStr deletes the first log-on of a substring SubStrToDel from string
  SourceString. The substring SubStrToDel is searched, since the character S [StartPos].
  Number of the character, with which starts the retrieved log-on of a substring, возвра-
  щается as result of the function (or zero, if the substring is not retrieved). Search
  Substrings SubStrToDel is fulfilled with case sensitivity of characters (large and
  The small characters differ).}

function Q_DeleteFirstStr (var S: string; const SubStrToDel: string; StartPos:
  Integer = 1): Integer;

{ Q_DeleteFirstText deletes the first log-on of a substring SubStrToDel from string
  SourceString. The substring SubStrToDel is searched, since the character S [StartPos].
  Number of the character, with which starts the retrieved log-on of a substring, возвра-
  щается as result of the function (or zero, if the substring is not retrieved). Search
  Substrings SubStrToDel is fulfilled without case sensitivity of characters (large and
  The small characters do not differ).}

function Q_DeleteFirstText (var S: string; const SubStrToDel: string; StartPos:
  Integer = 1): Integer;

{ Q_DeleteLastStr deletes the last log-on of a substring SubStrToDel in string
  SourceString. The substring SubStrToDel is searched to the left of the character S [LastPos]
  (In a fragment Copy (S, 1, LastPos-1)). Number of the character, with which starts
  The retrieved log-on of a substring, is reset as result of the function (or zero,
  If the substring is not retrieved). The search of a substring SubStrToDel is fulfilled with the registration
  The register of characters (the large and small characters differ).}

function Q_DeleteLastStr (var S: string; const SubStrToDel: string; LastPos:
  Integer = MaxInt): Integer;

{ Q_DeleteLastText deletes the last log-on of a substring SubStrToDel in string
  SourceString. The substring SubStrToDel is searched to the left of the character S [LastPos]
  (In a fragment Copy (S, 1, LastPos-1)). Number of the character, with which starts
  The retrieved log-on of a substring, is reset as result of the function (or zero,
  If the substring is not retrieved). Search of a substring SubStrToDel is fulfilled without
  Case sensitivity of characters (the large and small characters do not differ).}

function Q_DeleteLastText (var S: string; const SubStrToDel: string; LastPos:
  Integer = MaxInt): Integer;

{ Q_ReplaceChar substitutes in string S each log-on of the character ChOld on the character
  ChNew. The result of the function is equal to quantity of the effected replacements. Initial
  The string S varies.}

function Q_ReplaceChar (var S: string; ChOld, ChNew: Char): Integer;

{ Q_ReplaceChars substitutes in string S all characters from string StrChOld соответст-
  вующими by characters from string StrChNew. The initial string S varies. If
  The number of characters in string StrChOld is not equal to a number of characters in string StrChNew,
  There is an exclusive situation such as Exception.}

procedure Q_ReplaceChars (var S: string; const StrChOld, StrChNew: string);

{ Q_ReplaceCharsByOneChar substitutes all labour contract going log-ons of characters from
  Sets ChOldSet in string S by one character ChNew. Initial string S at
  It varies. If it is necessary to substitute some characters by one without deleting
  Repetitions, take advantage of the procedure Q_ReplaceChars.}

procedure Q_ReplaceCharsByOneChar (var S: string; const ChOldSet: TCharSet;
  ChNew: Char);

{ Q_StrScan finds the first log-on of the character Ch in string S, since the character
  Number StartPos. Resets number of the retrieved character or zero, if the character Ch
  In string S is not retrieved. Function Q_PStrScan is similar Q_StrScan for
  Pointer (String).}

function Q_StrScan (const S: string; Ch: Char; StartPos: Integer = 1): Integer;
function Q_PStrScan (P: Pointer; Ch: Char; StartPos: Integer = 1): Integer;

{ Q_StrRScan finds the previous log-on of the character Ch in string S. In the parameter
  LastPos number of the character appropriate to current log-on Ch in is transmitted
  String S. The search starts with the character prior to the character S [LastPos] or
  From the last character of string, if LastPos exceeds length of string S. The function
  Resets number of the retrieved character or zero, if character Ch is not retrieved in
  To string S more to the left of the character with number LastPos. The function Q_PStrRScan is similar
  Q_StrRScan For Pointer (String).}

function Q_StrRScan (const S: string; Ch: Char; LastPos: Integer = MaxInt): Integer;
function Q_PStrRScan (P: Pointer; Ch: Char; LastPos: Integer = MaxInt): Integer;

{ Q_StrSpn resets number of the first character of string S, not included in string
  Delimiters, since the character number StartPos. The string Delimiters sets
  Set of characters - separators, and the function finds the first character,
  Not being a separator. If the usual characters in string S are absent,
  The zero is reset. If Delimiters is equal to empty string, the list is used
  Separators, which were preset during previous call of one of
  The following functions: Q_StrTok, Q_StrSpn, Q_StrCSpn, Q_ProperCase, Q_WordAtPos,
  Q_GetWordN, Q_ProperCaseInPlace, Q_SetDelimiters, Q_CountOfWords. If
  The separators are transmitted as set such as TCharSet, they are not stored
  For the subsequent calls of functions.}

function Q_StrSpn (const S, Delimiters: string; StartPos: Cardinal = 1): Integer; overload;
function Q_StrSpn (const S: string; StartPos: Cardinal = 1;
  const Delimiters: TCharSet = Q_StdDelimsSet): Integer; overload;

{ Q_StrCSpn resets number of the first character of string S, included in string
  Delimiters, since the character number StartPos. The string Delimiters sets
  Set of characters - separators, and the function finds the first character - separator.
  If the characters - separators in string S are absent, the zero is reset. If
  Delimiters is equal to empty string, the list of separators, which is used
  Were preset during the previous call of one from the following functions: Q_StrTok,
  Q_StrSpn, Q_StrCSpn, Q_ProperCase, Q_ProperCaseInPlace, Q_WordAtPos,
  Q_GetWordN, Q_SetDelimiters, Q_CountOfWords. If the separators are transmitted
  As set, they are not stored for the subsequent calls of functions.}

function Q_StrCSpn (const S, Delimiters: string; StartPos: Cardinal = 1): Integer; overload;
function Q_StrCSpn (const S: string; StartPos: Cardinal = 1;
  const Delimiters: TCharSet = Q_StdDelimsSet): Integer; overload;

{ Q_DelCharInPlace deletes all characters Ch from string S. Initial string at
  It varies.}

procedure Q_DelCharInPlace (var S: string; Ch: Char = ' ');

{ Q_DelChar deletes all characters Ch from string transferred(handed) by the parameter S.}

function Q_DelChar (const S: string; Ch: Char = ' '): string;

{ Q_Delete deletes a substring from string S. Thus the initial string varies.
  Index - index of the first deleted character, Count - quantity of characters,
  Subjects to deleting. This function works faster, than standard Delete.}

procedure Q_Delete (var S: string; Index, Count: Integer);

{ Q_DelChars deletes from string S characters, which are present at string
  (Or set) CharsToRemove. The initial string S varies.}

procedure Q_DelChars (var S: string; const CharsToRemove: string); overload;
procedure Q_DelChars (var S: string; const CharsToRemove: TCharSet); overload;

{ Q_KeepChars leaves in string S only those characters, which are present in
  To string (or set) CharsToKeep, other characters deletes. Initial
  The string S varies.}

procedure Q_KeepChars (var S: string; const CharsToKeep: string); overload;
procedure Q_KeepChars (var S: string; const CharsToKeep: TCharSet); overload;

{ Q_ApplyMask applies a mask Mask to string SourceStr and resets obtained
  String as result of the function. Character MaskChar is used in string Mask for
  The instructions(indication) of positions, in which are substituted characters from string SourceStr. Length
  SourceStr should be equal to quantity of characters MaskChar a mask. An example:
   Q_ApplyMask('(###) ##-##-##','075723293','# ') will return string ' (075) 72-32-93 '.
  The idea is borrowed by this and following three functions from the unit jbStr. Pas, writer
  Which is Jaro Benes (micrel@micrel.cz). }

function Q_ApplyMask (const Mask, SourceStr: string; MaskChar: Char = 'X'): string;

{ Q_ApplyMaskInPlace applies a mask Mask to string SourceStr and saves
  The obtained string in a variable Mask. The character MaskChar is used in string
  Mask for the instruction(indication) of positions, in which the characters from string are substituted
  SourceStr. Length of string SourceStr should correspond(meet) to quantity of characters
  Substitutions MaskChar in a mask Mask.}

procedure Q_ApplyMaskInPlace (var Mask: string; const SourceStr: string;
  MaskChar: Char = 'X');

{ Q_ExtractByMask deletes from string S all characters being fixed
  For the given mask Mask, leaving only substituted characters, i.e. characters,
  To which in a mask there corresponds(meets) the character MaskChar. Obtained thus
  The string is reset as result of the function. Length of strings S and Mask should be
  Identical. An example: Q_ExtractByMask ('7-35-01','X-XX-XX') will return '73501'.}

function Q_ExtractByMask (const S, Mask: string; MaskChar: Char = 'X'): string;

{ Q_ExtractByMaskInPlace deletes from string S all characters being
  Fixed for the given mask Mask, leaving only substituted characters,
  I.e. characters, with which in a mask there corresponds(meets) the character MaskChar. Obtained
  Thus string is saved in a variable transferred(handed) by the parameter S.
  Initial length of strings S and Mask should be identical.}

procedure Q_ExtractByMaskInPlace (var S: string; const Mask: string;
  MaskChar: Char = 'X');


{ Formatting strings, clipping of fragments of string.}

{ Q_TrimInPlace deletes carrying on and end blanks and command characters from
  Strings S. Thus the initial string varies. This procedure works
  Faster, than standard function Trim.}

procedure Q_TrimInPlace (var S: string);

{ Q_TrimLeftInPlace deletes carrying on blanks and command characters from string S.
  Thus the initial string varies. This procedure works faster, than
  The standard function TrimLeft.}

procedure Q_TrimLeftInPlace (var S: string);

{ Q_TrimRightInPlace deletes end blanks and command characters from
  Strings S. Thus the initial string varies. This procedure works
  Faster, than standard function TrimRight.}

procedure Q_TrimRightInPlace (var S: string);

{ Q_TrimChar deletes carrying on and end characters Ch from string S. Initial
  The string S does not vary.}

function Q_TrimChar (const S: string; Ch: Char = ' '): string;

{ Q_TrimCharLeft deletes carrying on characters Ch from string S. The initial string S
  Does not vary.}

function Q_TrimCharLeft (const S: string; Ch: Char = ' '): string;

{ Q_TrimCharRight deletes end characters Ch from string S. The initial string S
  Does not vary.}

function Q_TrimCharRight (const S: string; Ch: Char = ' '): string;

{ Q_KeepOneChar deletes all the labour contract going characters Ch, except for one, from string,
  Transferred(handed) by the parameter S. The initial string thus does not vary. For example,
  Q_KeepOneChar (' How do you do ', ') will return string ' How do you do '.}

function Q_KeepOneChar (const S: string; Ch: Char = ' '): string;

{ Q_SpaceCompressInPlace deletes from string initial and trailing blanks and
  The command characters (are less than a blank). Besides all the labour contract going blanks
  And the command characters in middle of string are substituted by one blank. Initial
  The string varies.}

procedure Q_SpaceCompressInPlace (var S: string);

{ Q_SpaceCompress deletes from string initial both trailing blanks and controlling
  The characters (are less than a blank). Besides all the labour contract going blanks and controlling
  The characters in middle of string are substituted by one blank. The initial string thus
  Does not vary. This function works more slowly, than Q_SpaceCompressInPlace.}

function Q_SpaceCompress (const S: string): string;

{ Q_PadLeft supplements string S by characters PadCh at the left up to length Length. If
  Length of string S is more Length,, if the parameter Cut = True, string обрезается
  On the right up to length Length, differently (Cut = False) the initial string S is reset. }

function Q_PadLeft (const S: string; Length: Integer; PadCh: Char = ' ';
  Cut: Boolean = False): string;

{ Q_PadRight supplements string S by characters PadCh on the right up to length Length. If
  Length of string S is more Length,, if the parameter Cut = True, string обрезается
  On the right up to length Length, differently (Cut = False) the initial string S is reset.}

function Q_PadRight (const S: string; Length: Integer; PadCh: Char = ' ';
  Cut: Boolean = False): string;

{ Q_CenterStr centers string S by characters PadCh concerning length Length.
  If length of string S is more Length,, if the parameter Cut = True, string
  обрезается on the right up to length Length, differently (Cut = False) is reset initial
  String S.}

function Q_CenterStr (const S: string; Length: Integer; PadCh: Char = ' ';
  Cut: Boolean = False): string;

{ Q_PadInside adds (evenly) characters PadCh in string S in those positions,
  Where they already are present, so long as length of string does not become equal
  Length. In other words, this function makes alignment of the text on both
  To edges(territories). If length of string exceeds Length,, if the parameter Cut = True,
  The string обрезается on the right up to length Length, differently (Cut = False) is reset
  The initial string S. Any special characters are not recognized.}

function Q_PadInside (const S: string; Length: Integer; PadCh: Char = ' ';
  Cut: Boolean = False): string;

{ Q_TabsToSpaces substitutes all tab characters (#9) in string S by blanks.
  The interval of a tab stops is set by the parameter TabStop.}

function Q_TabsToSpaces (const S: string; TabStop: Integer = 8): string;

{ Q_SpacesToTabs substitutes sequences of blanks in string S by characters
  Tab stops #9. The interval of a tab stops is set by the parameter TabStop. This function
  Works, even, if the initial string already contains the tab characters.}

function Q_SpacesToTabs (const S: string; TabStop: Integer = 8): string;

{ Q_StrTok resets next fragment of string S, simultaneously deleting it(him) from
  The initial string. Q_StrTok considers string S as sequence from
  Zero or more text fragments separated from each other one or more
  By character - separator from string Delimiters. In the parameter Delimiters is transmitted
  String, which consists of characters, which are considered as separators
  For string S. Separators do not switch on in a fragment reset
  By the function Q_StrTok. If the string starts with characters - separators, all
  They are deleted. If Delimiters - empty string, the separators are used,
  Which were preset during the previous call of one from the following functions:
  Q_StrTok, Q_StrSpn, Q_StrCSpn, Q_ProperCase, Q_WordAtPos, Q_SetDelimiters,
  Q_GetWordN, Q_ProperCaseInPlace, Q_CountOfWords. If the separators are transmitted
  As set, they are not stored for the subsequent calls of functions.}

function Q_StrTok (var S: string; const Delimiters: string): string; overload;
function Q_StrTok (var S: string;
  const Delimiters: TCharSet = Q_StdDelimsSet): string; overload;

{ Q_StrTok1 resets the next fragment of string S, simultaneously deleting it(him)
  From the initial string and deleting the character - separator, following it(him). Q_StrTok1
  Considers string S as a sequence from zero or more text
  Fragments separated from each other by a single character - separator from
  Strings Delimiters. If in string S the labour contract goes some separators,
  The function will reset empty string for each such character, if
  Before it(him) there is no text fragment. In the parameter Delimiters is transmitted
  String, which consists of characters, which are considered as separators
  For string S. Separators do not switch on in a fragment reset
  By the function Q_StrTok1. If Delimiters - empty string, the characters are used,
  Which were preset during the previous call of one from the following functions:
  Q_StrTok, Q_StrSpn, Q_StrCSpn, Q_ProperCase, Q_WordAtPos, Q_SetDelimiters,
  Q_GetWordN, Q_ProperCaseInPlace, Q_CountOfWords. If the separators are transmitted
  As set, they are not stored for the subsequent calls of functions.}

function Q_StrTok1 (var S: string; const Delimiters: string): string; overload;
function Q_StrTok1 (var S: string;
  const Delimiters: TCharSet = Q_StdDelimsSet): string; overload;

{ Q_WordAtPos resets a word from string S, in which composition is found
  The character S [Pos]. The string Delimiters sets characters considered as separators
  Words in string S. If the character in a position Pos is a separator, function
  Resets empty string. If Delimiters is equal to empty string, is used
  List of separators, which were preset during previous call of one of
  The following functions: Q_StrTok, Q_StrSpn, Q_StrCSpn, Q_ProperCase, Q_WordAtPos,
  Q_GetWordN, Q_ProperCaseInPlace, Q_SetDelimiters, Q_CountOfWords. If
  The separators are transmitted as set such as TCharSet, they are not stored
  For the subsequent calls of functions.}

function Q_WordAtPos (const S: string; Pos: Integer; const Delimiters: string): string; overload;
function Q_WordAtPos (const S: string; Pos: Integer;
  const Delimiters: TCharSet = Q_StdDelimsSet): string; overload;

{ Q_GetWordN resets a word with a serial number OrdN from string S. Words
  Are numbered from unit. The string Delimiters sets characters considered divide
  телями of words in string S. If the word with number OrdN in string S is absent,
  The empty string is reset. If Delimiters is equal to empty string, is used
  List of separators, which were preset during previous call of one of
  The following functions: Q_StrTok, Q_StrSpn, Q_StrCSpn, Q_ProperCase, Q_WordAtPos,
  Q_GetWordN, Q_ProperCaseInPlace, Q_SetDelimiters, Q_CountOfWords. If
  The separators are transmitted as set such as TCharSet, they are not stored
  For the subsequent calls of functions.}

function Q_GetWordN (OrdN: Integer; const S, Delimiters: string): string; overload;
function Q_GetWordN (OrdN: Integer; const S: string;
  const Delimiters: TCharSet = Q_StdDelimsSet): string; overload;

{ The idea of the following of five functions is borrowed from the unit cStrings.pas, writer
  Which is David Butler (david@e.co.za). }

{ Q_CopyRange resets a substring from string S, since the character, index
  Which is equal Start, and finishing the character with an index Stop.}

function Q_CopyRange (const S: string; Start, Stop: Integer): string;

{ Q_CopyFrom resets a substring from string S, since the character, index
  Which is equal Start, and up to the end of string.}

function Q_CopyFrom (const S: string; Start: Integer): string;

{ Q_CopyLeft resets Count of the first characters of string transferred(handed) by the parameter S.}

function Q_CopyLeft (const S: string; Count: Integer): string;

{ Q_CopyRight resets Count of the last characters of string transferred(handed)
  By the parameter S.}

function Q_CopyRight (const S: string; Count: Integer): string;

{ Q_PasteStr substitutes Count of characters of string Dest, since a position Pos, содер-
  жимым of string Source. The string Dest thus varies. For example, if string
  Dest is equal 'How do you do', after execution of the procedure: Q_PasteStr (Dest,
  5,6,'does he'), she(it) will be equal 'How does he do'. This procedure is fulfilled
  Faster, than combination standard Delete-Insert.}

procedure Q_PasteStr (var Dest: string; Pos, Count: Integer; const Source: string);

{ Q_CopyDel resets a substring, simultaneously deleting her(it) from the initial string.
  S - initial string, Start - number of the first cut character, Len - length
  Cut substring. This function was borrowed from AGSLib.pas written by Alexey Lukin.}

function Q_CopyDel (var S: string; Start, Length: Integer): string;


{ Other functions for operation with strings.}

{ Q_SetDelimiters sets string (or set) characters - separators of words
  For the subsequent call of such functions, as Q_StrTok, Q_StrSpn, Q_StrCSpn,
  Q_ProperCase, Q_WordAtPos, Q_GetWordN, Q_ProperCaseInPlace, Q_CountOfWords.
  In this case argument Delimiters by call of these functions should be equal
  To empty string. If procedure Q_SetDelimiters is called(caused) without parameters, in
  Quality of separators the characters from constant set are received
  Q_StdDelimsSet, unit, defined in the beginning, QStrings.}

procedure Q_SetDelimiters (const Delimiters: string); overload;
procedure Q_SetDelimiters (const Delimiters: TCharSet = Q_StdDelimsSet); overload;

{ Q_GetDelimiters resets string consisting of characters - separators of words,
  Listed in the order, their appropriate codes. If were not called(caused) earlier
  Any functions specifying string of separators (for example Q_SetDelimiters),
  The result of the function Q_GetDelimiters will be uncertain.}

function Q_GetDelimiters: string;

{ Q_StrMoveL copies contents of string Source in string Dest. Maximum
  The number of copied characters is equal MaxL. Length of string Dest is installed
  Equal to number of the copied characters. The memory for string Dest should be
  Is distributed beforehand by function call SetString (or SetLength) (size
  Not less MaxL of characters). Q_StrMoveL works much faster, than usual
  The assignment of string, at which occurs its(her) copying.}

procedure Q_StrMoveL (const Source: string; var Dest: string; MaxL: Cardinal);

{ Q_StrReverse overturns string S so, that the first character becomes
  By the latter, second - penultimate etc. thus varies initial
  String. The function Q_PStrReverse is similar Q_StrReverse for Pointer (String).
  Besides she(it) resets the pointer by the beginning of string.}

procedure Q_StrReverse (var S: string);
function Q_PStrReverse (P: Pointer): Pointer;

{ Q_CutLeft обрезает string S at the left on CharCount of characters, reducing thus
  Its(her) length. If the parameter CharCount negative, string обрезается on the right.}

procedure Q_CutLeft (var S: string; CharCount: Integer);

{ Q_CutRight обрезает string S on the right on CharCount of characters, reducing thus
  Its(her) length. If the parameter CharCount negative, string обрезается at the left.}

procedure Q_CutRight (var S: string; CharCount: Integer);

{ Q_RotateLeft cyclic shifts string S on Shift of characters to the left. The parameter
  Shift can be both positive, and negative.}

procedure Q_RotateLeft (var S: string; Shift: Integer);

{ Q_RotateRight cyclic shifts string S on Shift of characters to the right.
  The parameter Shift can be both positive, and negative.}

procedure Q_RotateRight (var S: string; Shift: Integer);

{ Q_Duplicate resets string consisting from Count of copies of string S.}

function Q_Duplicate (const S: string; Count: Integer): string;

{ Q_Base64Encode resets string S or byte array addressed by the parameter
  P, length L byte encoded in the format Base64 (MIME), frequently used
  At transfer of files by the e-mail. The resulting string is supplemented
  In the end by characters '=' up to length, multiple 4. To avoid addition, length
  The initial string or byte array should be multiple 3.}

function Q_Base64Encode (const S: string): string; overload;
function Q_Base64Encode (P: Pointer; L: Cardinal): string; overload;

{ Q_Base64Decode resets result раскодирования of string S from the coding
  Base64, i.e. restores initial contents of string. Length
  The encoded string should be multiple 4.}

function Q_Base64Decode (const S: string): string;

{ The following functions realize compression of text and binary strings by a method RLE.
  The given method is based on deleting from the text of sequences identical
  Characters (replacement them by one character and counter). If the text does not contain
  Similar sequences, his(its) length usually does not vary. Length of the text
  Can even be magnified (in the worse case twice), if it(he) contains
  Special characters (see constants RLECC in section implementation). This
  The algorithm is very fast and provides effective compression of the information,
  Containing group of repeating bytes.

  If the character repeats less than three times, any replacements is not made.
  If number of repetitions of the character from three up to eight, this sequence
  Is substituted by two characters, from 9 up to 127 - three characters, from 128 up to 16383 -
  By four, from 16384 up to 2097151 - five characters, from 2097152 up to 268435455 -
  By six characters, otherwise - seven characters. Everyone single special
  The character is substituted by a couple of characters, sequence from two special
  Characters (identical) the sequence from 3 is substituted by a couple of characters
  Up to 127 special characters is substituted by three characters, further - as it is usual.
  If it is supposed to use these procedures for compression gray-scale
  Graphics images, the algorithm of their operation should be reconsidered on a subject
  Decreases of number of special characters with seven up to two or three.}

{ Q_GetPackRLESize resets a size of the output array (in bytes), which
  Is necessary for saving in it(him) of results of compression by a method RLE of the data,
  Addressed by the parameter Source by a size SrcL byte. Greatest possible
  The size is equal to double value of the parameter SrcL (worst variant).}

function Q_GetPackRLESize (Source: Pointer; SrcL: Cardinal): Cardinal;

{ Q_GetUnpackRLESize resets a size of the output array (in bytes), which
  Is necessary for saving in it(him) of results of unpacking of the data compressed by a method
  RLE, addressed by the parameter Source by a size SrcL byte. This function is called(caused)
  Before call Q_UnpackRLE for the byte array.}

function Q_GetUnpackRLESize (Source: Pointer; SrcL: Cardinal): Cardinal;

{ Q_PackRLE fulfils data compression addressed by the parameter Source, length
  SrcL byte by a method RLE. The result is saved in a storage area pointed
  Dest. The result of the function is equal to a size (bytes) output data array.
  The size of a storage area Dest should be sufficient (necessary size
  Is defined(determined) with the help Q_GetPackRLESize). Inside the  function any
  Similar checks is not fulfilled.}

function Q_PackRLE (Source, Dest: Pointer; SrcL: Cardinal): Cardinal; overload;

{ Q_UnpackRLE fulfils unpacking the data addressed by the parameter Source, length
  SrcL byte compressed by a method RLE. The output data is saved in a storage area
  Pointed Dest. The result of the function is equal to a size (bytes) output
  Data array. The size of a storage area Dest should be sufficient for this purpose,
  To contain all data (it(him) it is possible to define with the help Q_GetUnpackRLESize).
  Inside the function Q_UnpackRLE the size of a storage area is not checked.}

function Q_UnpackRLE (Source, Dest: Pointer; SrcL: Cardinal): Cardinal; overload;

{ Q_PackRLE packs string S by a method RLE. In the parameter MaxL is transmitted
  Greatest possible length of output string or -1, for automatic
  Definitions of length of output string.}

function Q_PackRLE (const S: string; MaxL: Integer = -1): string; overload;

{ Q_UnpackRLE fulfils unpacking string S, compressed by a method RLE. In the parameter
  MaxL is transmitted greatest possible length of output string or -1, for
  Autodetections of length of output string.}

function Q_UnpackRLE (const S: string; MaxL: Integer = -1): string; overload;


{ Information functions.}

{ Q_PStrLen resets length of string such as PChar, addressed by the parameter P. This
  The function works much faster standard StrLen. By the writer of implementation
  Is Robert Lee (rhlee@optimalcode.com). }

function Q_PStrLen (P: PChar): Integer;

{ Q_IsEmptyStr resets True, if the string S contains only characters from string
  (Or set) EmptyChars, differently function resets False. This function can
  To be called(caused) without the parameter EmptyChars. In this case all characters smaller or
  Equal to a blank are considered empty.}

function Q_IsEmptyStr (const S, EmptyChars: string): Boolean; overload;
function Q_IsEmptyStr (const S: string; const EmptyChars: TCharSet): Boolean; overload;
function Q_IsEmptyStr (const S: string): Boolean; overload;

{ Q_CharCount foots quantity of log-ons of the character Ch in string S.}

function Q_CharCount (const S: string; Ch: Char): Integer;

{ Q_CharsCount foots in string S quantity of log-ons of characters,
  Present in set CharSet.}

function Q_CharsCount (const S: string; const CharSet: TCharSet): Integer;

{ Q_GetCharStr resets string containing in alphabetic order all characters
  (On one), included in a composition of the initial string S.}

function Q_GetCharStr (const S: string): string;

{ Q_CountOfWords foots quantity of words in string transferred(handed) by the parameter
  S. In string Delimiters the characters are transmitted which are considered just -
  Dividers of words in string S. If Delimiters is equal to empty string, is used
  List of separators, which were preset during previous call of one of
  The following functions: Q_StrTok, Q_StrSpn, Q_StrCSpn, Q_ProperCase, Q_WordAtPos,
  Q_GetWordN, Q_ProperCaseInPlace, Q_SetDelimiters, Q_CountOfWords. If
  The separators are transmitted as set such as TCharSet, they are not stored
  For the subsequent calls of functions.}

function Q_CountOfWords (const S, Delimiters: string): Integer; overload;
function Q_CountOfWords (const S: string;
  const Delimiters: TCharSet = Q_StdDelimsSet): Integer; overload;

{ Q_StrCheckSum resets the sum of the characters codes making string S. It
  The value can be used as the check total. The function Q_PStrCheckSum
  Is similar Q_StrCheckSum for Pointer (String). For a control of integrity
  The data are more reliable for using the function Q_CRC32.}

function Q_StrCheckSum (const S: string): LongWord;
function Q_PStrCheckSum (P: Pointer): LongWord;

{ Q_StrCheckXOR resets number which is growing out of association everyone
  Four characters of string S in one double word with the help " eliminating or ".
  This operation is fulfilled very fast. The obtained thus number can
  To be used as the check total or value of hash-function. However, for
  Reliable control of data integrity, it is better to use Q_CRC32. The function
  Q_PStrCheckXOR Is similar Q_StrCheckXOR for Pointer (String).}

function Q_StrCheckXOR (const S: string): LongWord;
function Q_PStrCheckXOR (P: Pointer): LongWord;

{ Q_StrHashKey generates hash-code for string transferred(handed) by the parameter S.
  In string the large and small characters differ. The function Q_PStrHashKey
  Is similar Q_StrHashKey for Pointer (String).}

function Q_StrHashKey (const S: string): LongWord;
function Q_PStrHashKey (P: Pointer): LongWord;

{ Q_TextHashKey generates hash-code for string transferred(handed) by the parameter S.
  The large and small characters do not differ. The function Q_PTextHashKey is similar
  Q_TextHashKey For Pointer (String).}

function Q_TextHashKey (const S: string): LongWord;
function Q_PTextHashKey (P: Pointer): LongWord;

{ Q_CRC32 calculates CRC-32 for a storage area addressed by the parameter P. A size
  To storage area (in bytes) is set by the parameter L.}

function Q_CRC32 (P: Pointer; L: Cardinal): LongWord;

{ Q_NextCRC32 calculates on base value CRC32 new value for area
  Memories addressed by the parameter P, size L byte. New CRC32 is reset
  As value of the same parameter, and also as result of the function. If
  Earlier CRC32 was not calculated, the appropriate parameter should be equal 0.}

function Q_NextCRC32 (var CRC32: LongWord; P: Pointer; L: Cardinal): LongWord;

{ Q_TimeStamp reads out contents of a 64-bit time counter, which
  Is magnified by unit at each clock tick of the processor (value of the counter
  Is read by the command RDTSC). This function does not work on processors below P5.}

function Q_TimeStamp: Int64;


{ Function for operation with sets such as TCharSet.}

{ The standard procedures Include, Exclude and 'in' operator are assembled
  In the effective enough code. Instead of all other operations with sets,
  For a type TCharSet it is necessary to use functions from this units.}

{ Q_GetCharSet resets set such as TCharSet, switching on only characters,
  Which are present at string S.}

function Q_GetCharSet (const S: string): TCharSet;

{ Q_CharSetToStr resets string of characters, which represents
  Listing in alphabetic order of characters present in set
  CharSet such as TCharSet.}

function Q_CharSetToStr (const CharSet: TCharSet): string;

{ Q_ComplementChar adds in set CharSet the character Ch, if it(him) there
  Was not, or deletes the character Ch from this set, if it(he) there was.}

procedure Q_ComplementChar (var CharSet: TCharSet; Ch: Char);

{ Q_ClearCharSet deletes all characters from set CharSet. After an output(exit)
  From this procedure the set CharSet becomes empty.}

procedure Q_ClearCharSet (var CharSet: TCharSet);

{ Q_FillCharSet adds in set CharSet all missing characters. The ambassador
  Output(exit) from this procedure the set CharSet contains all possible(probable) characters.}

procedure Q_FillCharSet (var CharSet: TCharSet);

{ Q_ComplementSet inverts set CharSet. After execution by this
  Procedures the set CharSet will contain only those characters, which
  Were absent in this set before call Q_ComplementSet.}

procedure Q_ComplementSet (var CharSet: TCharSet);

{ Q_CloneCharSet copies set SourceSet in set DestSet. Both these
  The sets should have the type TCharSet. After execution of this procedure call
  The functions Q_IsEqualSet (SourceSet, DestSet) will be returned by(with) value True.}

procedure Q_CloneCharSet (const SourceSet: TCharSet; var DestSet: TCharSet);

{ Q_CharSetUnion consolidates sets SourceSet and DestSet and saves result
  In set DestSet. The resulting set includes characters, which
  Contain even in one of the initial sets.}

procedure Q_CharSetUnion (var DestSet: TCharSet; const SourceSet: TCharSet);

{ Q_CharSetSubtract subtracts set SourceSet from set DestSet and
  Saves result in set DestSet. The resulting set includes
  Only characters, which were present at set DestSet, but not присут-
  ствовали (i.e. were absent) in set SourceSet.}

procedure Q_CharSetSubtract (var DestSet: TCharSet; const SourceSet: TCharSet);

{ Q_CharSetIntersect finds an intersection (common units) sets SourceSet
  And DestSet also saves result in set DestSet. Obtained thus
  The set contains only characters present in both sets.}

procedure Q_CharSetIntersect (var DestSet: TCharSet; const SourceSet: TCharSet);

{ Q_CharSetXOR adds in set DestSet characters present in
  Set SourceSet, but absent in set DestSet, also deletes from
  Sets DestSet characters present in set SourceSet and
  Simultaneously present in set DestSet.}

procedure Q_CharSetXOR (var DestSet: TCharSet; const SourceSet: TCharSet);

{ Q_IsSubset checks, whether is the set LeftSet a subset in RightSet.
  This function resets True, if all characters present in set
  LeftSet, contain as well in set RightSet. If it is not fulfilled,
  The function resets False.}

function Q_IsSubset (const LeftSet, RightSet: TCharSet): Boolean;

{ Q_IsSuperset checks, whether is set LeftSet superset for
  RightSet. This function resets True, if set LeftSet contains on
  To extreme measure all characters from set RightSet. If it is not fulfilled,
  The function resets False.}

function Q_IsSuperset (const LeftSet, RightSet: TCharSet): Boolean;

{ Q_IsEqualSet checks equality of sets LeftSet and RightSet. The function
  Resets True, if each character from set LeftSet is present in
  Set RightSet and on the contrary. Differently, the function resets False.}

function Q_IsEqualSet (const LeftSet, RightSet: TCharSet): Boolean;

{ Q_IsEmptySet resets True, if CharSet is equal to empty set (i.e. it
  The set does not contain any character), differently function resets False.}

function Q_IsEmptySet (const CharSet: TCharSet): Boolean;

{ Q_CharSetCharCount resets quantity of characters in set CharSet.}

function Q_CharSetCharCount (const CharSet: TCharSet): Integer;


{ Function for operation with character record of numbers.}

{ Plenty of functions for conversion of number in string and on the contrary,
  For operation with date and time is found in the unit SysUtils. Here represented
  Some functions, which are critical on execution time or
  Are absent in standard libraries.}

{ Q_IsInteger resets True, if the string S contains character record whole
  Decimal number, which can be saved in a variable such as Integer,
  Differently function resets False.}

function Q_IsInteger (const S: string): Boolean;

{ Q_IsCardinal resets True, if the string S contains character record
  беззнакового of the whole decimal or hexadecimal number, which can
  To be saved in a variable such as Cardinal, differently function resets False.}

function Q_IsCardinal (const S: string): Boolean;

{ Q_IsDecimal resets True, if the string S contains only characters,
  Appropriate to decimal digits, differently resets False.}

function Q_IsDecimal (const S: string): Boolean;

{ Q_IsHexadecimal resets True, if the string S contains only characters,
  Appropriate to hexadecimal digits, differently resets False.}

function Q_IsHexadecimal (const S: string): Boolean;

{ Q_IsOctal resets True, if the string S contains only characters,
  Appropriate to octal digits, differently resets False.}

function Q_IsOctal (const S: string): Boolean;

{ Q_IsBinary resets True, if the string S contains only characters,
  Appropriate to binary figures, differently resets False.}

function Q_IsBinary (const S: string): Boolean;

{ Q_IsFloat checks, whether the string S contains character record material
  Numbers, which includes a mantissa and, probably, the order (they can be as
  Positive, and negative). For sharing the whole and fractional part
  The character from a standard variable DecimalSeparator is used. If string
  S can be transformed to the material type, the function resets True,
  Differently возращается False.}

function Q_IsFloat (const S: string): Boolean;

{ Q_AdjustSeparator substitutes the first point in string S on a comma, if the character
  DecimalSeparator is equal comma, or substitutes first comma in string S on
  Point, if DecimalSeparator is equal to a point. After application of this function to
  String she(it) can be transformed in number with fractional part irrespective of
  What character the fractional part is separated (point or comma). However,
  It is necessary to remember, that the procedure Val does not pay notice on DecimalSeparator and
  Always expects, that the fractional part is separated by a point.}

function Q_AdjustSeparator (const S: string): string;

{ The functions Q_BetweenXXX reset True, if the string S contains number (whole,
  Whole without the sign, 64-bit whole, material or number designating
  The pecuniary sum), finding in a range [LowBound, HighBound]. Differently,
  The functions reset False. By operation with Q_BetweenFloat and Q_BetweenCurr
  It is necessary to take into account, that the whole part of number should be separated from fractional
  Parts by the character assigned by a variable DecimalSeparator (from SysUtils).}

function Q_BetweenInt (const S: string; LowBound, HighBound: Integer): Boolean;
function Q_BetweenUInt (const S: string; LowBound, HighBound: LongWord): Boolean;
function Q_BetweenInt64 (const S: string; LowBound, HighBound: Int64): Boolean;
function Q_BetweenFloat (const S: string; LowBound, HighBound: Extended): Boolean;
function Q_BetweenCurr (const S: string; LowBound, HighBound: Currency): Boolean;

{ The functions Q_StrToXXX will convert number, which character record contains
  In string S, in the normal numerical form. The variable, in which is saved
  The result, is transmitted by the parameter V. If conversion of string S in number V
  Has passed successfully, the functions reset True. If during conversion has arisen
  The error, is reset False. The exclusive situation thus does not arise.}

function Q_StrToInt (const S: string; var V: Integer): Boolean;
function Q_StrToUInt (const S: string; var V: LongWord): Boolean;
function Q_StrToInt64 (const S: string; var V: Int64): Boolean;
function Q_StrToFloat (const S: string; var V: Extended): Boolean;
function Q_StrToCurr (const S: string; var V: Currency): Boolean;

{ Q_IntToStr resets decimal record of number N as string.}

function Q_IntToStr (N: Integer): string;

{ Q_IntToStrBuf saves decimal record of number N in string S. Memory under
  String S should be selected(allocated) beforehand by function call SetString (or
  SetLength) size, sufficient for storage of the greatest possible number N
  In view of the sign. Q_IntToStrBuf works faster, than Q_IntToStr.}

procedure Q_IntToStrBuf (N: Integer; var S: string);

{ Q_UIntToStr resets decimal record беззнакового of number N as string.}

function Q_UIntToStr (N: LongWord): string;

{ Q_UIntToStrBuf saves decimal record беззнакового of number N in string S.
  The memory under string S should be selected(allocated) beforehand by function call SetString
  (Or SetLength) size, sufficient for storage greatest possible
  Numbers N. Q_UIntToStrBuf works faster, than Q_UIntToStr.}

procedure Q_UIntToStrBuf (N: LongWord; var S: string);

{ Q_UIntToStrL resets decimal record беззнакового of number N as string.
  The parameter Digits sets quantity of characters in reset number. If it is necessary,
  It обрезается at the left or is supplemented in zero& at the left up to length Digits.}

function Q_UIntToStrL (N: LongWord; Digits: Cardinal): string;

{ Q_UIntToStrLBuf saves decimal record беззнакового of number N in string S.
  The parameter Digits sets quantity of characters in reset number. If it is necessary,
  The string S обрезается at the left or is supplemented in zero& at the left up to length Digits.
  The memory under string S should be selected(allocated) beforehand by function call SetString
  By size not less Digits of characters. Q_UIntToStrLBuf works faster, than
  Q_UIntToStrL.}

procedure Q_UIntToStrLBuf (N: LongWord; Digits: Cardinal; var S: string);

{ Q_IntToRoman resets string containing number N, written Roman
  In digits. A possible(probable) range of numbers: from 1 up to 5000. If number N does not lay in
  The indicated range, '?????' string is returned.}

function Q_IntToRoman (N: Integer): string;

{ Q_RomanToInt will convert Roman number (беззнаковое), transmitted string S,
  In an ordinary integer. If during conversion there is an error,
  The exclusive situation EConvertError. is excited}

function Q_RomanToInt (const S: string): Integer;

{ Q_UIntToHex resets hexadecimal record беззнакового number N as
  Strings. The parameter Digits sets quantity of digits in reset number. If
  It is necessary, it обрезается at the left or is supplemented in zero& at the left up to length Digits.}

function Q_UIntToHex (N: LongWord; Digits: Cardinal): string;

{ Q_UIntToHexBuf saves hexadecimal record беззнакового number N in
  To string S. The parameter Digits sets quantity of digits in reset number. If
  It is necessary, the string S обрезается at the left or is supplemented in zero& at the left up to length Digits.
  The memory under string S should be selected(allocated) beforehand by function call SetString
  By size not less Digits of characters. Q_UIntToHexBuf works faster, than
  Q_UIntToHex.}

procedure Q_UIntToHexBuf (N: LongWord; Digits: Cardinal; var S: string);

{ Q_HexToUInt will convert hexadecimal number transferred(handed) by string S,
  As a whole беззнаковое number. If during conversion there is an error,
  The exclusive situation EConvertError is excited.}

function Q_HexToUInt (const S: string): LongWord;

{ Q_UIntToOct resets octal record беззнакового number N as
  Strings. The parameter Digits sets quantity of digits in reset number. If
  It is necessary, it обрезается at the left or is supplemented in zero& at the left up to length Digits.}

function Q_UIntToOct (N: LongWord; Digits: Cardinal): string;

{ Q_UIntToOctBuf saves octal record беззнакового of number N in string S.
  The parameter Digits sets quantity of digits in reset number. If it is necessary,
  The string S обрезается at the left or is supplemented in zero& at the left up to length Digits.
  The memory under string S should be selected(allocated) beforehand by function call SetString
  By size not less Digits of characters. Q_UIntToOctBuf works faster, than
  Q_UIntToOct.}

procedure Q_UIntToOctBuf (N: LongWord; Digits: Cardinal; var S: string);

{ Q_OctToUInt will convert an octal number transferred(handed) by string S,
  As a whole беззнаковое number. If during conversion there is an error,
  The exclusive situation EConvertError is excited. }

function Q_OctToUInt (const S: string): LongWord;

{ Q_UIntToBin resets binary notation беззнакового of number N as string.
  The parameter Digits sets quantity of digits in reset number. If it is necessary
  обрезается at the left or is supplemented in zero& at the left up to length Digits.}

function Q_UIntToBin (N: LongWord; Digits: Cardinal): string;

{ Q_UIntToBinBuf saves binary notation беззнакового of number N in string S.
  The parameter Digits sets quantity of digits in reset number. If it is necessary,
  The string S обрезается at the left or is supplemented in zero& at the left up to length Digits.
  The memory under string S should be selected(allocated) beforehand by function call SetString
  By size not less Digits of characters. Q_UIntToBinBuf works faster, than
  Q_UIntToBin.}

procedure Q_UIntToBinBuf (N: LongWord; Digits: Cardinal; var S: string);

{ Q_BinToUInt will convert binary number transferred(handed) by string S, as a whole
  беззнаковое number. If during conversion there is an error,
  The exclusive situation EConvertError is excited. }

function Q_BinToUInt (const S: string): LongWord;

{ Q_ChangeBase translates беззнаковое number from a number system on the basis
  BaseFrom, in a number system on the basis BaseTo. Character record of number
  Is transmitted by string Number. The function resets number (string)((line)) written
  On the new basis. Parameters BaseFrom and BaseTo should be in range from
  2 up to 36. The digits more to than nine are designated by latin characters from A up to Z (or
  From a up to z). Quantity of signs among is not limited. If at it(him) are present
  Impermissible characters, the exclusive situation EConvertError is excited.
  If the parameter DigitsInGroup is distinct from zero, the digits in output string will be
  To be consolidated in groups on DigitsInGroup of digits. Among themselves groups are divided
  By the character GroupSeparator. The same character can be used in initial
  Number Number for sharing groups of digits.}

function Q_ChangeBase (const Number: string; BaseFrom, BaseTo: Cardinal;
  DigitsInGroup: Cardinal = 0; GroupSeparator: Char = ' '): string;

{ Q_StrToCodes resets string consisting of hexadecimal codes,
  Characters of string S. For example, Q_StrToCodes('XYZ#$*~') - > '58595A23242A7E'.
  The initial string thus does not vary.}

function Q_StrToCodes (const S: string): string;

{ Q_CodesToStr will convert string S, consisting from hexadecimal codes
  Characters, in string of characters. For example, Q_StrToCodes ('413F3C2A') - > 'A?<*'.
  The initial string thus does not vary. If during conversion
  There is an error, the exclusive situation EConvertError is excited. }

function Q_CodesToStr (const S: string): string;

{ Q_NumToStr saves in string S number transferred(handed) by the parameter N, written
  By words (in Russian). The parameter FmtFlags sets a way of conversion of number
  In string. The function Q_NumToStr resets number of the form, in which should stand
  The following behind the given number a word (see comment to constants rfmXXXX).
  The string S is always ended by a blank. If want to remove it(him), call
  Then Q_ShiftRight (S, 1).}

const
{ The constants for the parameter FmtFlags (can be consolidated them with the help "or"):}

  nsMale = 1;    {a Man's sort}
  nsFemale = 2;  {a Female sort}
  nsMiddle = 3;  {an Average sort}

  nsFull = 0;  {the Complete title of triads: one thousand, one million...}
  nsShort = 4; {the Brief title of triads: thousand, млн....}

{ Reset values of the function Q_NumToStr:}

  rfmFirst = 1;  {the First form: " one elephant " or " twenty one cats "}
  rfmSecond = 2; {the Second form: " three elephants " or " four cats "}
  rfmThird = 3;  {the Third form: " six elephants " or " eight cats "}

function Q_NumToStr(N: Int64; var S: string; FmtFlags: LongWord = nsMale): Integer;

{ Q_NumToRub resets the pecuniary sum in words. The parameter V should contain
  Numerical value of the pecuniary sum in roubles. The 100-th shares express copecks.
  The parameters RubFormat and CopFormat define(determine) record format accordingly
  Roubles and copecks. If CopFormat = nrNone, the sum is rounded off up to roubles and
  The copecks are not output. If RubFormat = nrNone, roubles are not output, and to
  To copecks the number of roubles multiplied on increases 100. If both parameters
  Are equal nrNone, the string containing number V is simply reset. A constant
  nrShTriad is combined with other constants with the help of the bit operation
  "or". The reset string starts with the large character. If the pecuniary sum
  Negative, the string consists in parentheseses. This function is written
  After acquaintance with the procedure num_to_rub (N2R.pas) Николая Глушнева.}

const
  nrNumShort = 1; {the Brief numerical format: " 475084 руб. " Or " 15 коп. "}
  nrShort = 3;    {the Brief lower case format: " руб. " Or " ten коп. "}
  nrNumFull = 0;  {the Complete numerical format: " 342 roubles " or " 25 copecks "}
  nrFull = 2;     {the Complete lower case format: " One rouble " or " two copecks "}
  nrShTriad = 4;  {Brief record of the titles of triads: thousand, млн....}
  nrNone = 8;     {There are no roubles, there are no copecks or simple numerical record}

function Q_NumToRub (V: Currency; RubFormat: LongWord = nrFull;
  CopFormat: LongWord = nrNumShort): string;

{ Function for operation with binary strings.}

{ Q_ZeroMem unsets a storage area pointed by the parameter P. Number byte
  Is set by the parameter L.}

procedure Q_ZeroMem (P: Pointer; L: Cardinal);

{ Q_OnesMem fills storage area addressed by the parameter P, units in
  All bits (byte $FF). The number byte is set by the parameter L.}

procedure Q_OnesMem (P: Pointer; L: Cardinal);

{ Q_FillChar fills L in adjoining byte addressed by the parameter P, value C.
  This procedure works faster standard FillChar.}

procedure Q_FillChar (P: Pointer; L: Cardinal; Ch: Char); overload;
procedure Q_FillChar (P: Pointer; L: Cardinal; Ch: Byte); overload;

{ Q_FillLong fills the array of units such as LongWord (or anyone another
  4-х-байтного such as), pointed by the parameter P, value Value. Quantity
  Array cells (not of bytes!!!) is transmitted by the parameter Count.}

procedure Q_FillLong (Value: LongWord; P: Pointer; Count: Cardinal);

{ Q_TinyFill fast fills up to 32 bytes of memory (inclusively), addressed
  By the parameter P, value, which is transmitted by the parameter Value (are used
  All 4 bytes Value). In the parameter L the number byte is set which is necessary
  To fill. Impermissible& to call(cause) Q_TinyFill with L it is more 32.}

procedure Q_TinyFill (P: Pointer; L: Cardinal; Value: LongWord);

{ Q_FillRandom fills the byte array addressed P, length L byte output
  By sequence of the standard generator of pseudo-random numbers. Initial
  The value of the generator is transmitted by the parameter Seed. It is necessary to mean, that
  This sequence is not криптографически a rack and impermissible&
  To use her(it) for creation of keys of encoding.}

procedure Q_FillRandom (P: Pointer; L: Cardinal; Seed: LongWord);

{ Q_CopyMem copies L byte from a storage area pointed by the parameter
  Source, in a storage area pointed by the parameter Dest. Storage areas
  Should not be overlapped. Q_CopyMem works much faster, than
  The standard procedure Move and, even, faster, than Q_MoveMem.}

procedure Q_CopyMem (Source, Dest: Pointer; L: Cardinal);

{ Q_CopyLongs copies Count of array cells such as LongWord (or any
  Another 4-х-байтного of a type, for example, Integer or Pointer) from a storage area,
  Pointed by the parameter Source, in a storage area addressed by the parameter Dest.
  The storage areas should not be overlapped.}

procedure Q_CopyLongs (Source, Dest: Pointer; Count: Cardinal);

{ Q_TinyCopy fast copies up to 32 bytes (inclusively) from a storage area,
  Pointed by the parameter Source in a storage area pointed by the parameter Dest.
  The number byte for copying is set by the parameter L. An overlap of storage areas
  Is not admitted. The call with L > 32 results in interesting errors.}

procedure Q_TinyCopy (Source, Dest: Pointer; L: Cardinal);

{ Q_MoveMem copies L byte from Source in Dest. She(it) works even in a case,
  When the memories addressed by parameters Source and Dest, are overlapped.
  If the memories are displaced from each other less, than on 4 bytes,
  It is better to take advantage of procedures Q_MoveBytes or Q_MoveWords. If beforehand
  It is known, that the memories are not overlapped, it is better to use Q_CopyMem.
  Q_MoveMem Works faster, than standard procedure Move.}

procedure Q_MoveMem (Source, Dest: Pointer; L: Cardinal);

{ Q_MoveLongs copies Count of array cells such as LongWord (or another
  4-х-байтного such as) from storage area pointed by the parameter Source, in
  Storage area pointed by the parameter Dest. This procedure works even
  In a case, when the storage areas are overlapped.}

procedure Q_MoveLongs (Source, Dest: Pointer; Count: Cardinal);

{ Q_MoveWords copies Count of array cells of a type Word or Smallint (2-bytes)
  from a storage area pointed by the parameter Source, in a storage area,
  Pointed by the parameter Dest. The storage areas can be overlapped.}

procedure Q_MoveWords (Source, Dest: Pointer; Count: Cardinal);

{ Q_MoveBytes copies L byte from a storage area pointed by the parameter
  Source in a storage area pointed by the parameter Dest. This procedure
  It is recommended to use, if offset of one storage area rather
  Another is less 4 (especially, when it is equal 1).}

procedure Q_MoveBytes (Source, Dest: Pointer; L: Cardinal);

{ Q_SwapMem interchanges contents of two byte arrays addressed P1 and P2,
  In length L byte. Before usage of this procedure consider possibility
  Exchange of values of pointers by the procedure Q_Exchange.}

procedure Q_SwapMem (P1, P2: Pointer; L: Cardinal);

{ Q_SwapLongs interchanges contents of the arrays 4-байтных of units addressed
  P1 And P2, length Count of double words. Before usage of this procedure
  Think of possibility of exchange of values of the pointers with the help Q_Exchange.}

procedure Q_SwapLongs (P1, P2: Pointer; Count: Cardinal);

{ Q_CompareMem fulfils побайтное matching of the memories addressed P1 and P2.
  The size of the memories (in bytes) is set by the parameter L. The function resets
  True, if contents of the memories are completely identical. This function works
  Faster by standard CompareMem.}

function Q_CompareMem (P1, P2: Pointer; L: Cardinal): Boolean;

{ Q_CompLongs fulfils побайтное matching of the arrays четырехбайтный of units
  (For example, such as Integer), addressed P1 and P2. Quantity of units in the arrays
  Is set by the parameter Count. The function resets True, if all appropriate
  The units of both arrays are equal, is otherwise reset False.}

function Q_CompLongs (P1, P2: Pointer; Count: Cardinal): Boolean;

{ Q_CompMemS fulfils побайтное matching of the memories addressed P1 and P2,
  By a principle it is more - less. The size of both memories (in bytes) is set
  By the parameter L. The function resets number less than zero, if contents first
  It is less than the block second (greater weight have bytes with the smaller address), resets
  The number is more than zero, if contents of the first block are more than second and 0, if
  Contents of both memories are completely identical.}

function Q_CompMemS (P1, P2: Pointer; L: Cardinal): Integer;

{ Q_ScanInteger finds number transferred(handed) by the parameter N in the array of numbers of a type
  Integer, pointed by the parameter ArrPtr. An index of the retrieved array cell
  Is reset as result of the function (units are numbered from zero). If number
  N in the array is not retrieved, is reset -1. In the parameter Count is transmitted
  Quantity of units in the array. Functions Q_ScanLongWord and Q_ScanPointer
  Are completely similar Q_ScanInteger.}

function Q_ScanInteger (N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
function Q_ScanLongWord (N: LongWord; ArrPtr: Pointer; Count: Cardinal): Integer;
function Q_ScanPointer (P: Pointer; ArrPtr: Pointer; Count: Cardinal): Integer;

{ Q_ScanWord finds number transferred(handed) by the parameter N in the array of numbers of a type
  Word, pointed by the parameter ArrPtr. An index of the retrieved array cell
  Is reset as result of the function (units are numbered from zero). If number
  N in the array is not retrieved, is reset -1. In the parameter Count is transmitted
  Quantity of units in the array.}

function Q_ScanWord (N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;

{ Q_ScanByte finds number transferred(handed) by the parameter N in the byte array,
  Pointed by the parameter ArrPtr. An index of the retrieved array cell
  Is reset as result of the function (index of first byte is equal to zero). If
  The number N in the array is not retrieved, is reset -1. In the parameter L is transmitted
  Size of the array in bytes.}

function Q_ScanByte (N: Integer; ArrPtr: Pointer; L: Cardinal): Integer;

{ Function Q_ScanGE_XXX scan array addressed ArrPtr, consisting from
  Count of units of an appropriate type, in searches of value, greater or
  Equal to number N. If such value is retrieved, the function resets an index
  Appropriate unit (indexing from zero). If a unit, greater or
  Equal N, in the array is absent, the function resets -1.}

function Q_ScanGE_Integer (N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
function Q_ScanGE_LongWord (N: LongWord; ArrPtr: Pointer; Count: Cardinal): Integer;
function Q_ScanGE_Word (N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;

{ Function Q_ScanLesser_XXX scan array addressed ArrPtr, consisting from
  Count of units of an appropriate type, in searches of value, smaller N. If
  Such value is retrieved, the function resets an index of an appropriate unit
  (Indexing from zero). If the unit, smaller N, in the array is absent,
  The function resets -1.}

function Q_ScanLesser_Integer (N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
function Q_ScanLesser_LongWord (N: LongWord; ArrPtr: Pointer; Count: Cardinal): Integer;
function Q_ScanLesser_Word (N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;

{ Q_CountInteger foots quantity of log-ons of number N in the array of numbers
  Such as Integer, pointed by the parameter ArrPtr. In the parameter Count is transmitted
  Quantity of units in the array. Functions Q_CountLongWord and Q_CountPointer
  Are completely similar Q_CountInteger and are entered only for convenience.}

function Q_CountInteger (N: Integer; ArrPtr: Pointer; Count: Cardinal): Cardinal;
function Q_CountLongWord (N: LongWord; ArrPtr: Pointer; Count: Cardinal): Cardinal;
function Q_CountPointer (P: Pointer; ArrPtr: Pointer; Count: Cardinal): Cardinal;

{ Q_CountWord foots quantity of log-ons of number N in the array of numbers
  Type Word, pointed by the parameter ArrPtr. In the parameter Count is transmitted
  Quantity of units in the array.}

function Q_CountWord (N: Integer; ArrPtr: Pointer; Count: Cardinal): Cardinal;

{ Q_CountByte foots quantity of log-ons of number N in the byte array,
  Pointed by the parameter ArrPtr. The parameter L sets a size of the array in bytes.}

function Q_CountByte (N: Integer; ArrPtr: Pointer; L: Cardinal): Cardinal;

{ Q_ReverseLongArr overturns the array of units such as LongWord so, that
  The first unit becomes the latter, second - penultimate etc. P - address
  The array, Count - quantity of units in the array.}

procedure Q_ReverseLongArr (P: Pointer; Count: Cardinal);

{ Q_ReverseWordArr overturns the array of units of a type Word so, that first
  The unit becomes the latter, second - penultimate etc. P - address of the array,
  Count - quantity of units in the array.}

procedure Q_ReverseWordArr (P: Pointer; Count: Cardinal);

{ Q_ReverseByteArr overturns the byte array so, that the first unit
  There is the latter, second - penultimate etc. P - address of the array,
  L - size of the array in bytes.}

procedure Q_ReverseByteArr (P: Pointer; L: Cardinal);

{ Q_BSwap changes the order of following of bytes in a double word D (first byte
  There is the latter, second - third and on the contrary).}

function Q_BSwap (D: LongWord): LongWord;

{ Q_BSwapLongs changes the order of following of bytes in each double word
  The array 4-х-байтных of units, which address is transmitted by the parameter P. Count
  Sets quantity of units in the array.}

procedure Q_BSwapLongs (P: Pointer; Count: Cardinal);

{ Q_Exchange interchanges values of two variables. The type of variables can be
  By one of the following: String, Pointer, PChar, Integer, Cardinal, Longint,
  LongWord, Single.}

procedure Q_Exchange (var A, B);

{ Q_ExchangeWords interchanges values variable anyone 2-x-байтного of a type.
  This type can be one of the following: Word, SmallInt, WideChar.}

procedure Q_ExchangeWords (var A, B);

{ Q_ExchangeBytes interchanges values of two variables by a size in one byte.
  This type can be one of the following: Byte, ShortInt, Char.}

procedure Q_ExchangeBytes (var A, B);

{ The functions Q_RemValue_XXX delete from the array of units of an appropriate type
  All units, which value are equal N (or P for a pointer array).
  ArrPtr - address of the array, Count (or L for the array of bytes) - initial number
  Units in the array. The functions reset new number of units, which
  Remained in the array after deleting an indicated value.}

function Q_RemValue_Integer (N: Integer; ArrPtr: Pointer; Count: Cardinal): Cardinal;
function Q_RemValue_LongWord (N: LongWord; ArrPtr: Pointer; Count: Cardinal): Cardinal;
function Q_RemValue_Pointer (P: Pointer; ArrPtr: Pointer; Count: Cardinal): Cardinal;

function Q_RemValue_Word (N: Integer; ArrPtr: Pointer; Count: Cardinal): Cardinal;

function Q_RemValue_Byte (N: Integer; ArrPtr: Pointer; L: Cardinal): Cardinal;

{ The functions Q_RemDuplicates_XXX delete from the array 4-байтных, 2-байтных or
  1-байтных of units, accordingly, all following one behind another (labour contract)
  Repeating units, i.e., if in the array is present a little number standing
  Units having the same value, from them there will be only one
  Unit with such value, and all others will be deleted. If the initial array
  Sorted, after application of the function Q_RemDuplicates_XXX, all
  The values in it(him) will be unique. If two arrays containing unique
  Value to unite in one array, then to sort it(him) and to apply to
  It(him) given function, then in obtained array will be all units (on
  To one), which are even in one of the initial arrays (operation "OR").
  ArrPtr - address of the array, Count (or L for the array of bytes) - initial number
  Units. The functions reset new number of units in the array, which
  Remained after deleting repetitions.}

function Q_RemDuplicates_Int32 (ArrPtr: Pointer; Count: Cardinal): Cardinal;
function Q_RemDuplicates_Int16 (ArrPtr: Pointer; Count: Cardinal): Cardinal;
function Q_RemDuplicates_Byte (ArrPtr: Pointer; L: Cardinal): Cardinal;

{ The functions Q_ANDSetInPlace_XXX delete from the array 4-байтных or 2-байтных
  Units, accordingly, all units, which are present at the array
  In the single copy (i.e. the unit will be remote, if at once ambassador
  Him(it) does not cost a unit with the same value). All repeating the labour contract
  The units are substituted of unique units with such value (i.e.,
  If in the array is present a little by number of standing units having one and
  The same value, from them remain only one unit with such value,
  And all others will be deleted). If two arrays containing unique values
  To unite in one array, then to sort it(him) and to apply to it(him) given
  Function, in result will stay only value, which are present in
  Both initial arrays (operation "And"). ArrPtr - address of the array, Count -
  The initial number of units. The functions reset new number of units, which
  Remained in the array after deleting repetitions and single values.}

function Q_ANDSetInPlace_Int32 (ArrPtr: Pointer; Count: Cardinal): Cardinal;
function Q_ANDSetInPlace_Int16 (ArrPtr: Pointer; Count: Cardinal): Cardinal;

{ The functions Q_XORSetInPlace_XXX delete from the array 4-байтных or 2-байтных
  Units, accordingly, all units, which are present at the array
  In several copies (one behind another, labour contract). In the array remain only
  Units which are not having "doubles". If two arrays containing unique
  Value to unite in one array, then to sort it(him) and to apply to
  To it(him) the given function, in this array the values, which will stay only
  Are present at one of the initial arrays, but not in both at once (operation
  " Eliminating OR "). ArrPtr - address of the array, Count - initial number
  Units. The functions reset new number of units, which remained
  In the array after deleting all not single values.}

function Q_XORSetInPlace_Int32 (ArrPtr: Pointer; Count: Cardinal): Cardinal;
function Q_XORSetInPlace_Int16 (ArrPtr: Pointer; Count: Cardinal): Cardinal;

{ The procedures Q_Sort_XXX sort the array 4-байтных or 2-байтных of units
  Appropriate type in ascending order (the algorithm fast is used
  Sortings). ArrPtr - address of the array, Count - number of units in the array.
  The value of the address of the array ArrPtr should be to a multiple size of an array cell.
  For descending sort sort the array with the help of one of the following
  Procedures, and then invert it(him) with the help Q_ReverseLong (Word) Arr.}

procedure Q_Sort_Integer (ArrPtr: Pointer; Count: Cardinal);
procedure Q_Sort_LongWord (ArrPtr: Pointer; Count: Cardinal);
procedure Q_Sort_Word (ArrPtr: Pointer; Count: Cardinal);

{ The functions Q_SearchUnique_XXX fulfil binary search in the array 4-байтных or
  2-байтных of units of value of an appropriate type. The array previously
  Should be sorted by increase. The function resets an index (starting
  From zero) retrieved value N in the array, which address is transmitted ArrPtr,
  Consisting from Count of units. If the value N in the array is not retrieved, function
  Resets -1. If the array contains repeating units, retrieved
  The unit will be not necessary for first among having such value.}

function Q_SearchUnique_Integer (N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
function Q_SearchUnique_LongWord (N: LongWord; ArrPtr: Pointer; Count: Cardinal): Integer;
function Q_SearchUnique_Word (N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;

{ The functions Q_SearchFirst_XXX fulfil binary search in the array 4-байтных or
  2-байтных of units of value of an appropriate type. The array previously
  Should be sorted by increase. The function resets very first
  Index (since zero) retrieved value N in the array, which address
  Is transmitted ArrPtr, consisting from Count of units. If value N in the array
  Is not retrieved, the function resets -1. If in the array there are repeating units,
  That the retrieved index always will be minimum for a given data.}

function Q_SearchFirst_Integer (N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
function Q_SearchFirst_LongWord (N: LongWord; ArrPtr: Pointer; Count: Cardinal): Integer;
function Q_SearchFirst_Word (N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;

{ The functions Q_SearchFirstGE_XXX fulfil binary search in the array 4-байтных or
  2-байтных of units of value of an appropriate type, greater or equal N.
  The array previously should be sorted by increase. The function
  Resets an index (since zero) first unit, which value is more
  Or is equal N, in the array, which address is transmitted ArrPtr, consisting from Count
  Units. If the value, greater or equal N, in the array is not retrieved,
  The function resets -1.}

function Q_SearchFirstGE_Integer (N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
function Q_SearchFirstGE_LongWord (N: LongWord; ArrPtr: Pointer; Count: Cardinal): Integer;
function Q_SearchFirstGE_Word (N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;

{ The functions Q_ANDSet_XXX form of two arrays 4-байтных or 2-байтных
  Units of an appropriate type the new array consisting only of those
  Units, which are present at both initial arrays. P1 - address first
  The array, Count1 - number of units in the first array, P2 - address second
  The array, Count2 - number of units in the second array, OutPlace - address of area
  Memories, in which the array - result will be saved. The functions reset
  Quantity of units in the output array. If OutPlace is equal nil, function
  Do not fill the output array, but only calculate quantity of units in it(him);
  Differently, OutPlace should point a storage area, sufficient for storage
  Array - result (the maximum size is equal to a size smaller of initial
  The arrays). The units of the arrays P1 and P2 should be sorted in the order
  Increases.}

function Q_ANDSet_Integer (P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer = nil): Cardinal;
function Q_ANDSet_LongWord (P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer = nil): Cardinal;
function Q_ANDSet_Word (P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer = nil): Cardinal;

{ The functions Q_ORSet_XXX form of two arrays 4-байтных or 2-байтных
  Units of an appropriate type the new array consisting of units,
  Which are present even at one of the initial arrays. P1 - address
  The first array, Count1 - number of units in the first array, P2 - address
  The second array, Count2 - number of units in the second array, OutPlace -
  The address of a storage area, in which will be saved array - result. Functions
  Reset quantity of units in the output array. If OutPlace is equal
  nil, the functions do not fill the output array, but only calculate quantity
  Units in it(him); differently, OutPlace should point a storage area,
  Sufficient for storage of array - result (the maximum size is equal
  To total size of the first and second arrays). Units of the arrays
  P1 And P2 should be sorted in ascending order.}

function Q_ORSet_Integer (P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer = nil): Cardinal;
function Q_ORSet_LongWord (P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer = nil): Cardinal;
function Q_ORSet_Word (P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer = nil): Cardinal;

{ The functions Q_XORSet_XXX form of two arrays 4-байтных or 2-байтных
  Units of an appropriate type the new array consisting of units,
  Which are present at one of the initial arrays, but are absent in the friend.
  P1 - address first array, Count1 - number of units in the first array,
  P2 - address second array, Count2 - number of units in the second array,
  OutPlace - the address of a storage area, in which will be saved array - result.
  The functions reset quantity of units in the output array. If OutPlace
  Equally nil, the functions do not fill the output array, but only calculate
  Quantity of units in it(him); differently, OutPlace should point area
  Memories, sufficient for storage of array - result (maximum size
  Is equal to a total size of the first and second arrays). Units of the arrays
  P1 And P2 should be sorted by increase.}

function Q_XORSet_Integer (P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer = nil): Cardinal;
function Q_XORSet_LongWord (P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer = nil): Cardinal;
function Q_XORSet_Word (P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer = nil): Cardinal;

{ The functions Q_ANDNOTSet_XXX form of two arrays 4-байтных or 2-байтных
  Units of an appropriate type the new array consisting only of those
  Units, which are present at the first initial array, but are absent
  In second. P1 - address first array, Count1 - number of units in first
  The array, P2 - address second array, Count2 - number of units in second
  The array, OutPlace - the address of a storage area, in which will be saved the array
  Result. The functions reset quantity of units in the output array. If
  OutPlace is equal nil, the functions do not fill the output array, but only calculate
  Quantity of units in it(him); differently, OutPlace should point area
  Memories, sufficient for storage of array - result (maximum size
  Is equal to a size of the first initial array). The units of the arrays P1 and P2 owe
  To be sorted by increase.}

function Q_ANDNOTSet_Integer (P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer = nil): Cardinal;
function Q_ANDNOTSet_LongWord (P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer = nil): Cardinal;
function Q_ANDNOTSet_Word (P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer = nil): Cardinal;

{ Q_BitTest32 resets True, if in a double word D bit with number Index
  Is placed(installed) (indexing of bits from zero). Differently, the function resets False.}

function Q_BitTest32 (D: LongWord; Index: Cardinal): Boolean;

{ Q_BitSet32 installs in a double word D bit with an index Index in unit.}

function Q_BitSet32 (D: LongWord; Index: Cardinal): LongWord;

{ Q_BitReset32 dumps(resets) in a double word D bit with an index Index in a zero.}

function Q_BitReset32 (D: LongWord; Index: Cardinal): LongWord;

{ Q_BitToggle32 inverts in a double word D bit with an index Index.}

function Q_BitToggle32 (D: LongWord; Index: Cardinal): LongWord;

{ Q_CountOfSetBits32 resets quantity placed(installed) (single) bits
  In a double word D.}

function Q_CountOfSetBits32 (D: LongWord): Cardinal;

{ Q_CountOfFreeBits32 resets quantity reset (zero) bits
  In a double word D.}

function Q_CountOfFreeBits32 (D: LongWord): Cardinal;

{ Q_SetBitScanForward32 scans a double word D in searches of bit, excellent(different)
  From zero. Function resets index of first placed(installed) bit, since
  Bit FirstBit (from 0 up to 31). If bit is not retrieved, the function resets -1.}

function Q_SetBitScanForward32 (D: LongWord; FirstBit: Integer = 0): Integer;

{ Q_FreeBitScanForward32 scans a double word D in searches of offbit.
  The function resets an index of first reset bit, since bit FirstBit
  (From 0 up to 31). If offbit is not retrieved, the function resets -1.}

function Q_FreeBitScanForward32 (D: LongWord; FirstBit: Integer = 0): Integer;

{ Q_SetBitScanReverse32 scans back double word D in searches of bit,
  Nonzero. The function resets an index of last placed(installed) bit,
  Since bit LastBit (from 0 up to 31). If onbit is not retrieved, function
  Resets -1.}

function Q_SetBitScanReverse32 (D: LongWord; LastBit: Integer = 31): Integer;

{ Q_FreeBitScanReverse32 scans back double word D in searches zero
  Bit. The function resets an index of last reset bit, since bit
  LastBit (from 0 up to 31). If offbit is not retrieved, the function resets -1.}

function Q_FreeBitScanReverse32 (D: LongWord; LastBit: Integer = 31): Integer;

{ Q_BitTest checks bit and resets True, if bit is placed(installed) and False, if
  It(he) is reset. The address of bit string is transmitted by the parameter P. Offset of bit
  Rather beginnings of string is set by the parameter Index. Very first bit of string
  Has offset a zero. The job(definition) of a negative index (offset) is possible.}

function Q_BitTest (P: Pointer; Index: Integer): Boolean;

{ Q_BitSet installs bit and resets True, if before bit already was
  Is placed(installed) and False, if before bit was reset. The address of bit string
  Is transmitted by the parameter P. The offset of bit rather beginnings of string is set
  By the parameter Index. Very first bit of string has offset a zero. Probably
  The job(definition) of a negative index (offset).}

function Q_BitSet (P: Pointer; Index: Integer): Boolean;

{ Q_BitReset dumps(resets) bit and resets True, if before bit was placed(installed)
  And False, if before bit was reset. The address of bit string is transmitted
  By the parameter P. The offset of bit rather beginnings of string is set by the parameter
  Index. The indexing of bits starts with zero. The job(definition) negative is possible
  Index (offset).}

function Q_BitReset (P: Pointer; Index: Integer): Boolean;

{ Q_BitToggle inverts bit and resets True, if earlier bit was placed(installed)
  And False, if bit was reset. The address of bit string is transmitted by the parameter P.
  The offset of bit rather beginnings of string is set by the parameter Index. Indexing
  Bits starts with zero. The job(definition) of a negative index (offset) is possible.}

function Q_BitToggle (P: Pointer; Index: Integer): Boolean;

{ Q_SetBits installs each bit in the array addressed by the parameter P,
  Since bit with index FirstBit (indexing from zero) and finishing bit with
  By index LastBit. A size of the array and his(its) address should be multiple to 4 bytes.}

procedure Q_SetBits (P: Pointer; FirstBit, LastBit: Integer);

{ Q_ResetBits unsets (dumps(resets)) each bit in the array, which address
  Is transmitted parameter P, since bit with index FirstBit (indexing from
  Zero) and finishing bit with an index LastBit. A size of the array and his(its) address owe
  To be multiple to 4 bytes.}

procedure Q_ResetBits (P: Pointer; FirstBit, LastBit: Integer);

{ Q_ToggleBits inverts each bit in the array addressed by the parameter P,
  Since bit with index FirstBit (indexing from zero) and finishing bit with
  By index LastBit. A size of the array and his(its) address should be multiple to 4 bytes.}

procedure Q_ToggleBits (P: Pointer; FirstBit, LastBit: Integer);

{ Q_CountOfSetBits resets number placed(installed) (single) bits in byte
  The array addressed by the parameter P, length L byte.}

function Q_CountOfSetBits (P: Pointer; L: Cardinal): Cardinal;

{ Q_CountOfFreeBits resets number reset (zero) bits in byte
  The array addressed by the parameter P, length L byte.}

function Q_CountOfFreeBits (P: Pointer; L: Cardinal): Cardinal;

{ Q_SetBitScanForward scans a bit map addressed P, in searches of bit,
  Nonzero. The function resets an index of first placed(installed) bit,
  Since bit with an index FirstBit (from zero) and finishing an index LastBit. If
  Onbit in this range is not retrieved, the function resets -1. A size
  The array and his(its) address should be multiple to 4 bytes.}

function Q_SetBitScanForward (P: Pointer; FirstBit, LastBit: Integer): Integer;

{ Q_FreeBitScanForward scans a bit map addressed P, in searches
  Offbit. The function resets an index of first reset bit, starting
  From bit with an index FirstBit (indexing from zero) and finishing an index LastBit.
  If offbit in this range is not retrieved, the function resets -1. A size
  The array and his(its) address should be multiple to 4 bytes.}

function Q_FreeBitScanForward (P: Pointer; FirstBit, LastBit: Integer): Integer;

{ Q_SetBitScanReverse scans back bit map addressed P, in searches
  Bit, nonzero. The function resets an index last placed(installed)
  Bit, since bit with an index LastBit and finishing an index FirstBit. If
  Onbit in this range is not retrieved, the function resets -1. A size
  The array and his(its) address should be multiple to 4 bytes.}

function Q_SetBitScanReverse (P: Pointer; FirstBit, LastBit: Integer): Integer;

{ Q_FreeBitScanReverse scans back bit map addressed P, in searches
  Offbit. The function resets an index of last reset bit, starting
  From bit with an index LastBit and finishing an index FirstBit. If offbit in
  This range is not retrieved, the function resets -1. A size of the array and his(its) address
  Should be multiple to 4 bytes.}

function Q_FreeBitScanReverse (P: Pointer; FirstBit, LastBit: Integer): Integer;

{ Q_NOTByteArr inverts each bit of the array addressed by the parameter P,
  In length L byte.}

procedure Q_NOTByteArr (P: Pointer; L: Cardinal);

{ Q_XORByData fulfils the bit operation " eliminating or " above each byte
  Storage areas addressed by the parameter Source, and appropriate byte
  Storage areas addressed by the parameter Dest. The result is saved in Dest.
  The parameter L sets a size of storage areas in bytes.}

procedure Q_XORByData (Dest, Source: Pointer; L: Cardinal);

{ Q_ANDLongs fulfils logical multiplying above each appropriate bit
  Arrays addressed by parameters Dest and Source, also writes result in
  The array Dest (Dest < - Dest AND Source). In Count the number double is transmitted
  Words in each of the arrays.}

procedure Q_ANDLongs (Dest, Source: Pointer; Count: Cardinal);

{ Q_ORLongs fulfils logical addition above each appropriate bit
  Arrays addressed by parameters Dest and Source, also writes result in
  The array Dest (Dest < - Dest OR Source). In Count the number double is transmitted
  Words in each of the arrays.}

procedure Q_ORLongs (Dest, Source: Pointer; Count: Cardinal);

{ Q_XORLongs fulfils the operation " eliminating or " between appropriate
  In bits of the arrays addressed by parameters Dest and Source, also writes result
  In the array Dest (Dest < - Dest XOR Source). In Count the number double is transmitted
  Words in each of the arrays.}

procedure Q_XORLongs (Dest, Source: Pointer; Count: Cardinal);

{ Q_NOTLongArr inverts each bit of the array addressed by the parameter P,
  In length Count of double words.}

procedure Q_NOTLongArr (P: Pointer; Count: Cardinal);

{ Q_ANDNOTLongs fulfils logical multiplying of each bit of the array,
  Addressed by the parameter Dest on appropriate inverted bit of the array,
  Addressed Source, also writes result in the array Dest
  (Dest < - Dest AND NOT Source). In Count the number of double words is transmitted
  In each of the arrays. As a result of execution of this operation in the array
  Dest all bits, for which appropriate bits in the array Source are unset
  Are placed(installed) in unit.}

procedure Q_ANDNOTLongs (Dest, Source: Pointer; Count: Cardinal);

{ Q_LShift1Longs shifts the array addressed by the parameter P, length Count double
  Words on 1 bit to the left (multiplies it(him) on 2). The low bit of the array is unset.}

procedure Q_LShift1Longs (P: Pointer; Count: Cardinal);

{ Q_RShift1Longs shifts the array addressed by the parameter P, length Count double
  Words into 1 bit to the right (divides it(him) on 2). Most significant bit of the array is unset.}

procedure Q_RShift1Longs (P: Pointer; Count: Cardinal);

{ Q_ReverseBits pays a bit map addressed by the parameter P, so, that
  The first bits become last, and last - first. Length of the array
  (In bits) is transmitted by the parameter BitCount. If this length is not multiple to eight
  To bits, the bits, remaining up to a multiplicity, are filled in zero&.}

procedure Q_ReverseBits (P: Pointer; BitCount: Cardinal);

{ Q_Lrot32 cyclic shifts a double word D on Shift bit to the left.}

function Q_Lrot32 (D: LongWord; Shift: Byte): LongWord;

{ Q_Rrot32 cyclic shifts a double word D on Shift bit to the right.}

function Q_Rrot32 (D: LongWord; Shift: Byte): LongWord;

{ Q_Lrot16 cyclic shifts a machine word W on Shift bit to the left.}

function Q_Lrot16 (W: Word; Shift: Byte): Word;

{ Q_Rrot16 cyclic shifts a machine word W on Shift bit to the right.}

function Q_Rrot16 (W: Word; Shift: Byte): Word;

{ Q_Lrot8 cyclic shifts byte B on Shift bit to the left.}

function Q_Lrot8 (B, Shift: Byte): Byte;

{ Q_Rrot8 cyclic shifts byte B on Shift bit to the right.}

function Q_Rrot8 (B, Shift: Byte): Byte;

{ Q_RotateLongsLeft cyclic shifts each array cell consisting from
  Count of units such as LongWord, addressed by the parameter P, on Shift bit to the left.}

procedure Q_RotateLongsLeft (P: Pointer; Count: Cardinal; Shift: Byte);

{ Q_RotateLongsRight cyclic shifts each array cell consisting from
  Count of units such as LongWord, addressed by the parameter P, on Shift bit to the right.}

procedure Q_RotateLongsRight (P: Pointer; Count: Cardinal; Shift: Byte);

{ Q_RotateWordsLeft cyclic shifts each array cell consisting from
  Count of units of a type Word, addressed by the parameter P, on Shift bit to the left.}

procedure Q_RotateWordsLeft (P: Pointer; Count: Cardinal; Shift: Byte);

{ Q_RotateWordsRight cyclic shifts each array cell consisting from
  Count of units of a type Word, addressed by the parameter P, on Shift bit to the right.}

procedure Q_RotateWordsRight (P: Pointer; Count: Cardinal; Shift: Byte);

{ Q_RotateBytesLeft cyclic shifts each byte in the memory by a size L
  Byte addressed by the parameter P, on Shift bit to the left.}

procedure Q_RotateBytesLeft (P: Pointer; L: Cardinal; Shift: Byte);

{ Q_RotateBytesRight cyclic shifts each byte in the memory by a size L
  Byte addressed by the parameter P, on Shift bit to the right.}

procedure Q_RotateBytesRight (P: Pointer; L: Cardinal; Shift: Byte);

{ Q_RotateBitsLeft cyclic shifts bit string of length L byte, address
  To which is transmitted by the parameter P, on Shift bit to the left. The parameter Shift can
  To receive both positive, and negative values. Bit string,
  As it is accepted, is written, since most significant bits.}

procedure Q_RotateBitsLeft (P: Pointer; L: Cardinal; Shift: Integer);

{ Q_RotateBitsRight cyclic shifts bit string of length L byte, address
  To which is transmitted by the parameter P, on Shift bit to the right. The parameter Shift can
  To receive both positive, and negative values. Bit string
  Is written, since most significant bits.}

procedure Q_RotateBitsRight (P: Pointer; L: Cardinal; Shift: Integer);


{ Cryptography functions.}

{ Before to use any functions from this units, familiarize
  With the legislation of your country in that part, which concerns usage
  Algorithms of proof encoding!!! If it is not authorized by your laws,
  Delete all functions from this units from your copy of the unit QStrings.}

{ Q_XORByChar fulfils the bit operation "exclusive or" above each byte
  Storage areas of length L byte addressed by the parameter P, and character (byte)
  Ch. The repeated application of this operation with the same character Ch restores
  Reset state of the byte array.}

procedure Q_XORByChar (P: Pointer; L: Cardinal; Ch: Char); overload;
procedure Q_XORByChar (P: Pointer; L: Cardinal; Ch: Byte); overload;

{ Q_XORByLong is similar Q_XORByChar, but works not with bytes, and with double
  By words (LongWord). Count sets quantity четырехбайтных of double words.}

procedure Q_XORByLong (P: Pointer; Count: Cardinal; D: LongWord);

{ Q_XORByWord is similar Q_XORByChar, but works not with bytes, and with machine
  By words (Word). Count sets quantity двухбайтных of words.}

procedure Q_XORByWord (P: Pointer; Count: Cardinal; W: Word);

{ Q_XORByStr fulfils the bit operation " eliminating or " above each byte
  Storage areas of length L byte addressed by the parameter P, and each character
  Strings Key. If the size of a storage area exceeds length of string Key, the ambassador
  Sampling of the last character from this string she(it) starts to be viewed with
  Beginnings. Repeated application of this operation to a storage area with the same key
  Restores a reset state of a storage area.}

procedure Q_XORByStr (P: Pointer; L: Cardinal; const Key: string);

{ Q_XORByRandom fulfils гаммирование (with the help XOR) byte array of length
  L byte addressed by the parameter P, sensor of pseudo-random numbers similar
  To standard (Random). Initial value of a pseudo-random sequence
  Is defined(determined) by a constant Seed. Repeated application of this operation to same
  The storage areas with same Seed are restored by(with) a reset state of the data.}

procedure Q_XORByRandom (P: Pointer; L: Cardinal; Seed: LongWord);

{ The functions Q_RC4XXX realize line algorithm of encoding RC4, developed
  Роном Ривестом (all rights belong RSA Data Security, Inc.), which was
  Is anonymously published in computer conference sci.crypt in 1995. Algorithm
  Very fast and reliable enough. Repeated application it(him) with same
  By key restores a reset state of the data. It is not necessary to use
  The same key some times. If it is necessary to you fast to cipher
  Any data, generate a pseudo-random session key K1 (for example,
  With the help Q_SecureRandFill) size from 16 up to 256 bytes, cipher it(him)
  Method CAST6 or RC6 in mode CBC with some key K2 also place in
  Output stream of the data, then with the help of a session key K1 cipher
  The data by a method RC4. The key K2 for encoding a session key can vary
  Not too frequently. At data reading you first of all read out ciphered
  Session key also decrypt it(him) with the help of a key, known (you,) K2
  (By procedures Q_CAST6XXX or Q_RC6XXX), receive a session key K1, read
  Data, decrypt them with a key K1 (RC4 method).}

type
  TRC4ID = type Pointer;

{ Q_RC4Init sets a key sequence for data encoding by a method
  RC4 Also initializes the process of encoding. As a key is received
  Contents of the byte array addressed by the parameter Key, length L byte.
  The key length can be up to 256 characters. In the parameter ID is reset
  Identifier of the given process of encoding, which then is transmitted in
  Procedures Q_RC4Apply and Q_RC4Done. Each identifier owes in the end
  To be released by call Q_RC4Done. Since the version QStrings 5.10 methods
  RC4 Is realized as it is necessary (key is viewed from the first bytes).}

procedure Q_RC4Init (var ID: TRC4ID; Key: Pointer; KeyLen: Cardinal);

{ Q_RC4Apply fulfils actual encoding or decryption of the data addressed
  By the parameter P, length L byte by a key coupled to the identifier ID. This
  The procedure can be called(caused) multiply for sequential encoding
  Several data arrays. Thus Q_RC4Init and Q_RC4Done are called(caused) only
  Once, in the beginning and at the end of all process.}

procedure Q_RC4Apply (ID: TRC4ID; P: Pointer; L: Cardinal);

{ Q_RC4Done releases internal operational lifes coupled to the identifier ID.
  After call Q_RC4Done this identifier can not be used any more
  By the procedure Q_RC4Apply.}

procedure Q_RC4Done (ID: TRC4ID);

{ Q_RC4SelfTest checks service capability of algorithm RC4 on test vectors,
  Given in the function RC4SelfTest from the unit RC4.pas (writer of this unit
  Is David Barton (davebarton@bigfoot.com)). If the test пройден is successful,
  The function resets True, differently False.}

function Q_RC4SelfTest: Boolean;

{ The procedures Q_RC6XXX realize block algorithm of encoding RC6 (TM). The writers
  Algorithm: Ronald L. Rivest, M.J.B. Robshaw, R. Sidney and Y.L. Yin (algorithm
  Is patented). Unique feature of this cipher is his(its) high
  Stability(resistance) at excellent(different) productivity (one of five algorithms, past
  In the second round AES). A key length - up to 255 bytes, however to use keys
  More longly than 48 bytes it is not recommended (normal key length - 32 bytes). Here
  The modes CBC, CFB and OFB are realized. }

type
  TRC6InitVector = array [0.. 15] of Byte;
  TRC6ID = type Pointer;

{ Q_RC6Init initializes encoding by a method RC6. The key is transmitted by the parameter
  Key (address of the byte array), and key length (in bytes) - parameter KeyLen.
  The identifier of the current process of encoding is reset in the parameter ID.
  The given identifier is transmitted as the first parameter in all other procedures
  Q_RC6XXX, and in the end it(he) should be released by call Q_RC6Done.}

procedure Q_RC6Init (var ID: TRC6ID; Key: Pointer; KeyLen: Cardinal);

{ The procedures of encoding / decryption in the mode ECB should be used only
  In the debug purposes. A size of a data package - 128 bits.}

procedure IntRC6EncryptECB (ID: TRC6ID; P: Pointer);
procedure IntRC6DecryptECB (ID: TRC6ID; P: Pointer);

{ Q_RC6SetOrdinaryVector unsets an initial vector used in modes
  CBC, CFB and OFB, and then ciphers by his(its) current key. Thus passes
  Necessity in синхропосылке. ID - identifier of the process of encoding,
  Obtained from Q_RC6Init. This procedure should be called(caused) before everyone
  By session of encoding (decryption). The idea of the procedure is borrowed from the unit
  RC6.pas In a composition DCPcrypt v2, which writer is David Barton
  (davebarton@bigfoot.com). }

procedure Q_RC6SetOrdinaryVector (ID: TRC6ID);

{ Q_RC6SetInitVector installs an initial vector used in modes
  CBC, CFB and OFB, equal to a vector transferred(handed) by the parameter IV. ID - the identifier
  The process of encoding obtained from Q_RC6Init. This procedure should be called(caused)
  Before each session of encoding (or decryption).}

procedure Q_RC6SetInitVector (ID: TRC6ID; const IV: TRC6InitVector);

{ Q_RC6EncryptCBC fulfils encoding the byte array addressed
  By the parameter P, length L byte by a method RC6 in the mode CBC. ID - identifier
  The process of encoding obtained from Q_RC6Init. If length of the byte array
  Is not multiple to 128 bits (16 characters), the tail is ciphered in the mode CFB.}

procedure Q_RC6EncryptCBC (ID: TRC6ID; P: Pointer; L: Cardinal);

{ Q_RC6DecryptCBC fulfils decryption of the byte array addressed
  By the parameter P, length L byte ciphered by the procedure Q_RC6EncryptCBC.
  ID - identifier of the process of encoding obtained from Q_RC6Init.}

procedure Q_RC6DecryptCBC (ID: TRC6ID; P: Pointer; L: Cardinal);

{ Q_RC6EncryptCFB128 fulfils encoding the byte array addressed
  By the parameter P, length L byte by a method RC6 in a block mode CFB (128-bit).
  ID - identifier of the process of encoding obtained from Q_RC6Init.}

procedure Q_RC6EncryptCFB128 (ID: TRC6ID; P: Pointer; L: Cardinal);

{ Q_RC6DecryptCFB128 fulfils decryption of the byte array addressed
  By the parameter P, length L byte ciphered by the procedure Q_RC6EncryptCFB128.
  ID - identifier of the process of encoding obtained from Q_RC6Init.}

procedure Q_RC6DecryptCFB128 (ID: TRC6ID; P: Pointer; L: Cardinal);

{ Q_RC6EncryptCFB fulfils encoding the byte array addressed
  By the parameter P, length L byte by a method RC6 in the mode CFB (8-bit). ID -
  The identifier of the process of encoding obtained from Q_RC6Init.}

procedure Q_RC6EncryptCFB (ID: TRC6ID; P: Pointer; L: Cardinal);

{ Q_RC6DecryptCFB fulfils decryption of the byte array addressed
  By the parameter P, length L byte ciphered by the procedure Q_RC6EncryptCFB.
  ID - identifier of the process of encoding obtained from Q_RC6Init.}

procedure Q_RC6DecryptCFB (ID: TRC6ID; P: Pointer; L: Cardinal);

{ Q_RC6ApplyOFB128 fulfils encoding or decryption of a storage area,
  Addressed by the parameter P, length L byte by a method RC6 in a block mode OFB
  (128 bits). The parameter ID sets the identifier of the process of encoding. Follows
  To mean, that the mode OFB does not provide distribution of errors and,
  Actually, is an equivalent of the line cipher. In this mode does not follow
  To use the same key some times. The block mode OFB follows
  To apply with extra care, as, if for someone becomes known
  Fragment of a plain text, it(he) easily can decrypt all subsequent
  The text. If the size of a storage area is not multiple to 16 bytes, a fragment of a gamma,
  Come on bytes, missing up to a multiplicity, is skipped.}

procedure Q_RC6ApplyOFB128 (ID: TRC6ID; P: Pointer; L: Cardinal);

{ Q_RC6ApplyOFB fulfils encoding or decryption of a storage area addressed
  By the parameter P, length L byte by a method RC6 in the mode OFB (8 bits). The parameter ID
  Sets the identifier of the process of encoding.}

procedure Q_RC6ApplyOFB (ID: TRC6ID; P: Pointer; L: Cardinal);

{ Q_RC6Done releases identifier of the process of encoding ID, obtained from
  Procedures Q_RC6Init and all operational lifes, coupled to it(him).}

procedure Q_RC6Done (ID: TRC6ID);

{ Q_RC6SelfTest checks service capability of algorithm RC6 on test vectors,
  Given in the document: The RC6 (TM) Block Cipher. Ronald L. Rivest, M.J.B.
  Robshaw, R. Sidney, and Y.L. Yin, http://theory.lcs.mit.edu/~rivest/rc6.pdf.
  If the test passed successfully, the function returns True, otherwise it
  returns False.}

function Q_RC6SelfTest: Boolean;

{ The procedures Q_CAST6XXX realize block algorithm of encoding CAST6-256. It(him)
  The writers are Carlisle Adams and Jeff Gilchrist (Canada). This algorithm
  Is extremely reliable, but works in 2.5 times more slowly, than RC6.
  Key length - up to 256 bits (32 bytes). The modes CBC, CFB and OFB are maintained.
  This implementation of algorithm completely corresponds(meets) RFC 2612.}

type
  TCAST6InitVector = array [0.. 15] of Byte;
  TCASTID = type Pointer;

{ Q_CAST6Init initializes encoding by a method CAST6. The key is transmitted a couple
  In meter Key (address of the byte array), and the key length (in bytes) is set
  By the parameter KeyLen. The identifier of the current process of encoding is reset
  In the parameter ID. The given identifier is transmitted as the first parameter in all
  Other procedures Q_CAST6XXX, and in the end it(he) should be released by call
  Q_CAST6Done.}

procedure Q_CAST6Init (var ID: TCASTID; Key: Pointer; KeyLen: Cardinal);

{ The procedures of encoding / decryption in the mode ECB should be used only
  In the debug purposes. A size of a data package - 128 bits.}

procedure IntCAST6EncryptECB (ID: TCASTID; P: Pointer);
procedure IntCAST6DecryptECB (ID: TCASTID; P: Pointer);

{ Q_CAST6SetOrdinaryVector unsets initial vector, which is required in
  Modes CBC, CFB and OFB, and then ciphers by his(its) current key. Thus passes
  Necessity in синхропосылке. ID - identifier of the process of encoding,
  Obtained from Q_CAST6Init. This procedure should be called(caused) before each session
  Encodings (or decryption). The idea of the procedure is borrowed from the unit
  Cast256.pas In a composition DCPcrypt v2, which writer is David Barton
  (davebarton@bigfoot.com). }

procedure Q_CAST6SetOrdinaryVector (ID: TCASTID);

{ Q_CAST6SetInitVector installs an initial vector used in modes
  CBC, CFB and OFB, equal to a vector IV. ID - identifier of the process of encoding,
  Obtained from Q_CAST6Init. This procedure should be called(caused) before everyone
  By session of encoding (or decryption).}

procedure Q_CAST6SetInitVector (ID: TCASTID; const IV: TCAST6InitVector);

{ Q_CAST6EncryptCBC fulfils encoding the byte array addressed
  By the parameter P, length L byte by a method CAST6 in the mode CBC. ID - identifier
  The process of encoding obtained from Q_CAST6Init. If length of the byte array
  Is not multiple to 128 bits (16 characters), the tail is ciphered in the mode CFB.}

procedure Q_CAST6EncryptCBC (ID: TCASTID; P: Pointer; L: Cardinal);

{ Q_CAST6DecryptCBC fulfils decryption of the byte array addressed
  By the parameter P, length L byte ciphered by the procedure Q_CAST6EncryptCBC.
  ID - identifier of the process of encoding obtained from Q_CAST6Init.}

procedure Q_CAST6DecryptCBC (ID: TCASTID; P: Pointer; L: Cardinal);

{ Q_CAST6EncryptCFB128 fulfils encoding the byte array addressed
  By the parameter P, length L byte by a method CAST6 in a block mode CFB (128-bit).
  ID - identifier of the process of encoding obtained from Q_CAST6Init.}

procedure Q_CAST6EncryptCFB128 (ID: TCASTID; P: Pointer; L: Cardinal);

{ Q_CAST6DecryptCFB128 fulfils decryption of the byte array addressed
  By the parameter P, length L byte ciphered by the procedure Q_CAST6EncryptCFB128.
  ID - identifier of the process of encoding obtained from Q_CAST6Init.}

procedure Q_CAST6DecryptCFB128 (ID: TCASTID; P: Pointer; L: Cardinal);

{ Q_CAST6EncryptCFB fulfils encoding the byte array addressed
  By the parameter P, length L byte by a method CAST6 in the mode CFB (8-bit). ID -
  The identifier of the process of encoding obtained from Q_CAST6Init.}

procedure Q_CAST6EncryptCFB (ID: TCASTID; P: Pointer; L: Cardinal);

{ Q_CAST6DecryptCFB fulfils decryption of the byte array addressed
  By the parameter P, length L byte ciphered by the procedure Q_CAST6EncryptCFB.
  ID - identifier of the process of encoding obtained from Q_CAST6Init.}

procedure Q_CAST6DecryptCFB (ID: TCASTID; P: Pointer; L: Cardinal);

{ Q_CAST6ApplyOFB128 fulfils encoding or decryption of a storage area,
  Addressed by the parameter P, length L byte by a method CAST6 in a block mode OFB
  (128 bits). The parameter ID sets the identifier of the process of encoding. Follows
  To mean, that the mode OFB does not provide distribution of errors and,
  Actually, is an equivalent of the line cipher. In this mode does not follow
  To use the same key some times. The block mode OFB follows
  To apply with extra care, as, if for someone becomes known
  Fragment of a plain text, it(he) easily can decrypt all subsequent
  The text. If the size of a storage area is not multiple to 16 bytes, a fragment of a gamma,
  Come on bytes, missing up to a multiplicity, is skipped.}

procedure Q_CAST6ApplyOFB128 (ID: TCASTID; P: Pointer; L: Cardinal);

{ Q_CAST6ApplyOFB fulfils encoding or decryption of a storage area, which
  Is addressed by the parameter P, length L byte by a method CAST6 in the mode OFB (8 bits).
  ID - identifier of the process of encoding.}

procedure Q_CAST6ApplyOFB (ID: TCASTID; P: Pointer; L: Cardinal);

{ Q_CAST6Done releases identifier of the process of encoding ID, obtained from
  Procedures Q_CAST6Init and all operational lifes, coupled to it(him).}

procedure Q_CAST6Done (ID: TCASTID);

{ Q_CAST6SelfTest checks service capability of algorithm CAST6 on test
  To vectors given in RFC 2612. If the test пройден is successful, function
  Resets True, differently False.}

function Q_CAST6SelfTest: Boolean;

{ The functions Q_SHA1XXX are intended for obtaining value of hash-function SHA-1
  (дайджеста) for the byte array or string of characters. This algorithm (Secure
  Hash Standard-1) is developed NIST (National Institute of Standards and
  Technology) also is published in FIPS180-1 (Federal Information Processing
  Standards Publication 180-1). The functions Q_SHA1XXX generate the 160-bit array,
  Consisting from five double words (or 20 bytes). On дайджесту practically
  It is impossible to restore the initial message. It is impossible also to find another
  The message with same digest. The given implementation of algorithm SHA-1 is fulfilled
  On the basis of the unit SHA1.pas, which writer is David Barton.}

type
  TSHA1Digest = array [0.. 4] of LongWord;
  TSHAID = type Pointer;

  TMixDigest = array [0.. 9] of LongWord; // see Q_MixHashXXX (below)
  TMixID = type Pointer;

{ Q_SHA1Init initializes hash-function SHA-1. In the parameter ID is reset
  The new identifier дайджеста. In further this identifier is transmitted in
  Procedures Q_SHA1Update and Q_SHA1Final. Last of these procedures it is necessary
  Should be called(caused) in the end for release of the occupied operational lifes.}

procedure Q_SHA1Init (var ID: TSHAID);

{ Q_SHA1Update adds to дайджесту, which identifier is transferred(handed) by the parameter
  ID, information on the byte array of length L byte addressed by the parameter P. This
  The procedure can is called(caused) multiply for several data arrays, however
  Value дайджеста will be obtained only at closing the identifier
  By the procedure Q_SHA1Final.}

procedure Q_SHA1Update (ID: TSHAID; P: Pointer; L: Cardinal);

{ Q_SHA1Final resets in parameter Digest value of hash-function SHA-1 for
  The indicated identifier ID. Thus the operational lifes coupled with are released
  By this identifier. After call Q_SHA1Final to use given ID
  Already it is impossible.}

procedure Q_SHA1Final (ID: TSHAID; var Digest: TSHA1Digest);

{ Q_SHA1 resets in the parameter Digest value of hash-function SHA-1 for string
  Characters S, storage area of length L byte addressed by the parameter P, or
  Some reference value SourceDigest 160-bit or 320-bit
  Hash-functions (SHA-1 or MixHash).}

procedure Q_SHA1 (const S: string; var Digest: TSHA1Digest); overload;
procedure Q_SHA1 (P: Pointer; L: Cardinal; var Digest: TSHA1Digest); overload;

procedure Q_SHA1 (const SourceDigest: TSHA1Digest; var Digest: TSHA1Digest); overload;
procedure Q_SHA1 (const SourceDigest: TMixDigest; var Digest: TSHA1Digest); overload;

{ Q_SHA1SelfTest checks implementation of algorithm SHA-1 on test vectors,
  Published NIST. If the test пройден is successful, the function resets True,
  Differently False.}

function Q_SHA1SelfTest: Boolean;

{ The functions Q_MixHashXXX realize algorithm of hash-function with twice greater, than
  For SHA-1 in length of a signature (320 bits): the initial string (or byte array)
  Is divided into three approximately equal parts, for generation of a signature рассчи-
  тываются three 160-bit values of the one-sided function SHA-1: the first value
  The second value - on second and is calculated by the first and second parts of string
  To the third parts, and third - by the third and first parts. First two values
  Hash-functions SHA-1 are consolidated with each other by concatenation, derivating 320-
  Bit string. Third value is used as key of encoding for
  Modifications of the obtained bit string. This modification is fulfilled with the help
  Four-feed-through encoding with cyclic distribution of errors on a basis
  Algorithm CAST6-160. The four-feed-through encoding is necessary to reach(achieve)
  Complete distribution of errors both in one, and in other side. Thus
  Each bit participates in eight cycles of encoding. At the end of the process bit
  String covered by a pseudo-random sequence obtained on a basis
  First two values of the function SHA-1, also increases (on double words) to
  To reference value of hash-function. A disadvantage of given hash-function is
  That she(it) works approximately at 2.25 of time more slowly, than SHA-1.}

{ Q_MixHashInit initializes hash-function. In the parameter ID is reset
  The new identifier дайджеста. In further this identifier is transmitted
  In procedures Q_MixHashUpdate and Q_MixHashFinal. Last of these procedures
  Necessarily should be called(caused) in the end for release of the occupied operational lifes.}

procedure Q_MixHashInit (var ID: TMixID);

{ Q_MixHashUpdate adds to дайджесту, which identifier is transferred(handed)
  By the parameter ID, information on the byte array of length L byte addressed
  By the parameter P. This procedure can is called(caused) multiply for several
  Data arrays, however digest will be obtained only at closing the identifier
  by the procedure Q_MixHashFinal.}

procedure Q_MixHashUpdate (ID: TMixID; P: Pointer; L: Cardinal);

{ Q_MixHashFinal resets in parameter Digest value of hash-function for
  The indicated identifier ID. Thus the operational lifes coupled with are released
  By this identifier. After call Q_MixHashFinal to use given ID
  Already it is impossible.}

procedure Q_MixHashFinal (ID: TMixID; var Digest: TMixDigest);

{ Q_MixHash resets in the parameter Digest value of hash-function for string
  Characters S, storage area of length L byte addressed by the parameter P, or
  Some reference value SourceDigest of given hash-function.}

procedure Q_MixHash (const S: string; var Digest: TMixDigest); overload;
procedure Q_MixHash (P: Pointer; L: Cardinal; var Digest: TMixDigest); overload;
procedure Q_MixHash (const SourceDigest: TMixDigest; var Digest: TMixDigest); overload;

{ Q_MixHashSelfTest checks implementation of algorithm on test vectors,
  If the test пройден is successful, the function resets True, differently False.}

function Q_MixHashSelfTest: Boolean;

{ The procedures Q_RandXXX realize the sensor of pseudo-random numbers Mersenne Twister
  (unsigned integers in an interval from 0 up to 2^32 - 1) with the uniform law
  Allocations. His(its) writers: Makoto Matsumoto (matumoto@math.keio.ac.jp) and
  Takuji Nishimura. This sensor is developed on the basis of several existing
  Generators ПСП also has incorporated their best qualities. His(its) period constitutes
  2^19937 - 1. In itself sensor is not intended for the cryptography purposes,
  Since on sampling the large size suffices it is possible to restore initial
  Sequence. However, in the represented here implementation to obtained
  To values of the sensor can be applied one-sided (not inverted) hash-
  The function SHA-1. In this case, if the sensor was correctly initialized
  (Procedures Q_RandInit and Q_RandXXXUpdate), his(its) output sequence
  Will be криптографически a rack. More complete information on this sensor ПСП
  It is possible to receive on a site: http://www.math.keio.ac.jp/~matumoto/emt.html }

type
  TRandVector = array [0.. 623] of LongWord;
  TMTID = type Pointer;

{ Q_RandInit initializes the sensor ПСП. The identifier of the sensor is reset
  In the parameter ID. It(he) should be transmitted as the first parameter by call others
  Functions Q_RandXXX. For initialization is used беззнаковое an integer
  Seed, unequal zero, or initial vector InitVector. The identifier owes
  To be released at the end of operation with the sensor with the help of call Q_RandDone.}

procedure Q_RandInit (var ID: TMTID; Seed: LongWord = 4357); overload;
procedure Q_RandInit (var ID: TMTID; const InitVector: TRandVector); overload;

{ The procedures MT_RandXXXUpdate change an internal state (vector) sensor
  Pseudo-random numbers. The current value of a vector is ciphered with the help one
  From algorithms: CAST-256 or RC6. The second parameter of procedures Q_RandXXXUpdate
  Is used as a key of encoding. It(he) can be string of characters S, byte
  By the array addressed P, length L byte, or digital signature (SHA-1 or
  MixHash), transmitted the parameter Digest. If length of string S, or array,
  Addressed parameter P, is more 32 bytes (in case CAST6) or 48 bytes (in
  Case RC6), the encoding is fulfilled some times, using fragments of a key
  In length up to 32 bytes or up to 48 bytes, accordingly. At creation of string S
  Use value of the counter of clock ticks of the processor (Q_TimeStamp), current date
  And time, identifiers of the user and system, arbitrary string of characters,
  Entered by the user or manager from the keyboard etc. In finite
  The score, the stability(resistance) of the sensor is defined(determined) by , as far as
  unpredictable and the initializing byte sequence transmitted will be long
  In Q_RandXXXUpdate. These procedures can be called(caused) in various combinations.}

procedure Q_RandCAST6Update (ID: TMTID; const S: string); overload;
procedure Q_RandCAST6Update (ID: TMTID; P: Pointer; L: Cardinal); overload;
procedure Q_RandCAST6Update (ID: TMTID; const Digest: TSHA1Digest); overload;

procedure Q_RandRC6Update (ID: TMTID; const S: string); overload;
procedure Q_RandRC6Update (ID: TMTID; P: Pointer; L: Cardinal); overload;
procedure Q_RandRC6Update (ID: TMTID; const Digest: TSHA1Digest); overload;
procedure Q_RandRC6Update (ID: TMTID; const Digest: TMixDigest); overload;

{ Q_RandGetVector copies current contents of a vector used for
  Generation of pseudo-random numbers, in the array transferred(handed) by the parameter Vector.
  Thus, in the further generator it is possible to initialize by this vector
  And to play back sequence of pseudo-random numbers (since
  The following number). In the parameter ID the identifier of the sensor is transmitted.}

procedure Q_RandGetVector (ID: TMTID; var Vector: TRandVector);

{ Q_RandSetVector sets new value of a vector used for generation
  Pseudo-random numbers. Allows to play back a sequence, starting
  From a vector transferred(handed) by the parameter Vector. ID - the identifier of the sensor.}

procedure Q_RandSetVector (ID: TMTID; const Vector: TRandVector);

{ MT_RandNext resets the following pseudo-random number, which is
  Evenly distributed in an interval from 0 up to $FFFFFFFF, for the sensor,
  Which identifier is transmitted by the parameter ID. Values of this function
  Are not cryptographically strong.}

function Q_RandNext (ID: TMTID): LongWord;

{ Q_RandUniform resets the pseudo-random value evenly distributed
  In an interval [0,1], for the sensor, which identifier is transferred(handed) by the parameter ID.
  The continuous pseudo-random value with uniform allocation in an interval
  [Min,Max] it turns out under the formula: (Max-Min)*Q_RandUniform(ID)+Min. }

function Q_RandUniform (ID: TMTID): Extended;

{ Q_RandUInt32 resets whole pseudo-random number (0 < = X < Range) for
  The sensor, which identifier is transferred(handed) by the parameter ID. The function Q_RandUInt64
  Is similar Q_RandUInt32, but the valid range is set by 64-bit number.
  Q_RandUInt64 Works much more slowly, than Q_RandUInt32.}

function Q_RandUInt32 (ID: TMTID; Range: Cardinal): Cardinal;
function Q_RandUInt64 (ID: TMTID; Range: Int64): Int64;

{ Q_RandGauss is used for simulation of a continuous random variable
  With the normal law of allocation. This function resets (0,1) normal&
  The distributed pseudo-random number for the sensor, which identifier
  Is transmitted by the parameter ID. In the parameter ExtraNumber the address can be transferred(handed)
  The variable such as Double, in which will write other number with similar
  By allocation. It is possible, as used algorithm (Marsaglia-Bray)
  Generates at once pair of numbers with normal allocation. That
  To receive the value with expectation Mean and среднеквадратическим
  By deviation StdDev, use the formula: Q_RandGauss(ID)*StdDev+Mean.}

function Q_RandGauss (ID: TMTID; ExtraNumber: Pointer = nil): Extended;

{ MT_SecureRandNext resets the following pseudo-random number, which
  Is evenly distributed in interval from 0 up to $FFFFFFFF, for
  The sensor, which identifier is transmitted by the parameter ID. This value
  It turns out as a result of application of the one-sided function (SHA-1) to output
  Sequences of the sensor Mersenne Twister. Thus everyone 5 values
  The functions MT_SecureRandNext are formed on the basis of 14 values of the function
  Q_RandNext. At usage of this sensor in a subsystem of safety
  The special notice is necessary to give initialization of the sensor.}

function Q_SecureRandNext (ID: TMTID): LongWord;

{ Q_RandFill fills a storage area addressed by the parameter P, length L byte
  By pseudo-random numbers with the uniform law of allocation. ID -
  The identifier of the sensor obtained by call Q_RandInit. Received by such
  By image the sequence is not криптографически a rack.}

procedure Q_RandFill (ID: TMTID; P: Pointer; L: Cardinal);

{ MT_SecureRandFill fills a storage area addressed by the parameter P in length
  L byte криптографически by a rack (at correct initialization of the sensor)
  By sequence of pseudo-random numbers on the basis of the sensor, identifier
  Which is transmitted by the parameter ID.}

procedure Q_SecureRandFill (ID: TMTID; P: Pointer; L: Cardinal);

{ Q_SecureRandXOR fulfils overlay with the help " eliminating OR " by a rack
  Pseudo-random sequence on a storage area addressed by the parameter
  P, length L byte. The identifier of the sensor is transmitted by the parameter ID. Repeated
  The overlay of the same sequence restores a reset state
  Storage areas. If this operation is applied to several fragments of memory,
  That at data recovery sizes of these fragments should coincide with
  Initial, or length of all fragments should be multiple to four bytes.
  It is coupled that, if Q_SecureRandXOR is applied to a storage area,
  Which size is not multiple to four bytes, a fragment of a gammas come
  On bytes, missing up to a multiplicity, is lost. The procedure Q_SecureRandXOR,
  Actually, realizes self algorithm of stream encryption, which at
  Appropriate initialization of the generator can considerably exceed
  On stability(resistance) such methods as RC6 or CAST6. However, for this purpose is necessary
  Source of a long pseudo-random sequence (for example, another
  It is impossible to admit the similar generator) and, besides, that any
  The fragment of a gamma was repeatedly superimposed on other data.}

procedure Q_SecureRandXOR (ID: TMTID; P: Pointer; L: Cardinal);

{ Q_RandDone releases operational lifes coupled c by the identifier of the sensor
  Pseudo-random numbers ID. After call of this procedure given ID it is more
  Can not be used.}

procedure Q_RandDone (ID: TMTID);

{ The procedures Q_DHXXX realize algorithm of the Diffie-Hellman, which is used
  In encoding with an open key. These procedures do not fulfil directly
  Encodings of the information. They are intended for generation of a key, which can
  Then to be used for encoding by usual methods. This key is formed
  With the help of a couple of keys: closed and open. The closed key, as a rule
  Is generated by the pseudo-random generator, and the open key forms on the basis of closed.
  At calculation of an open key the integer G (erected is used still
  In a degree pseudo-random number), which size corresponds(meets) to a size
  Keys. It should be identical to both participants of information exchange
  And to be nonzero and unit. Main idea of algorithm of the Diffie-Hellman
  Consists in that the same private key, with which are ciphered
  The data, can be obtained as from the closed key and another's open,
  And from the open key and another's closed. Thus, everyone
  The participant of information exchange should own the personal closed key and open
  By keys of all other participants. Then for each couple will be confidential
  Key, and, not knowing their closed keys, nobody can decrypt or
  To forge the messages, which these two participants interchange. It is possible also
  To prepare the message for the concrete subject (holder of the closed key),
  Using his(its) open key, so that anybody, except for this subject, could not
  To read the given message (even the remailer). That it to make,
  It is necessary to generate the temporary closed key, to encipher a message with the help
  The given key and open key of the receiver to generate temporary open
  Key (on the basis of closed) and to enclose it(him) on the message, then temporary
  The closed key to delete. A method of the Diffie-Hellman on the basis here is realized
  2^4253-1. This prime number such as Mersenne. On stability(resistance) the given implementation
  Corresponds(meets) approximately to 226 bits for the symmetric ciphers. Have in
  To sort, that the generation open or private key can borrow(occupy) a little
  Seconds. The most simple way of modular exponentiation is used.}

type
  PDHKey4253 = ^TDHKey4253;
  TDHKey4253 = array [0.. 132] of LongWord;

{ Q_DHCreatePublicKey form an open key PublicKey on the basis of closed
  Key PrivateKey and constant G. Memory under open key is arranged up to
  Call of this procedure (the one who calls(causes) her(it)).}

procedure Q_DHCreatePublicKey (G, PrivateKey: PDHKey4253; PublicKey: PDHKey4253);

{ Q_DHGetCipherKey generate a main key by a size KeySize byte, which
  Then is used for encoding or decryption of the data (with the help anyone
  Symmetric method), on the basis of an open key PublicKey (another's) and
  private key PrivateKey (one's). The obtained key is saved in area
  Memories pointed by the parameter CipherKey.}

procedure Q_DHGetCipherKey (PublicKey, PrivateKey: PDHKey4253;
  CipherKey: Pointer; KeySize: Integer);

{ Q_DHSelfTest checks service capability of procedures realizing a method Diffie-Hellman
  also returns True, if all in the order, differently - False. On the computer
  With the processor PII-233 this function is fulfilled approximately for 8.5 seconds.}

function Q_DHSelfTest: Boolean;

{ Useful standard string functions.}

{ StringOfChar resets string consisting from Count of characters Ch. For example,
   StringOfChar ('A', 10) will return string 'AAAAAAAAAA'. (unit System)

function StringOfChar (Ch: Char; Count: Integer): string;}

{ SetString installs S so that she(it) pointed on anew
  The distributed string of length Len of characters. If the parameter Buffer is equal nil,
  Contents of new string remain uncertain, differently SetString copies
  Len of characters from Buffer in new string. If for allocation of string
  Not enough memory there is an exclusive situation EOutOfMemory. Call
  Functions SetString guarantees, that S will be unique string, i.e.
  The counter of the references of this string will be equal to unit.

  For allocation in memory of new string S in length Len of characters use call
  Procedures SetString: SetString (S,nil,Len). (unit System)

procedure SetString (var S: string; Buffer: PChar; Len: Integer);}

{ SetLength reallocates memory under string addressed by the parameter S, such
  By image, that its(her) length equalled NewLength. Existing characters in string
  Are saved, and contents of the additional selected(allocated) memory will be
  Uncertain. If the necessary quantity of memory can not be selected(allocated),
  The exception EOutOfMemory is called(caused). The call SetLength guarantees, that the ambassador
  Him(it) the string S will be unique, i.e. its(her) counter of the references will be equal
  To unit. (unit System)

procedure SetLength (var S; NewLength: Integer);}

{ Copy resets a substring from string. S is initial string (expression of a type
  string), Index and Count - integer expressions. Copy resets a substring,
  Containing Count of characters, since S [Index]. If Index it is more, than length
  Strings S, Copy resets empty string. If Count sets more characters,
  Than them there is up to an end of string, are reset a substring, since S [Index]
  And up to the end of string. (unit System)

function Copy (S; Index, Count: Integer): string;}

{ Insert inserts a substring into string, since the indicated character. A substring
  Source is inserted into string S, since the character S [Index]. Index - number
  The character, with which the insert starts. If Index lengths of string S there are more,
  The substring Source is simply added in the end of string S. (unit System)

procedure Insert (Source: string; var S: string; Index: Integer);}

{ UniqueString guarantees, that the given string Str will have the counter of the references
  Equal to unit. The call of this procedure is necessary, when in the program string
  Is resulted in the type PChar, and then contents of string vary.
  (Unit System)

procedure UniqueString (var Str: string);}

{ WrapText finds in string Line characters entering into set BreakChars,
  Also adds line feed transferred(handed) by the parameter BreakStr, ambassador of the latter
  The character from BreakChars before a position MaxCol. MaxCol is a maximum length
  Strings (up to characters of line feed). If parameters BreakStr and BreakChars
  Are lowered(omitted), the function WrapText searches in string Line for blanks, characters of a worm and characters
  The tab stops, in which position can be supplemented breakings of string as a couple
  Characters #13#10. This function does not insert breakings of string into fragments,
  Concluded in single or double ковычки. (unit SysUtils)

function WrapText (const Line, BreakStr: string; BreakChars: TSysCharSet;
  MaxCol: Integer): string; overload;
function WrapText (const Line: string; MaxCol: Integer = 45): string; overload;}

{ AdjustLineBreaks substitutes all line feeds in string S on correct
  Sequence of characters CR/LF. This function transforms any character CR,
  Which the character LF does not follow, and any character LF, before which does not cost
  The character CR, in a pair of characters CR/LF. She(it) also will convert couples of characters LF/CR
  In couples CR/LF. The sequence LF/CR is usually used in text
  Files Unix. (unit SysUtils)

function AdjustLineBreaks (const S: string): string;}

{ QuotedStr concludes the string,(line,) transferred(handed) to it,(her,) in unary quotes ('). They
  Are added in the beginning and at the end of string. Besides each character unary
  Quotes inside string is doubled. (unit SysUtils)

function QuotedStr (const S: string): string;}


{ DEFINE USE_DYNAMIC_TABLES} // Substitute or remove $ in the beginning of string

{ Code charts for change of the register of characters and code conversion of strings from
  OEM in ANSI and on the contrary can be formed dynamically at start of the program
  (According to current customizations on the computer of the user) or they
  Can rigidly be set at compilation of the program (as the constant arrays).
  If the character USE_DYNAMIC_TABLES is defined (with the help $DEFINE), are used
  The dynamic tables, differently - static.}

{$IFNDEF USE_DYNAMIC_TABLES}

{ If you gather to use static tables of code conversion, but with
  By other character set (distinct from Russian coding Win1251), for this purpose
  It is necessary to change the given below constant arrays. They can be
  Are obtained as follows: install in Control Panel the language, necessary to you,
  Compile and launch the following program.

  program MakeTables;
  uses
    Windows, SysUtils;
  var
    Ch, N: Byte;
    I, J: Integer;
    F: TextFile;
  begin
    AssignFile (F, 'CsTables.txt');
    Rewrite (F);
    WriteLn (F);
    WriteLn (F, ' ToUpperChars: array [0.. 255] of Char = ');
    Write (F, '(');
    for I: = 0 to 15 do
    begin
      for J: = 0 to 15 do
      begin
        N: = (I shl 4) or J;
        Ch: = N;
        CharUpperBuff (@Ch, 1);
         Write(F,'#$'+IntToHex(Ch,2));
        if N < > 255 then
          Write (F ',')
        else
          Write (F, '); ');
      end;
      WriteLn (F);
      Write (F, ');
    end;
    WriteLn (F);
    WriteLn (F, ' ToLowerChars: array [0.. 255] of Char = ');
    Write (F, '(');
    for I: = 0 to 15 do
    begin
      for J: = 0 to 15 do
      begin
        N: = (I shl 4) or J;
        Ch: = N;
        CharLowerBuff (@Ch, 1);
         Write(F,'#$'+IntToHex(Ch,2));
        if N < > 255 then
          Write (F ',')
        else
          Write (F, '); ');
      end;
      WriteLn (F);
      Write (F, ');
    end;
    WriteLn (F);
    WriteLn (F, ' ToOemChars: array [0.. 255] of Char = ');
    Write (F, '(');
    for I: = 0 to 15 do
    begin
      for J: = 0 to 15 do
      begin
        N: = (I shl 4) or J;
        CharToOemBuff (@N, @Ch, 1);
         Write(F,'#$'+IntToHex(Ch,2));
        if N < > 255 then
          Write (F ',')
        else
          Write (F, '); ');
      end;
      WriteLn (F);
      Write (F, ');
    end;
    WriteLn (F);
    WriteLn (F, ' ToAnsiChars: array [0.. 255] of Char = ');
    Write (F, '(');
    for I: = 0 to 15 do
    begin
      for J: = 0 to 15 do
      begin
        N: = (I shl 4) or J;
        OemToCharBuff (@N, @Ch, 1);
         Write(F,'#$'+IntToHex(Ch,2));
        if N < > 255 then
          Write (F ',')
        else
          Write (F, '); ');
      end;
      WriteLn (F);
      Write (F, ');
    end;
    CloseFile (F);
  end.

  In the current directory find the file CsTables.txt and "as is" insert his(its) ambassador
  const. Compiling this unit with a key $J+, you receive possibility to change
  The static tables of code conversion in execution time of the program (dynamic
  The tables you can change irrespective of any keys).

  Multibyte characters and Unicode are not supported.
}

const
  ToUpperChars: array[0..255] of Char =
    (#$00,#$01,#$02,#$03,#$04,#$05,#$06,#$07,#$08,#$09,#$0A,#$0B,#$0C,#$0D,#$0E,#$0F,
     #$10,#$11,#$12,#$13,#$14,#$15,#$16,#$17,#$18,#$19,#$1A,#$1B,#$1C,#$1D,#$1E,#$1F,
     #$20,#$21,#$22,#$23,#$24,#$25,#$26,#$27,#$28,#$29,#$2A,#$2B,#$2C,#$2D,#$2E,#$2F,
     #$30,#$31,#$32,#$33,#$34,#$35,#$36,#$37,#$38,#$39,#$3A,#$3B,#$3C,#$3D,#$3E,#$3F,
     #$40,#$41,#$42,#$43,#$44,#$45,#$46,#$47,#$48,#$49,#$4A,#$4B,#$4C,#$4D,#$4E,#$4F,
     #$50,#$51,#$52,#$53,#$54,#$55,#$56,#$57,#$58,#$59,#$5A,#$5B,#$5C,#$5D,#$5E,#$5F,
     #$60,#$41,#$42,#$43,#$44,#$45,#$46,#$47,#$48,#$49,#$4A,#$4B,#$4C,#$4D,#$4E,#$4F,
     #$50,#$51,#$52,#$53,#$54,#$55,#$56,#$57,#$58,#$59,#$5A,#$7B,#$7C,#$7D,#$7E,#$7F,
     #$80,#$81,#$82,#$81,#$84,#$85,#$86,#$87,#$88,#$89,#$8A,#$8B,#$8C,#$8D,#$8E,#$8F,
     #$80,#$91,#$92,#$93,#$94,#$95,#$96,#$97,#$98,#$99,#$8A,#$9B,#$8C,#$8D,#$8E,#$8F,
     #$A0,#$A1,#$A1,#$A3,#$A4,#$A5,#$A6,#$A7,#$A8,#$A9,#$AA,#$AB,#$AC,#$AD,#$AE,#$AF,
     #$B0,#$B1,#$B2,#$B2,#$A5,#$B5,#$B6,#$B7,#$A8,#$B9,#$AA,#$BB,#$A3,#$BD,#$BD,#$AF,
     #$C0,#$C1,#$C2,#$C3,#$C4,#$C5,#$C6,#$C7,#$C8,#$C9,#$CA,#$CB,#$CC,#$CD,#$CE,#$CF,
     #$D0,#$D1,#$D2,#$D3,#$D4,#$D5,#$D6,#$D7,#$D8,#$D9,#$DA,#$DB,#$DC,#$DD,#$DE,#$DF,
     #$C0,#$C1,#$C2,#$C3,#$C4,#$C5,#$C6,#$C7,#$C8,#$C9,#$CA,#$CB,#$CC,#$CD,#$CE,#$CF,
     #$D0,#$D1,#$D2,#$D3,#$D4,#$D5,#$D6,#$D7,#$D8,#$D9,#$DA,#$DB,#$DC,#$DD,#$DE,#$DF);

  ToLowerChars: array[0..255] of Char =
    (#$00,#$01,#$02,#$03,#$04,#$05,#$06,#$07,#$08,#$09,#$0A,#$0B,#$0C,#$0D,#$0E,#$0F,
     #$10,#$11,#$12,#$13,#$14,#$15,#$16,#$17,#$18,#$19,#$1A,#$1B,#$1C,#$1D,#$1E,#$1F,
     #$20,#$21,#$22,#$23,#$24,#$25,#$26,#$27,#$28,#$29,#$2A,#$2B,#$2C,#$2D,#$2E,#$2F,
     #$30,#$31,#$32,#$33,#$34,#$35,#$36,#$37,#$38,#$39,#$3A,#$3B,#$3C,#$3D,#$3E,#$3F,
     #$40,#$61,#$62,#$63,#$64,#$65,#$66,#$67,#$68,#$69,#$6A,#$6B,#$6C,#$6D,#$6E,#$6F,
     #$70,#$71,#$72,#$73,#$74,#$75,#$76,#$77,#$78,#$79,#$7A,#$5B,#$5C,#$5D,#$5E,#$5F,
     #$60,#$61,#$62,#$63,#$64,#$65,#$66,#$67,#$68,#$69,#$6A,#$6B,#$6C,#$6D,#$6E,#$6F,
     #$70,#$71,#$72,#$73,#$74,#$75,#$76,#$77,#$78,#$79,#$7A,#$7B,#$7C,#$7D,#$7E,#$7F,
     #$90,#$83,#$82,#$83,#$84,#$85,#$86,#$87,#$88,#$89,#$9A,#$8B,#$9C,#$9D,#$9E,#$9F,
     #$90,#$91,#$92,#$93,#$94,#$95,#$96,#$97,#$98,#$99,#$9A,#$9B,#$9C,#$9D,#$9E,#$9F,
     #$A0,#$A2,#$A2,#$BC,#$A4,#$B4,#$A6,#$A7,#$B8,#$A9,#$BA,#$AB,#$AC,#$AD,#$AE,#$BF,
     #$B0,#$B1,#$B3,#$B3,#$B4,#$B5,#$B6,#$B7,#$B8,#$B9,#$BA,#$BB,#$BC,#$BE,#$BE,#$BF,
     #$E0,#$E1,#$E2,#$E3,#$E4,#$E5,#$E6,#$E7,#$E8,#$E9,#$EA,#$EB,#$EC,#$ED,#$EE,#$EF,
     #$F0,#$F1,#$F2,#$F3,#$F4,#$F5,#$F6,#$F7,#$F8,#$F9,#$FA,#$FB,#$FC,#$FD,#$FE,#$FF,
     #$E0,#$E1,#$E2,#$E3,#$E4,#$E5,#$E6,#$E7,#$E8,#$E9,#$EA,#$EB,#$EC,#$ED,#$EE,#$EF,
     #$F0,#$F1,#$F2,#$F3,#$F4,#$F5,#$F6,#$F7,#$F8,#$F9,#$FA,#$FB,#$FC,#$FD,#$FE,#$FF);

  ToOemChars: array[0..255] of Char =
    (#$00,#$01,#$02,#$03,#$04,#$05,#$06,#$07,#$08,#$09,#$0A,#$0B,#$0C,#$0D,#$0E,#$0F,
     #$10,#$11,#$12,#$13,#$14,#$15,#$16,#$17,#$18,#$19,#$1A,#$1B,#$1C,#$1D,#$1E,#$1F,
     #$20,#$21,#$22,#$23,#$24,#$25,#$26,#$27,#$28,#$29,#$2A,#$2B,#$2C,#$2D,#$2E,#$2F,
     #$30,#$31,#$32,#$33,#$34,#$35,#$36,#$37,#$38,#$39,#$3A,#$3B,#$3C,#$3D,#$3E,#$3F,
     #$40,#$41,#$42,#$43,#$44,#$45,#$46,#$47,#$48,#$49,#$4A,#$4B,#$4C,#$4D,#$4E,#$4F,
     #$50,#$51,#$52,#$53,#$54,#$55,#$56,#$57,#$58,#$59,#$5A,#$5B,#$5C,#$5D,#$5E,#$5F,
     #$60,#$61,#$62,#$63,#$64,#$65,#$66,#$67,#$68,#$69,#$6A,#$6B,#$6C,#$6D,#$6E,#$6F,
     #$70,#$71,#$72,#$73,#$74,#$75,#$76,#$77,#$78,#$79,#$7A,#$7B,#$7C,#$7D,#$7E,#$7F,
     #$3F,#$3F,#$27,#$3F,#$22,#$3A,#$C5,#$D8,#$3F,#$25,#$3F,#$3C,#$3F,#$3F,#$3F,#$3F,
     #$3F,#$27,#$27,#$22,#$22,#$07,#$2D,#$2D,#$3F,#$54,#$3F,#$3E,#$3F,#$3F,#$3F,#$3F,
     #$FF,#$F6,#$F7,#$3F,#$FD,#$3F,#$B3,#$15,#$F0,#$63,#$F2,#$3C,#$BF,#$2D,#$52,#$F4,
     #$F8,#$2B,#$3F,#$3F,#$3F,#$E7,#$14,#$FA,#$F1,#$FC,#$F3,#$3E,#$3F,#$3F,#$3F,#$F5,
     #$80,#$81,#$82,#$83,#$84,#$85,#$86,#$87,#$88,#$89,#$8A,#$8B,#$8C,#$8D,#$8E,#$8F,
     #$90,#$91,#$92,#$93,#$94,#$95,#$96,#$97,#$98,#$99,#$9A,#$9B,#$9C,#$9D,#$9E,#$9F,
     #$A0,#$A1,#$A2,#$A3,#$A4,#$A5,#$A6,#$A7,#$A8,#$A9,#$AA,#$AB,#$AC,#$AD,#$AE,#$AF,
     #$E0,#$E1,#$E2,#$E3,#$E4,#$E5,#$E6,#$E7,#$E8,#$E9,#$EA,#$EB,#$EC,#$ED,#$EE,#$EF);

  ToAnsiChars: array[0..255] of Char =
    (#$00,#$01,#$02,#$03,#$04,#$05,#$06,#$07,#$08,#$09,#$0A,#$0B,#$0C,#$0D,#$0E,#$A4,
     #$10,#$11,#$12,#$13,#$B6,#$A7,#$16,#$17,#$18,#$19,#$1A,#$1B,#$1C,#$1D,#$1E,#$1F,
     #$20,#$21,#$22,#$23,#$24,#$25,#$26,#$27,#$28,#$29,#$2A,#$2B,#$2C,#$2D,#$2E,#$2F,
     #$30,#$31,#$32,#$33,#$34,#$35,#$36,#$37,#$38,#$39,#$3A,#$3B,#$3C,#$3D,#$3E,#$3F,
     #$40,#$41,#$42,#$43,#$44,#$45,#$46,#$47,#$48,#$49,#$4A,#$4B,#$4C,#$4D,#$4E,#$4F,
     #$50,#$51,#$52,#$53,#$54,#$55,#$56,#$57,#$58,#$59,#$5A,#$5B,#$5C,#$5D,#$5E,#$5F,
     #$60,#$61,#$62,#$63,#$64,#$65,#$66,#$67,#$68,#$69,#$6A,#$6B,#$6C,#$6D,#$6E,#$6F,
     #$70,#$71,#$72,#$73,#$74,#$75,#$76,#$77,#$78,#$79,#$7A,#$7B,#$7C,#$7D,#$7E,#$7F,
     #$C0,#$C1,#$C2,#$C3,#$C4,#$C5,#$C6,#$C7,#$C8,#$C9,#$CA,#$CB,#$CC,#$CD,#$CE,#$CF,
     #$D0,#$D1,#$D2,#$D3,#$D4,#$D5,#$D6,#$D7,#$D8,#$D9,#$DA,#$DB,#$DC,#$DD,#$DE,#$DF,
     #$E0,#$E1,#$E2,#$E3,#$E4,#$E5,#$E6,#$E7,#$E8,#$E9,#$EA,#$EB,#$EC,#$ED,#$EE,#$EF,
     #$2D,#$2D,#$2D,#$A6,#$2B,#$A6,#$A6,#$AC,#$AC,#$A6,#$A6,#$AC,#$2D,#$2D,#$2D,#$AC,
     #$4C,#$2B,#$54,#$2B,#$2D,#$2B,#$A6,#$A6,#$4C,#$E3,#$A6,#$54,#$A6,#$3D,#$2B,#$A6,
     #$A6,#$54,#$54,#$4C,#$4C,#$2D,#$E3,#$2B,#$2B,#$2D,#$2D,#$2D,#$2D,#$A6,#$A6,#$2D,
     #$F0,#$F1,#$F2,#$F3,#$F4,#$F5,#$F6,#$F7,#$F8,#$F9,#$FA,#$FB,#$FC,#$FD,#$FE,#$FF,
     #$A8,#$B8,#$AA,#$BA,#$AF,#$BF,#$A1,#$A2,#$B0,#$95,#$B7,#$76,#$B9,#$A4,#$A6,#$A0);

{$ELSE}

var
  ToUpperChars,ToLowerChars: array[0..255] of Char;
  ToOemChars,ToAnsiChars: array[0..255] of Char;

{$ENDIF}

implementation

uses Windows, SysUtils, Math;

function Q_CompStr(const S1, S2: string): Integer;
asm
        TEST    EAX,EAX
        JE      @@2
        TEST    EDX,EDX
        JE      @@3
        PUSH    EAX
        MOVZX   EAX,BYTE PTR [EAX]
        MOVZX   ECX,BYTE PTR [EDX]
        SUB     EAX,ECX
        JE      @@m
        POP     ECX
        RET
@@m:    POP     EAX
        INC     EAX
        INC     EDX
@@0:    TEST    CL,CL
        JE      @@5
        MOV     CL,BYTE PTR [EAX]
        MOV     CH,BYTE PTR [EDX]
        CMP     CL,CH
        JNE     @@ne
        TEST    CL,CL
        JE      @@5
        MOV     CL,BYTE PTR [EAX+1]
        MOV     CH,BYTE PTR [EDX+1]
        CMP     CL,CH
        JNE     @@ne
        TEST    CL,CL
        JE      @@5
        MOV     CL,BYTE PTR [EAX+2]
        MOV     CH,BYTE PTR [EDX+2]
        CMP     CL,CH
        JNE     @@ne
        TEST    CL,CL
        JE      @@5
        MOV     CL,BYTE PTR [EAX+3]
        MOV     CH,BYTE PTR [EDX+3]
        ADD     EAX,4
        ADD     EDX,4
        CMP     CL,CH
        JE      @@0
@@ne:   MOVZX   EAX,CL
        MOVZX   EDX,CH
        SUB     EAX,EDX
        RET
@@2:    TEST    EDX,EDX
        JE      @@7
        MOV     CH,BYTE PTR [EDX]
        TEST    CH,CH
        JE      @@7
        NOT     EAX
        RET
@@3:    MOV     CL,BYTE PTR [EAX]
        TEST    CL,CL
        JE      @@5
        MOV     EAX,1
        RET
@@5:    XOR     EAX,EAX
@@7:
end;

function Q_PCompStr(P1, P2: PChar): Integer;
asm
        TEST    EAX,EAX
        JE      @@2
        TEST    EDX,EDX
        JE      @@3
        PUSH    EAX
        MOVZX   EAX,BYTE PTR [EAX]
        MOVZX   ECX,BYTE PTR [EDX]
        SUB     EAX,ECX
        JE      @@m
        POP     ECX
        RET
@@m:    POP     EAX
        INC     EAX
        INC     EDX
@@0:    TEST    CL,CL
        JE      @@5
        MOV     CL,BYTE PTR [EAX]
        MOV     CH,BYTE PTR [EDX]
        CMP     CL,CH
        JNE     @@ne
        TEST    CL,CL
        JE      @@5
        MOV     CL,BYTE PTR [EAX+1]
        MOV     CH,BYTE PTR [EDX+1]
        CMP     CL,CH
        JNE     @@ne
        TEST    CL,CL
        JE      @@5
        MOV     CL,BYTE PTR [EAX+2]
        MOV     CH,BYTE PTR [EDX+2]
        CMP     CL,CH
        JNE     @@ne
        TEST    CL,CL
        JE      @@5
        MOV     CL,BYTE PTR [EAX+3]
        MOV     CH,BYTE PTR [EDX+3]
        ADD     EAX,4
        ADD     EDX,4
        CMP     CL,CH
        JE      @@0
@@ne:   MOVZX   EAX,CL
        MOVZX   EDX,CH
        SUB     EAX,EDX
        RET
@@2:    TEST    EDX,EDX
        JE      @@7
        MOV     CH,BYTE PTR [EDX]
        TEST    CH,CH
        JE      @@7
        NOT     EAX
        RET
@@3:    MOV     CL,BYTE PTR [EAX]
        TEST    CL,CL
        JE      @@5
        MOV     EAX,1
        RET
@@5:    XOR     EAX,EAX
@@7:
end;

function Q_CompStrL(const S1, S2: string; MaxL: Cardinal): Integer;
asm
        TEST    ECX,ECX
        JE      @@1
        TEST    EAX,EAX
        JE      @@2
        TEST    EDX,EDX
        JE      @@3
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,[EAX-4]
        MOV     ESI,[EDX-4]
        SUB     EBX,ESI
        JG      @@w1
        ADD     ESI,EBX
@@w1:   CMP     ECX,ESI
        JA      @@fc
@@dn:   POP     ESI
@@lp:   DEC     ECX
        JS      @@zq
        MOV     BL,BYTE PTR [EAX]
        MOV     BH,BYTE PTR [EDX]
        CMP     BL,BH
        JNE     @@ne
        DEC     ECX
        JS      @@zq
        MOV     BL,BYTE PTR [EAX+1]
        MOV     BH,BYTE PTR [EDX+1]
        CMP     BL,BH
        JNE     @@ne
        DEC     ECX
        JS      @@zq
        MOV     BL,BYTE PTR [EAX+2]
        MOV     BH,BYTE PTR [EDX+2]
        CMP     BL,BH
        JNE     @@ne
        DEC     ECX
        JS      @@zq
        MOV     BL,BYTE PTR [EAX+3]
        MOV     BH,BYTE PTR [EDX+3]
        ADD     EAX,4
        ADD     EDX,4
        CMP     BL,BH
        JE      @@lp
@@ne:   MOVZX   EAX,BL
        MOVZX   EDX,BH
        SUB     EAX,EDX
        POP     EBX
        RET
@@fc:   LEA     ECX,[ESI+1]
        JMP     @@dn
@@1:    XOR     EAX,EAX
        RET
@@2:    TEST    EDX,EDX
        JE      @@7
        MOV     CH,BYTE PTR [EDX]
        TEST    CH,CH
        JE      @@7
        NOT     EAX
        RET
@@3:    MOV     CL,BYTE PTR [EAX]
        TEST    CL,CL
        JE      @@5
        MOV     EAX,1
        RET
@@zq:   POP     EBX
@@5:    XOR     EAX,EAX
@@7:
end;

function Q_CompText(const S1, S2: string): Integer;
asm
        TEST    EAX,EAX
        JE      @@2
        TEST    EDX,EDX
        JE      @@3
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        JMP     @@1
@@0:    TEST    AL,AL
        JE      @@4
        INC     ESI
        INC     EDI
@@1:    MOVZX   EAX,BYTE PTR [ESI]
        MOVZX   EDX,BYTE PTR [EDI]
        CMP     AL,DL
        JE      @@0
        MOV     AL,BYTE PTR [EAX+ToUpperChars]
        MOV     DL,BYTE PTR [EDX+ToUpperChars]
        CMP     AL,DL
        JE      @@0
        MOVZX   EAX,AL
        MOVZX   EDX,DL
        SUB     EAX,EDX
        POP     EDI
        POP     ESI
        RET
@@2:    TEST    EDX,EDX
        JE      @@7
        MOV     CH,BYTE PTR [EDX]
        TEST    CH,CH
        JE      @@7
        NOT     EAX
        RET
@@3:    MOV     CL,BYTE PTR [EAX]
        TEST    CL,CL
        JE      @@5
        MOV     EAX,1
        RET
@@4:    POP     EDI
        POP     ESI
@@5:    XOR     EAX,EAX
@@7:
end;

function Q_PCompText(P1, P2: PChar): Integer;
asm
        TEST    EAX,EAX
        JE      @@2
        TEST    EDX,EDX
        JE      @@3
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        JMP     @@1
@@0:    TEST    AL,AL
        JE      @@4
        INC     ESI
        INC     EDI
@@1:    MOVZX   EAX,BYTE PTR [ESI]
        MOVZX   EDX,BYTE PTR [EDI]
        CMP     AL,DL
        JE      @@0
        MOV     AL,BYTE PTR [EAX+ToUpperChars]
        MOV     DL,BYTE PTR [EDX+ToUpperChars]
        CMP     AL,DL
        JE      @@0
        MOVZX   EAX,AL
        MOVZX   EDX,DL
        SUB     EAX,EDX
        POP     EDI
        POP     ESI
        RET
@@2:    TEST    EDX,EDX
        JE      @@7
        MOV     CH,BYTE PTR [EDX]
        TEST    CH,CH
        JE      @@7
        NOT     EAX
        RET
@@3:    MOV     CL,BYTE PTR [EAX]
        TEST    CL,CL
        JE      @@5
        MOV     EAX,1
        RET
@@4:    POP     EDI
        POP     ESI
@@5:    XOR     EAX,EAX
@@7:
end;

function Q_CompTextL(const S1, S2: string; MaxL: Cardinal): Integer;
asm
        TEST    ECX,ECX
        JE      @@5
        TEST    EAX,EAX
        JE      @@2
        TEST    EDX,EDX
        JE      @@3
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,[EAX-4]
        MOV     EDI,[EDX-4]
        SUB     ESI,EDI
        JG      @@w1
        ADD     EDI,ESI
@@w1:   CMP     ECX,EDI
        JA      @@fc
@@dn:   MOV     ESI,EAX
        MOV     EDI,EDX
@@lp:   DEC     ECX
        JS      @@zq
        MOVZX   EAX,BYTE PTR [ESI]
        MOVZX   EDX,BYTE PTR [EDI]
        INC     ESI
        INC     EDI
        CMP     AL,DL
        JE      @@lp
        MOV     AL,BYTE PTR [EAX+ToUpperChars]
        MOV     DL,BYTE PTR [EDX+ToUpperChars]
        CMP     AL,DL
        JE      @@lp
@@ne:   MOVZX   EAX,AL
        MOVZX   EDX,DL
        SUB     EAX,EDX
        POP     EDI
        POP     ESI
        RET
@@fc:   LEA     ECX,[EDI+1]
        JMP     @@dn
@@2:    TEST    EDX,EDX
        JE      @@7
        MOV     CH,BYTE PTR [EDX]
        TEST    CH,CH
        JE      @@7
        NOT     EAX
        RET
@@3:    MOV     CL,BYTE PTR [EAX]
        TEST    CL,CL
        JE      @@5
        MOV     EAX,1
        RET
@@zq:   POP     EDI
        POP     ESI
@@5:    XOR     EAX,EAX
@@7:
end;

function Q_SameStr(const S1, S2: string): Boolean;
asm
        CMP     EAX,EDX
        JE      @@08
        TEST    EAX,EAX
        JE      @@qt2
        TEST    EDX,EDX
        JE      @@qt1
        MOV     ECX,[EAX-4]
        CMP     ECX,[EDX-4]
        JE      @@01
@@qt1:  XOR     EAX,EAX
@@qt2:  RET
@@01:   PUSH    EBX
        SUB     ECX,8
        JS      @@nx
@@lp:   MOV     EBX,DWORD PTR [EAX+ECX]
        CMP     EBX,DWORD PTR [EDX+ECX]
        JNE     @@zq
        MOV     EBX,DWORD PTR [EAX+ECX+4]
        CMP     EBX,DWORD PTR [EDX+ECX+4]
        JNE     @@zq
        SUB     ECX,8
        JNS     @@lp
@@nx:   JMP     DWORD PTR @@tV[ECX*4+32]
@@tV:   DD      @@eq,@@t1,@@t2,@@t3
        DD      @@t4,@@t5,@@t6,@@t7
@@t7:   MOV     BL,BYTE PTR [EAX+6]
        XOR     BL,BYTE PTR [EDX+6]
        JNE     @@zq
@@t6:   MOV     BL,BYTE PTR [EAX+5]
        XOR     BL,BYTE PTR [EDX+5]
        JNE     @@zq
@@t5:   MOV     BL,BYTE PTR [EAX+4]
        XOR     BL,BYTE PTR [EDX+4]
        JNE     @@zq
@@t4:   MOV     EBX,DWORD PTR [EAX]
        CMP     EBX,DWORD PTR [EDX]
        JNE     @@zq
@@eq:   POP     EBX
@@08:   MOV     EAX,1
        RET
@@t3:   MOV     BL,BYTE PTR [EAX+2]
        XOR     BL,BYTE PTR [EDX+2]
        JNE     @@zq
@@t2:   MOV     BL,BYTE PTR [EAX+1]
        XOR     BL,BYTE PTR [EDX+1]
        JNE     @@zq
@@t1:   MOV     BL,BYTE PTR [EAX]
        XOR     BL,BYTE PTR [EDX]
        JNE     @@zq
        POP     EBX
        MOV     EAX,1
        RET
@@zq:   POP     EBX
@@07:   XOR     EAX,EAX
end;

function Q_PSameStr(P1, P2: Pointer): Boolean;
asm
        CMP     EAX,EDX
        JE      @@08
        TEST    EAX,EAX
        JE      @@qt2
        TEST    EDX,EDX
        JE      @@qt1
        MOV     ECX,[EAX-4]
        CMP     ECX,[EDX-4]
        JE      @@01
@@qt1:  XOR     EAX,EAX
@@qt2:  RET
@@01:   PUSH    EBX
        SUB     ECX,8
        JS      @@nx
@@lp:   MOV     EBX,DWORD PTR [EAX+ECX]
        CMP     EBX,DWORD PTR [EDX+ECX]
        JNE     @@zq
        MOV     EBX,DWORD PTR [EAX+ECX+4]
        CMP     EBX,DWORD PTR [EDX+ECX+4]
        JNE     @@zq
        SUB     ECX,8
        JNS     @@lp
@@nx:   JMP     DWORD PTR @@tV[ECX*4+32]
@@tV:   DD      @@eq,@@t1,@@t2,@@t3
        DD      @@t4,@@t5,@@t6,@@t7
@@t7:   MOV     BL,BYTE PTR [EAX+6]
        XOR     BL,BYTE PTR [EDX+6]
        JNE     @@zq
@@t6:   MOV     BL,BYTE PTR [EAX+5]
        XOR     BL,BYTE PTR [EDX+5]
        JNE     @@zq
@@t5:   MOV     BL,BYTE PTR [EAX+4]
        XOR     BL,BYTE PTR [EDX+4]
        JNE     @@zq
@@t4:   MOV     EBX,DWORD PTR [EAX]
        CMP     EBX,DWORD PTR [EDX]
        JNE     @@zq
@@eq:   POP     EBX
@@08:   MOV     EAX,1
        RET
@@t3:   MOV     BL,BYTE PTR [EAX+2]
        XOR     BL,BYTE PTR [EDX+2]
        JNE     @@zq
@@t2:   MOV     BL,BYTE PTR [EAX+1]
        XOR     BL,BYTE PTR [EDX+1]
        JNE     @@zq
@@t1:   MOV     BL,BYTE PTR [EAX]
        XOR     BL,BYTE PTR [EDX]
        JNE     @@zq
        POP     EBX
        MOV     EAX,1
        RET
@@zq:   POP     EBX
@@07:   XOR     EAX,EAX
end;

function Q_SameStrL(const S1, S2: string; MaxL: Cardinal): Boolean;
asm
        CMP     EAX,EDX
        JE      @@08
        TEST    EAX,EAX
        JE      @@09
        TEST    EDX,EDX
        JE      @@07
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,[EAX-4]
        MOV     ESI,[EDX-4]
        SUB     EBX,ESI
        JG      @@w1
        ADD     ESI,EBX
@@w1:   CMP     ECX,ESI
        JA      @@fc
@@dn:   SUB     ECX,4
        JS      @@nx
@@lp:   MOV     EBX,DWORD PTR [EAX+ECX]
        CMP     EBX,DWORD PTR [EDX+ECX]
        JNE     @@zq
        SUB     ECX,4
        JNS     @@lp
@@nx:   JMP     DWORD PTR @@tV[ECX*4+16]
@@tV:   DD      @@eq,@@t1,@@t2,@@t3
@@t3:   MOV     BL,BYTE PTR [EAX+2]
        XOR     BL,BYTE PTR [EDX+2]
        JNE     @@zq
@@t2:   MOV     BL,BYTE PTR [EAX+1]
        XOR     BL,BYTE PTR [EDX+1]
        JNE     @@zq
@@t1:   MOV     BL,BYTE PTR [EAX]
        XOR     BL,BYTE PTR [EDX]
        JNE     @@zq
@@eq:   POP     ESI
        POP     EBX
@@08:   MOV     EAX,1
        RET
@@fc:   MOV     ECX,ESI
        TEST    EBX,EBX
        JE      @@dn
@@zq:   POP     ESI
        POP     EBX
@@07:   XOR     EAX,EAX
@@09:
end;

function Q_SameText(const S1, S2: string): Boolean;
asm
        CMP     EAX,EDX
        JE      @@08
        TEST    EAX,EAX
        JE      @@09
        TEST    EDX,EDX
        JE      @@07
        MOV     ECX,[EAX-4]
        CMP     ECX,[EDX-4]
        JE      @@im
        XOR     EAX,EAX
        RET
@@im:   TEST    ECX,ECX
        JE      @@07
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
@@00:   DEC     ECX
        JS      @@qt
@@01:   MOVZX   EAX,BYTE PTR [ESI+ECX]
        MOVZX   EDX,BYTE PTR [EDI+ECX]
        CMP     AL,DL
        JE      @@00
        MOV     AL,BYTE PTR [EAX+ToUpperChars]
        XOR     AL,BYTE PTR [EDX+ToUpperChars]
        JE      @@00
        POP     EDI
        POP     ESI
@@07:   XOR     EAX,EAX
@@09:   RET
@@qt:   POP     EDI
        POP     ESI
@@08:   MOV     EAX,1
end;

function Q_PSameText(P1, P2: Pointer): Boolean;
asm
        CMP     EAX,EDX
        JE      @@08
        TEST    EAX,EAX
        JE      @@09
        TEST    EDX,EDX
        JE      @@07
        MOV     ECX,[EAX-4]
        CMP     ECX,[EDX-4]
        JE      @@im
        XOR     EAX,EAX
        RET
@@im:   TEST    ECX,ECX
        JE      @@07
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
@@00:   DEC     ECX
        JS      @@qt
@@01:   MOVZX   EAX,BYTE PTR [ESI+ECX]
        MOVZX   EDX,BYTE PTR [EDI+ECX]
        CMP     AL,DL
        JE      @@00
        MOV     AL,BYTE PTR [EAX+ToUpperChars]
        XOR     AL,BYTE PTR [EDX+ToUpperChars]
        JE      @@00
        POP     EDI
        POP     ESI
@@07:   XOR     EAX,EAX
@@09:   RET
@@qt:   POP     EDI
        POP     ESI
@@08:   MOV     EAX,1
end;

function Q_SameTextL(const S1, S2: string; MaxL: Cardinal): Boolean;
asm
        CMP     EAX,EDX
        JE      @@08
        TEST    EAX,EAX
        JE      @@xx
        TEST    EDX,EDX
        JE      @@07
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,[EAX-4]
        MOV     EDI,[EDX-4]
        SUB     ESI,EDI
        JG      @@w1
        ADD     EDI,ESI
@@w1:   CMP     ECX,EDI
        JA      @@fc
@@dn:   TEST    ECX,ECX
        JE      @@zq
        MOV     ESI,EAX
        MOV     EDI,EDX
@@0:    DEC     ECX
        JS      @@eq
@@1:    MOVZX   EAX,BYTE PTR [ESI+ECX]
        MOVZX   EDX,BYTE PTR [EDI+ECX]
        CMP     AL,DL
        JE      @@0
        MOV     AL,BYTE PTR [EAX+ToUpperChars]
        XOR     AL,BYTE PTR [EDX+ToUpperChars]
        JE      @@0
@@zq:   POP     EDI
        POP     ESI
@@07:   XOR     EAX,EAX
@@xx:   RET
@@fc:   MOV     ECX,EDI
        TEST    ESI,ESI
        JE      @@dn
        JMP     @@zq
@@eq:   POP     EDI
        POP     ESI
@@08:   MOV     EAX,1
end;

function Q_MatchStr(const SubStr, S: string; Pos: Integer): Boolean;
asm
        TEST    EAX,EAX
        JE      @@qt
        TEST    EDX,EDX
        JE      @@zq
        PUSH    EBX
        DEC     ECX
        JL      @@m0
        MOV     EBX,[EDX-4]
        SUB     EBX,ECX
        JLE     @@m0
        ADD     EDX,ECX
        MOV     ECX,[EAX-4]
        CMP     EBX,ECX
        JL      @@m0
        SUB     ECX,4
        JS      @@nx
@@lp:   MOV     EBX,DWORD PTR [EAX+ECX]
        CMP     EBX,DWORD PTR [EDX+ECX]
        JNE     @@m0
        SUB     ECX,4
        JNS     @@lp
@@nx:   JMP     DWORD PTR @@tV[ECX*4+16]
@@tV:   DD      @@m1,@@t1,@@t2,@@t3
@@t3:   MOV     BL,BYTE PTR [EAX+2]
        XOR     BL,BYTE PTR [EDX+2]
        JNE     @@m0
@@t2:   MOV     BL,BYTE PTR [EAX+1]
        XOR     BL,BYTE PTR [EDX+1]
        JNE     @@m0
@@t1:   MOV     BL,BYTE PTR [EAX]
        XOR     BL,BYTE PTR [EDX]
        JNE     @@m0
@@m1:   POP     EBX
@@qt:   MOV     EAX,1
        RET
@@m0:   POP     EBX
@@zq:   XOR     EAX,EAX
end;

function Q_MatchText(const SubStr, S: string; Pos: Integer): Boolean;
asm
        TEST    EAX,EAX
        JE      @@qt
        TEST    EDX,EDX
        JE      @@zq
        PUSH    EBX
        PUSH    ESI
        DEC     ECX
        JL      @@m0
        MOV     EBX,[EDX-4]
        SUB     EBX,ECX
        JLE     @@m0
        ADD     EDX,ECX
        MOV     ESI,[EAX-4]
        CMP     EBX,ESI
        JL      @@m0
@@lp:   DEC     ESI
        JS      @@m1
        MOVZX   EBX,BYTE PTR [EAX+ESI]
        MOVZX   ECX,BYTE PTR [EDX+ESI]
        CMP     BL,CL
        JE      @@lp
        MOV     BL,BYTE PTR [EBX+ToUpperChars]
        XOR     BL,BYTE PTR [ECX+ToUpperChars]
        JE      @@lp
@@m0:   POP     ESI
        POP     EBX
@@zq:   XOR     EAX,EAX
        RET
@@m1:   POP     ESI
        POP     EBX
@@qt:   MOV     EAX,1
end;

function Q_TestByMask(const S, Mask: string; MaskChar: Char): Boolean;
asm
        TEST    EAX,EAX
        JE      @@qt2
        PUSH    EBX
        TEST    EDX,EDX
        JE      @@qt1
        MOV     EBX,[EAX-4]
        CMP     EBX,[EDX-4]
        JE      @@01
@@qt1:  XOR     EAX,EAX
        POP     EBX
@@qt2:  RET
@@01:   DEC     EBX
        JS      @@07
@@lp:   MOV     CH,BYTE PTR [EDX+EBX]
        CMP     CL,CH
        JNE     @@cm
        DEC     EBX
        JS      @@eq
        MOV     CH,BYTE PTR [EDX+EBX]
        CMP     CL,CH
        JNE     @@cm
        DEC     EBX
        JS      @@eq
        MOV     CH,BYTE PTR [EDX+EBX]
        CMP     CL,CH
        JNE     @@cm
        DEC     EBX
        JS      @@eq
        MOV     CH,BYTE PTR [EDX+EBX]
        CMP     CL,CH
        JNE     @@cm
        DEC     EBX
        JNS     @@lp
@@eq:   MOV     EAX,1
        POP     EBX
        RET
@@cm:   CMP     CH,BYTE PTR [EAX+EBX]
        JNE     @@07
        DEC     EBX
        JNS     @@lp
        MOV     EAX,1
        POP     EBX
        RET
@@07:   XOR     EAX,EAX
        POP     EBX
end;

function Q_TestWildStr(const S, Mask: string; MaskChar, WildCard: Char): Boolean;
label
  99;
var
  L,X,X0,Q: Integer;
  P,P1,B: PChar;
  C: Char;
begin
  X := Q_StrScan(Mask,WildCard);
  if X = 0 then
  begin
    Result := Q_TestByMask(S,Mask,MaskChar);
    Exit;
  end;
  L := Length(S);
  P := Pointer(S);
  Result := False;
  B := Pointer(Mask);
  Q := X-1;
  if L < Q then
    Exit;
  while Q > 0 do
  begin
    C := B^;
    if (C<>MaskChar) and (C<>P^) then
      Exit;
    Dec(Q);
    Inc(B);
    Inc(P);
  end;
  Dec(L,X-1);
  repeat
    X0 := X;
    P1 := P;
    while Mask[X0] = WildCard do
      Inc(X0);
    X := Q_StrScan(Mask,WildCard,X0);
    if X = 0 then
      Break;
  99:
    P := P1;
    B := @Mask[X0];
    Q := X-X0;
    if L < Q then
      Exit;
    while Q > 0 do
    begin
      C := B^;
      if (C<>MaskChar) and (C<>P^) then
      begin
        Inc(P1);
        Dec(L);
        goto 99;
      end;
      Dec(Q);
      Inc(B);
      Inc(P);
    end;
    Dec(L,X-X0);
  until False;
  X := Length(Mask);
  if L >= X-X0+1 then
  begin
    P := Pointer(S);
    Inc(P,Length(S)-1);
    while X >= X0 do
    begin
      C := Mask[X];
      if (C<>MaskChar) and (C<>P^) then
        Exit;
      Dec(X);
      Dec(P);
    end;
    Result := True;
  end;
end;

function Q_TestWildText(const S, Mask: string; MaskChar, WildCard: Char): Boolean;
label
  99;
var
  L,X,X0,Q: Integer;
  P,P1,B: PChar;
  C: Char;
begin
  X := Q_StrScan(Mask,WildCard);
  Result := False;
  if X = 0 then
  begin
    L := Length(Mask);
    if (L>0) and (L=Length(S)) then
    begin
      P := Pointer(S);
      B := Pointer(Mask);
      repeat
        C := B^;
        if (C<>MaskChar) and (C<>P^) and
            (ToUpperChars[Byte(C)]<>ToUpperChars[Byte(P^)]) then
          Exit;
        Dec(L);
        Inc(B);
        Inc(P);
      until L = 0;
      Result := True;
    end;
    Exit;
  end;
  L := Length(S);
  P := Pointer(S);
  B := Pointer(Mask);
  Q := X-1;
  if L < Q then
    Exit;
  while Q > 0 do
  begin
    C := B^;
    if (C<>MaskChar) and (C<>P^) and
        (ToUpperChars[Byte(C)]<>ToUpperChars[Byte(P^)]) then
      Exit;
    Dec(Q);
    Inc(B);
    Inc(P);
  end;
  Dec(L,X-1);
  repeat
    X0 := X;
    P1 := P;
    while Mask[X0] = WildCard do
      Inc(X0);
    X := Q_StrScan(Mask,WildCard,X0);
    if X = 0 then
      Break;
  99:
    P := P1;
    B := @Mask[X0];
    Q := X-X0;
    if L < Q then
      Exit;
    while Q > 0 do
    begin
      C := B^;
      if (C<>MaskChar) and (C<>P^) and
        (ToUpperChars[Byte(C)]<>ToUpperChars[Byte(P^)]) then
      begin
        Inc(P1);
        Dec(L);
        goto 99;
      end;
      Dec(Q);
      Inc(B);
      Inc(P);
    end;
    Dec(L,X-X0);
  until False;
  X := Length(Mask);
  if L >= X-X0+1 then
  begin
    P := Pointer(S);
    Inc(P,Length(S)-1);
    while X >= X0 do
    begin
      C := Mask[X];
      if (C<>MaskChar) and (C<>P^) and
          (ToUpperChars[Byte(C)]<>ToUpperChars[Byte(P^)]) then
        Exit;
      Dec(X);
      Dec(P);
    end;
    Result := True;
  end;
end;

function Q_CharUpper(Ch: Char): Char;
asm
        MOVZX   EDX,AL
        MOV     AL,BYTE PTR [EDX+ToUpperChars]
end;

function Q_CharLower(Ch: Char): Char;
asm
        MOVZX   EDX,AL
        MOV     AL,BYTE PTR [EDX+ToLowerChars]
end;

procedure Q_StrUpper(var S: string);
asm
        CALL    UniqueString
        TEST    EAX,EAX
        JE      @@2
        MOV     ECX,[EAX-4]
        DEC     ECX
        JS      @@2
@@1:    MOVZX   EDX,BYTE PTR [EAX+ECX]
        MOV     DL,BYTE PTR [EDX+ToUpperChars]
        MOV     BYTE PTR [EAX+ECX],DL
        DEC     ECX
        JNS     @@1
@@2:
end;

function Q_PStrUpper(P: PChar): PChar;
asm
        TEST    EAX,EAX
        JE      @@2
        PUSH    EAX
        JMP     @@1
@@0:    MOV     CL,BYTE PTR [EDX+ToUpperChars]
        MOV     BYTE PTR [EAX],CL
        INC     EAX
@@1:    MOVZX   EDX,BYTE PTR [EAX]
        TEST    DL,DL
        JNE     @@0
        POP     EAX
@@2:
end;

procedure Q_StrLower(var S: string);
asm
        CALL    UniqueString
        TEST    EAX,EAX
        JE      @@2
        MOV     ECX,[EAX-4]
        DEC     ECX
        JS      @@2
@@1:    MOVZX   EDX,BYTE PTR [EAX+ECX]
        MOV     DL,BYTE PTR [EDX+ToLowerChars]
        MOV     BYTE PTR [EAX+ECX],DL
        DEC     ECX
        JNS     @@1
@@2:
end;

function Q_PStrLower(P: PChar): PChar;
asm
        TEST    EAX,EAX
        JE      @@2
        PUSH    EAX
        JMP     @@1
@@0:    MOV     CL,BYTE PTR [EDX+ToLowerChars]
        MOV     BYTE PTR [EAX],CL
        INC     EAX
@@1:    MOVZX   EDX,BYTE PTR [EAX]
        TEST    DL,DL
        JNE     @@0
        POP     EAX
@@2:
end;

procedure Q_StrUpperMoveL(const Source: string; var Dest: string; MaxL: Cardinal);
asm
        MOV     EDX,[EDX]
        TEST    EAX,EAX
        JE      @@x2
        PUSH    EDI
        MOV     EDI,[EAX-4]
        TEST    EDI,EDI
        JE      @@x4
        CMP     ECX,EDI
        JB      @@x0
        MOV     ECX,EDI
@@x0:   MOV     [EDX-4],ECX
        MOV     BYTE PTR [ECX+EDX],$00
        DEC     ECX
        JS      @@qt
        MOV     EDI,EDX
@@1:    MOVZX   EDX,BYTE PTR [EAX+ECX]
        MOV     DL,BYTE PTR [EDX+ToUpperChars]
        MOV     BYTE PTR [EDI+ECX],DL
        DEC     ECX
        JNS     @@1
@@qt:   POP     EDI
        RET
@@x4:   POP     EDI
        XOR     EAX,EAX
@@x2:   TEST    EDX,EDX
        JE      @@x3
        MOV     [EDX-4],EAX
        MOV     BYTE PTR [EDX],AL
@@x3:
end;

procedure Q_StrLowerMoveL(const Source: string; var Dest: string; MaxL: Cardinal);
asm
        MOV     EDX,[EDX]
        TEST    EAX,EAX
        JE      @@x2
        PUSH    EDI
        MOV     EDI,[EAX-4]
        TEST    EDI,EDI
        JE      @@x4
        CMP     ECX,EDI
        JB      @@x0
        MOV     ECX,EDI
@@x0:   MOV     [EDX-4],ECX
        MOV     BYTE PTR [ECX+EDX],$00
        DEC     ECX
        JS      @@qt
        MOV     EDI,EDX
@@1:    MOVZX   EDX,BYTE PTR [EAX+ECX]
        MOV     DL,BYTE PTR [EDX+ToLowerChars]
        MOV     BYTE PTR [EDI+ECX],DL
        DEC     ECX
        JNS     @@1
@@qt:   POP     EDI
        RET
@@x4:   POP     EDI
        XOR     EAX,EAX
@@x2:   TEST    EDX,EDX
        JE      @@x3
        MOV     [EDX-4],EAX
        MOV     BYTE PTR [EDX],AL
@@x3:
end;

function Q_UpperCase(const S: string): string;
var
  L: Integer;
begin
  L := Length(S);
  SetString(Result, nil, L);
  Q_StrUpperMoveL(S, Result, L);
end;

function Q_LowerCase(const S: string): string;
var
  L: Integer;
begin
  L := Length(S);
  SetString(Result, nil, L);
  Q_StrLowerMoveL(S, Result, L);
end;

procedure Q_UpLowerInPlace(var S: string);
var
  P: PChar;
begin
  if Length(S) <> 0 then
  begin
    Q_StrLower(S);
    P := Pointer(S);
    P^ := ToUpperChars[Byte(P^)];
  end;
end;

function Q_UpLowerStr(const S: string): string;
var
  L: Integer;
  P: PChar;
begin
  L := Length(S);
  if L > 1 then
  begin
    SetString(Result, nil, L);
    Q_StrLowerMoveL(S, Result, L);
    P := Pointer(Result);
    P^ := ToUpperChars[Byte(P^)];
  end
  else if L = 1 then
    Result := ToUpperChars[Byte(S[1])]
  else
    Result := '';
end;

type
  PDelimsMap = ^TDelimsMap;
  TDelimsMap = array[#0..#255] of Boolean;
  PDelimsArr = ^TDelimsArr;
  TDelimsArr = array[0..255] of Boolean;

  PByte = ^Byte;
  PWord = ^Word;
  PLong = ^LongWord;

threadvar
  UDelimsMap: TDelimsMap;
  PosTable: array[0..255] of Byte;
  TableFindString: string;

procedure Q_ProperCaseInPlace(var S: string; const Delimiters: string); overload;
var
  DelimsMap: PDelimsMap;
  I: Integer;
  L: LongWord;
  P: PChar;
  A: Boolean;
begin
  asm
        CALL    SysInit.@GetTls
        LEA     EAX,[EAX+UDelimsMap]
        MOV     DelimsMap,EAX
  end;
  L := Length(Delimiters);
  if L <> 0 then
  begin
    Q_FillLong(0,DelimsMap,64);
    for I := 1 to L do
      DelimsMap^[Delimiters[I]] := True;
  end;
  Q_StrLower(S);
  P := Pointer(S);
  A := False;
  for I := 1 to Length(S) do
  begin
    if not DelimsMap^[P^] then
    begin
      if not A then
      begin
        A := True;
        P^ := ToUpperChars[Byte(P^)];
      end;
    end
    else if A then
      A := False;
    Inc(P);
  end;
end;

procedure Q_ProperCaseInPlace(var S: string; const Delimiters: TCharSet); overload;
var
  I: Integer;
  P: PChar;
  A: Boolean;
begin
  Q_StrLower(S);
  P := Pointer(S);
  A := False;
  for I := 1 to Length(S) do
  begin
    if not (P^ in Delimiters) then
    begin
      if not A then
      begin
        A := True;
        P^ := ToUpperChars[Byte(P^)];
      end;
    end
    else if A then
      A := False;
    Inc(P);
  end;
end;

function Q_ProperCase(const S, Delimiters: string): string; overload;
var
  DelimsMap: PDelimsMap;
  I: Integer;
  L: LongWord;
  P: PChar;
  A: Boolean;
begin
  asm
        CALL    SysInit.@GetTls
        LEA     EAX,[EAX+UDelimsMap]
        MOV     DelimsMap,EAX
  end;
  L := Length(Delimiters);
  if L <> 0 then
  begin
    Q_FillLong(0,DelimsMap,64);
    for I := 1 to L do
      DelimsMap^[Delimiters[I]] := True;
  end;
  L := Length(S);
  if L <> 0 then
  begin
    SetString(Result, nil, L);
    Q_StrLowerMoveL(S, Result, L);
    P := Pointer(Result);
    A := False;
    for I := 1 to L do
    begin
      if not DelimsMap^[P^] then
      begin
        if not A then
        begin
          A := True;
          P^ := ToUpperChars[Byte(P^)];
        end;
      end
      else if A then
        A := False;
      Inc(P);
    end;
  end else
    Result := '';
end;

function Q_ProperCase(const S: string; const Delimiters: TCharSet): string; overload;
var
  I: Integer;
  L: LongWord;
  P: PChar;
  A: Boolean;
begin
  L := Length(S);
  if L <> 0 then
  begin
    SetString(Result, nil, L);
    Q_StrLowerMoveL(S, Result, L);
    P := Pointer(Result);
    A := False;
    for I := 1 to L do
    begin
      if not (P^ in Delimiters) then
      begin
        if not A then
        begin
          A := True;
          P^ := ToUpperChars[Byte(P^)];
        end;
      end
      else if A then
        A := False;
      Inc(P);
    end;
  end else
    Result := '';
end;

procedure Q_StrToAnsi(var S: string);
asm
        CALL    UniqueString
        TEST    EAX,EAX
        JE      @@2
        MOV     ECX,[EAX-4]
        DEC     ECX
        JS      @@2
@@1:    MOVZX   EDX,BYTE PTR [EAX+ECX]
        MOV     DL,BYTE PTR [EDX+ToAnsiChars]
        MOV     BYTE PTR [EAX+ECX],DL
        DEC     ECX
        JNS     @@1
@@2:
end;

function Q_PStrToAnsi(P: PChar): PChar;
asm
        TEST    EAX,EAX
        JE      @@2
        PUSH    EAX
        JMP     @@1
@@0:    MOV     CL,BYTE PTR [EDX+ToAnsiChars]
        MOV     BYTE PTR [EAX],CL
        INC     EAX
@@1:    MOVZX   EDX,BYTE PTR [EAX]
        TEST    DL,DL
        JNE     @@0
        POP     EAX
@@2:
end;

procedure Q_StrToOem(var S: string);
asm
        CALL    UniqueString
        TEST    EAX,EAX
        JE      @@2
        MOV     ECX,[EAX-4]
        DEC     ECX
        JS      @@2
@@1:    MOVZX   EDX,BYTE PTR [EAX+ECX]
        MOV     DL,BYTE PTR [EDX+ToOemChars]
        MOV     BYTE PTR [EAX+ECX],DL
        DEC     ECX
        JNS     @@1
@@2:
end;

function Q_PStrToOem(P: PChar): PChar;
asm
        TEST    EAX,EAX
        JE      @@2
        PUSH    EAX
        JMP     @@1
@@0:    MOV     CL,BYTE PTR [EDX+ToOemChars]
        MOV     BYTE PTR [EAX],CL
        INC     EAX
@@1:    MOVZX   EDX,BYTE PTR [EAX]
        TEST    DL,DL
        JNE     @@0
        POP     EAX
@@2:
end;

function Q_PStrToAnsiL(P: PChar; L: Cardinal): PChar;
asm
        DEC     EDX
        JS      @@2
        PUSH    EBX
@@0:    MOVZX   EBX,BYTE PTR [EAX+EDX]
        MOV     CL,BYTE PTR [EBX+ToAnsiChars]
        MOV     BYTE PTR [EAX+EDX],CL
        DEC     EDX
        JNS     @@0
        POP     EBX
@@2:
end;

function Q_PStrToOemL(P: PChar; L: Cardinal): PChar;
asm
        DEC     EDX
        JS      @@2
        PUSH    EBX
@@0:    MOVZX   EBX,BYTE PTR [EAX+EDX]
        MOV     CL,BYTE PTR [EBX+ToOemChars]
        MOV     BYTE PTR [EAX+EDX],CL
        DEC     EDX
        JNS     @@0
        POP     EBX
@@2:
end;

procedure Q_Str2ToAnsi(const Source: string; var Dest: string);
asm
        TEST    EAX,EAX
        JE      @@4
        PUSH    EBX
        PUSH    EAX
        MOV     ECX,[EAX-4]
        MOV     EBX,EDX
        PUSH    ECX
        MOV     EAX,EDX
        XOR     EDX,EDX
        CALL    System.@LStrFromPCharLen
        POP     ECX
        POP     EAX
        DEC     ECX
        JS      @@3
        MOV     EDX,[EBX]
@@1:    MOVZX   EBX,BYTE PTR [EAX+ECX]
        MOV     BL,BYTE PTR [EBX+ToAnsiChars]
        MOV     BYTE PTR [EDX+ECX],BL
        DEC     ECX
        JNS     @@1
@@3:    POP     EBX
        RET
@@4:    MOV     EAX,EDX
        CALL    System.@LStrClr
end;

function Q_PStr2ToAnsi(Source, Dest: PChar): PChar;
asm
        TEST    EAX,EAX
        JE      @@2
        PUSH    EDX
        PUSH    EBX
        JMP     @@1
@@0:    MOV     CL,BYTE PTR [EBX+ToAnsiChars]
        MOV     BYTE PTR [EDX],CL
        INC     EAX
        INC     EDX
@@1:    MOVZX   EBX,BYTE PTR [EAX]
        TEST    BL,BL
        JNE     @@0
        POP     EBX
        POP     EAX
        RET
@@2:    MOV     EAX,EDX
end;

procedure Q_Str2ToOem(const Source: string; var Dest: string);
asm
        TEST    EAX,EAX
        JE      @@4
        PUSH    EBX
        PUSH    EAX
        MOV     ECX,[EAX-4]
        MOV     EBX,EDX
        PUSH    ECX
        MOV     EAX,EDX
        XOR     EDX,EDX
        CALL    System.@LStrFromPCharLen
        POP     ECX
        POP     EAX
        DEC     ECX
        JS      @@3
        MOV     EDX,[EBX]
@@1:    MOVZX   EBX,BYTE PTR [EAX+ECX]
        MOV     BL,BYTE PTR [EBX+ToOemChars]
        MOV     BYTE PTR [EDX+ECX],BL
        DEC     ECX
        JNS     @@1
@@3:    POP     EBX
        RET
@@4:    MOV     EAX,EDX
        CALL    System.@LStrClr
end;

function Q_PStr2ToOem(Source, Dest: PChar): PChar;
asm
        TEST    EAX,EAX
        JE      @@2
        PUSH    EDX
        PUSH    EBX
        JMP     @@1
@@0:    MOV     CL,BYTE PTR [EBX+ToOemChars]
        MOV     BYTE PTR [EDX],CL
        INC     EAX
        INC     EDX
@@1:    MOVZX   EBX,BYTE PTR [EAX]
        TEST    BL,BL
        JNE     @@0
        POP     EBX
        POP     EAX
        RET
@@2:    MOV     EAX,EDX
end;

function Q_PStr2ToAnsiL(Source, Dest: PChar; L: Cardinal): PChar;
asm
        DEC     ECX
        JS      @@2
        PUSH    EBX
@@1:    MOVZX   EBX,BYTE PTR [EAX+ECX]
        MOV     BL,BYTE PTR [EBX+ToAnsiChars]
        MOV     BYTE PTR [EDX+ECX],BL
        DEC     ECX
        JNS     @@1
        POP     EBX
@@2:    MOV     EAX,EDX
end;

function Q_PStr2ToOemL(Source, Dest: PChar; L: Cardinal): PChar;
asm
        DEC     ECX
        JS      @@2
        PUSH    EBX
@@1:    MOVZX   EBX,BYTE PTR [EAX+ECX]
        MOV     BL,BYTE PTR [EBX+ToOemChars]
        MOV     BYTE PTR [EDX+ECX],BL
        DEC     ECX
        JNS     @@1
        POP     EBX
@@2:    MOV     EAX,EDX
end;

function Q_ToAnsi(const OemStr: string): string;
var
  L: Integer;
begin
  L := Length(OemStr);
  SetString(Result, nil, L);
  Q_PStr2ToAnsiL(Pointer(OemStr), Pointer(Result), L);
end;

function Q_ToOem(const AnsiStr: string): string;
var
  L: Integer;
begin
  L := Length(AnsiStr);
  SetString(Result, nil, L);
  Q_PStr2ToOemL(Pointer(AnsiStr), Pointer(Result), L);
end;

function Q_PosStr(const FindString, SourceString: string; StartPos: Integer): Integer;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        PUSH    EDX
        TEST    EAX,EAX
        JE      @@qt
        TEST    EDX,EDX
        JE      @@qt0
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EAX,[EAX-4]
        MOV     EDX,[EDX-4]
        DEC     EAX
        SUB     EDX,EAX
        DEC     ECX
        SUB     EDX,ECX
        JNG     @@qt0
        XCHG    EAX,EDX
        ADD     EDI,ECX
        MOV     ECX,EAX
        JMP     @@nx
@@fr:   INC     EDI
        DEC     ECX
        JE      @@qt0
@@nx:   MOV     EBX,EDX
        MOV     AL,BYTE PTR [ESI]
@@lp1:  CMP     AL,BYTE PTR [EDI]
        JE      @@uu
        INC     EDI
        DEC     ECX
        JE      @@qt0
        CMP     AL,BYTE PTR [EDI]
        JE      @@uu
        INC     EDI
        DEC     ECX
        JE      @@qt0
        CMP     AL,BYTE PTR [EDI]
        JE      @@uu
        INC     EDI
        DEC     ECX
        JE      @@qt0
        CMP     AL,BYTE PTR [EDI]
        JE      @@uu
        INC     EDI
        DEC     ECX
        JNE     @@lp1
@@qt0:  XOR     EAX,EAX
@@qt:   POP     ECX
        POP     EBX
        POP     EDI
        POP     ESI
        RET
@@uu:   TEST    EDX,EDX
        JE      @@fd
@@lp2:  MOV     AL,BYTE PTR [ESI+EBX]
        CMP     AL,BYTE PTR [EDI+EBX]
        JNE     @@fr
        DEC     EBX
        JE      @@fd
        MOV     AL,BYTE PTR [ESI+EBX]
        CMP     AL,BYTE PTR [EDI+EBX]
        JNE     @@fr
        DEC     EBX
        JE      @@fd
        MOV     AL,BYTE PTR [ESI+EBX]
        CMP     AL,BYTE PTR [EDI+EBX]
        JNE     @@fr
        DEC     EBX
        JE      @@fd
        MOV     AL,BYTE PTR [ESI+EBX]
        CMP     AL,BYTE PTR [EDI+EBX]
        JNE     @@fr
        DEC     EBX
        JNE     @@lp2
@@fd:   LEA     EAX,[EDI+1]
        SUB     EAX,[ESP]
        POP     ECX
        POP     EBX
        POP     EDI
        POP     ESI
end;

function Q_PosText(const FindString, SourceString: string; StartPos: Integer): Integer;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        TEST    EAX,EAX
        JE      @@qt
        TEST    EDX,EDX
        JE      @@qt0
        MOV     ESI,EAX
        MOV     EDI,EDX
        PUSH    EDX
        MOV     EAX,[EAX-4]
        MOV     EDX,[EDX-4]
        DEC     EAX
        SUB     EDX,EAX
        DEC     ECX
        PUSH    EAX
        SUB     EDX,ECX
        JNG     @@qtx
        ADD     EDI,ECX
        MOV     ECX,EDX
        MOV     EDX,EAX
        MOVZX   EBX,BYTE PTR [ESI]
        MOV     AL,BYTE PTR [EBX+ToUpperChars]
@@lp1:  MOVZX   EBX,BYTE PTR [EDI]
        CMP     AL,BYTE PTR [EBX+ToUpperChars]
        JE      @@uu
@@fr:   INC     EDI
        DEC     ECX
        JE      @@qtx
        MOVZX   EBX,BYTE PTR [EDI]
        CMP     AL,BYTE PTR [EBX+ToUpperChars]
        JE      @@uu
        INC     EDI
        DEC     ECX
        JE      @@qtx
        MOVZX   EBX,BYTE PTR [EDI]
        CMP     AL,BYTE PTR [EBX+ToUpperChars]
        JE      @@uu
        INC     EDI
        DEC     ECX
        JE      @@qtx
        MOVZX   EBX,BYTE PTR [EDI]
        CMP     AL,BYTE PTR [EBX+ToUpperChars]
        JE      @@uu
        INC     EDI
        DEC     ECX
        JNE     @@lp1
@@qtx:  ADD     ESP,$08
@@qt0:  XOR     EAX,EAX
@@qt:   POP     EBX
        POP     EDI
        POP     ESI
        RET
@@ms:   MOVZX   EBX,BYTE PTR [ESI]
        MOV     AL,BYTE PTR [EBX+ToUpperChars]
        MOV     EDX,[ESP]
        JMP     @@fr
@@uu:   TEST    EDX,EDX
        JE      @@fd
@@lp2:  MOV     BL,BYTE PTR [ESI+EDX]
        MOV     AH,BYTE PTR [EDI+EDX]
        CMP     BL,AH
        JE      @@eq
        MOV     AL,BYTE PTR [EBX+ToUpperChars]
        MOVZX   EBX,AH
        CMP     AL,BYTE PTR [EBX+ToUpperChars]
        JNE     @@ms
@@eq:   DEC     EDX
        JNZ     @@lp2
@@fd:   LEA     EAX,[EDI+1]
        POP     ECX
        SUB     EAX,[ESP]
        POP     ECX
        POP     EBX
        POP     EDI
        POP     ESI
end;

function Q_PosLastStr(const FindString, SourceString: string;
  LastPos: Integer): Integer;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        TEST    EAX,EAX
        JE      @@qt
        TEST    EDX,EDX
        JE      @@qt0
        DEC     ECX
        JLE     @@qt0
        MOV     ESI,EAX
        LEA     EDI,[EDX-1]
        MOV     EAX,[EAX-4]
        MOV     EDX,[EDX-4]
        DEC     EAX
        CMP     ECX,EDX
        JL      @@nu
        MOV     ECX,EDX
@@nu:   SUB     ECX,EAX
        JLE     @@qt0
        JMP     @@ft
@@nx:   SUB     EDI,ECX
        DEC     ECX
        JE      @@qt0
@@ft:   MOV     DL,BYTE PTR [ESI]
@@lp1:  CMP     DL,BYTE PTR [EDI+ECX]
        JE      @@uu
        DEC     ECX
        JE      @@qt0
        CMP     DL,BYTE PTR [EDI+ECX]
        JE      @@uu
        DEC     ECX
        JE      @@qt0
        CMP     DL,BYTE PTR [EDI+ECX]
        JE      @@uu
        DEC     ECX
        JE      @@qt0
        CMP     DL,BYTE PTR [EDI+ECX]
        JE      @@uu
        DEC     ECX
        JNE     @@lp1
@@qt0:  XOR     EAX,EAX
@@qt:   POP     EBX
        POP     EDI
        POP     ESI
        RET
@@uu:   TEST    EAX,EAX
        JE      @@fd
        ADD     EDI,ECX
        MOV     EBX,EAX
@@lp2:  MOV     DL,BYTE PTR [ESI+EBX]
        CMP     DL,BYTE PTR [EDI+EBX]
        JNE     @@nx
        DEC     EBX
        JE      @@fd
        MOV     DL,BYTE PTR [ESI+EBX]
        CMP     DL,BYTE PTR [EDI+EBX]
        JNE     @@nx
        DEC     EBX
        JE      @@fd
        MOV     DL,BYTE PTR [ESI+EBX]
        CMP     DL,BYTE PTR [EDI+EBX]
        JNE     @@nx
        DEC     EBX
        JE      @@fd
        MOV     DL,BYTE PTR [ESI+EBX]
        CMP     DL,BYTE PTR [EDI+EBX]
        JNE     @@nx
        DEC     EBX
        JNE     @@lp2
@@fd:   MOV     EAX,ECX
        POP     EBX
        POP     EDI
        POP     ESI
end;

function Q_PosLastText(const FindString, SourceString: string;
  LastPos: Integer): Integer;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        PUSH    EBP
        TEST    EAX,EAX
        JE      @@qt
        TEST    EDX,EDX
        JE      @@qt0
        DEC     ECX
        JLE     @@qt0
        MOV     ESI,EAX
        LEA     EDI,[EDX-1]
        MOV     EAX,[EAX-4]
        MOV     EDX,[EDX-4]
        DEC     EAX
        CMP     ECX,EDX
        JL      @@nu
        MOV     ECX,EDX
@@nu:   SUB     ECX,EAX
        JLE     @@qt0
        JMP     @@ft
@@nx:   SUB     EDI,ECX
        DEC     ECX
        JE      @@qt0
@@ft:   MOVZX   EBP,BYTE PTR [ESI]
        MOV     DL,BYTE PTR [EBP+ToUpperChars]
@@lp1:  MOVZX   EBP,BYTE PTR [EDI+ECX]
        CMP     DL,BYTE PTR [EBP+ToUpperChars]
        JE      @@uu
        DEC     ECX
        JE      @@qt0
        MOVZX   EBP,BYTE PTR [EDI+ECX]
        CMP     DL,BYTE PTR [EBP+ToUpperChars]
        JE      @@uu
        DEC     ECX
        JE      @@qt0
        MOVZX   EBP,BYTE PTR [EDI+ECX]
        CMP     DL,BYTE PTR [EBP+ToUpperChars]
        JE      @@uu
        DEC     ECX
        JE      @@qt0
        MOVZX   EBP,BYTE PTR [EDI+ECX]
        CMP     DL,BYTE PTR [EBP+ToUpperChars]
        JE      @@uu
        DEC     ECX
        JNE     @@lp1
@@qt0:  XOR     EAX,EAX
@@qt:   POP     EBP
        POP     EBX
        POP     EDI
        POP     ESI
        RET
@@uu:   TEST    EAX,EAX
        JE      @@fd
        ADD     EDI,ECX
        MOV     EBX,EAX
@@lp2:  MOVZX   EDX,BYTE PTR [ESI+EBX]
        MOVZX   EBP,BYTE PTR [EDI+EBX]
        CMP     EDX,EBP
        JE      @@ws
        MOV     DL,BYTE PTR [EDX+ToUpperChars]
        CMP     DL,BYTE PTR [EBP+ToUpperChars]
        JNE     @@nx
@@ws:   DEC     EBX
        JNE     @@lp2
@@fd:   MOV     EAX,ECX
        POP     EBP
        POP     EBX
        POP     EDI
        POP     ESI
end;

procedure Q_InitTablePosStr(const FindString: string);
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EAX
        CALL    SysInit.@GetTls
        MOV     ESI,EAX
        ADD     EAX,OFFSET TableFindString
        TEST    EDI,EDI
        JE      @@nomove
        MOV     ECX,[EDI-4]
        TEST    ECX,ECX
        JE      @@nomove
        PUSH    ECX
        MOV     EDX,EDI
        CALL    System.@LStrFromPCharLen
        ADD     ESI,OFFSET PosTable
        POP     ECX
        PUSH    EBX
        LEA     EBX,[EDI+ECX]
        TEST    ECX,$FFFFFF00
        JNE     @@su
        MOV     EAX,ECX
        SHL     EAX,8
        OR      EAX,ECX
        MOV     ECX,EAX
        SHL     EAX,16
        OR      EAX,ECX
        JMP     @@fi
@@su:   MOV     EAX,$FFFFFFFF
@@fi:   MOV     EDI,ESI
        MOV     ECX,64
        REP     STOSD
        XOR     EDX,EDX
        MOVZX   ECX,AL
        SUB     EBX,ECX
        DEC     ECX
        JE      @@nl
@@lp:   MOVZX   EDX,BYTE PTR [EBX]
        MOV     BYTE PTR [ESI+EDX],CL
        INC     EBX
        DEC     ECX
        JNE     @@lp
@@nl:   POP     EBX
        POP     ESI
        POP     EDI
        RET
@@nomove:
        CALL    System.@LStrClr
        POP     ESI
        POP     EDI
end;

function Q_TablePosStr(const SourceString: string; var LastPos: Integer): Boolean;
asm
        TEST    EAX,EAX
        JE      @@qt0
        PUSH    EBX
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EAX
        MOV     EBX,EDX
        CALL    SysInit.@GetTls
        MOV     ESI,EAX
        ADD     EAX,OFFSET TableFindString
        MOV     EAX,[EAX]
        TEST    EAX,EAX
        JE      @@qt1
        MOV     ECX,[EDI-4]
        PUSH    EDI
        PUSH    EBX
        MOV     EDX,[EAX-4]
        MOV     EBX,[EBX]
        DEC     EDX
        TEST    EBX,EBX
        JLE     @@zp
        ADD     EBX,EDX
        SUB     ECX,EBX
@@rt:   SUB     ECX,EDX
        JLE     @@qt2
        ADD     EDI,EBX
        ADD     EDI,EDX
        ADD     ESI,OFFSET PosTable
        PUSH    EAX
        MOV     AL,BYTE PTR [EAX+EDX]
@@lp1:  MOVZX   EBX,BYTE PTR [EDI]
        CMP     AL,BL
        JE      @@eq
@@fr:   MOVZX   EBX,BYTE PTR [ESI+EBX]
        ADD     EDI,EBX
        SUB     ECX,EBX
        JG      @@lp1
        JMP     @@qt3
@@eq:   TEST    EDX,EDX
        JE      @@fd
        MOV     EAX,[ESP]
        PUSH    EDI
        SUB     EDI,EDX
        PUSH    EBX
        MOV     BL,BYTE PTR [EDI]
        CMP     BYTE PTR [EAX],BL
        JNE     @@ms1
        PUSH    ECX
        LEA     ECX,[EDX-1]
        TEST    ECX,ECX
        JE      @@ok
@@lp2:  MOV     BL,BYTE PTR [EDI+ECX]
        CMP     BYTE PTR [EAX+ECX],BL
        JNE     @@ms0
        DEC     ECX
        JNE     @@lp2
@@ok:   ADD     ESP,12
@@fd:   POP     ECX
        POP     EBX
        INC     EDI
        POP     ESI
        SUB     EDI,ESI
        MOV     [EBX],EDI
        MOV     EAX,1
@@qt1:  POP     ESI
        POP     EDI
        POP     EBX
@@qt0:  RET
@@zp:   XOR     EBX,EBX
        JMP     @@rt
@@ms0:  POP     ECX
@@ms1:  POP     EBX
        POP     EDI
        MOV     EAX,EBX
        JMP     @@fr
@@qt3:  POP     ECX
@@qt2:  XOR     EAX,EAX
        ADD     ESP,8
        POP     ESI
        POP     EDI
        POP     EBX
end;

procedure Q_InitTablePosText(const FindString: string);
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EAX
        CALL    SysInit.@GetTls
        MOV     ESI,EAX
        ADD     EAX,OFFSET TableFindString
        TEST    EDI,EDI
        JE      @@nomove
        MOV     ECX,[EDI-4]
        TEST    ECX,ECX
        JE      @@nomove
        PUSH    EBX
        PUSH    ECX
        XOR     EDX,EDX
        MOV     EBX,EAX
        CALL    System.@LStrFromPCharLen
        MOV     EAX,EDI
        MOV     EDX,EBX
        MOV     ECX,[ESP]
        CALL    Q_StrUpperMoveL
        POP     ECX
        MOV     EDI,[EBX]
        ADD     ESI,OFFSET PosTable
        LEA     EBX,[EDI+ECX]
        TEST    ECX,$FFFFFF00
        JNE     @@su
        MOV     EAX,ECX
        SHL     EAX,8
        OR      EAX,ECX
        MOV     ECX,EAX
        SHL     EAX,16
        OR      EAX,ECX
        JMP     @@fi
@@su:   MOV     EAX,$FFFFFFFF
@@fi:   MOV     EDI,ESI
        MOV     ECX,64
        REP     STOSD
        XOR     EDX,EDX
        MOVZX   ECX,AL
        SUB     EBX,ECX
        DEC     ECX
        JE      @@nl
@@lp:   MOVZX   EDX,BYTE PTR [EBX]
        MOV     BYTE PTR [ESI+EDX],CL
        INC     EBX
        DEC     ECX
        JNE     @@lp
@@nl:   POP     EBX
        POP     ESI
        POP     EDI
        RET
@@nomove:
        CALL    System.@LStrClr
        POP     ESI
        POP     EDI
end;

function Q_TablePosText(const SourceString: string; var LastPos: Integer): Boolean;
asm
        TEST    EAX,EAX
        JE      @@qt0
        PUSH    EBX
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EAX
        MOV     EBX,EDX
        CALL    SysInit.@GetTls
        MOV     ESI,EAX
        ADD     EAX,OFFSET TableFindString
        MOV     EAX,[EAX]
        TEST    EAX,EAX
        JE      @@qt1
        MOV     ECX,[EDI-4]
        PUSH    EDI
        PUSH    EBX
        MOV     EDX,[EAX-4]
        MOV     EBX,[EBX]
        DEC     EDX
        TEST    EBX,EBX
        JLE     @@zp
        ADD     EBX,EDX
        SUB     ECX,EBX
@@rt:   SUB     ECX,EDX
        JLE     @@qt2
        ADD     EDI,EBX
        ADD     EDI,EDX
        ADD     ESI,OFFSET PosTable
        PUSH    EAX
        MOV     AL,BYTE PTR [EAX+EDX]
@@lp1:  MOVZX   EBX,BYTE PTR [EDI]
        MOVZX   EBX,BYTE PTR [EBX+ToUpperChars]
        CMP     AL,BL
        JE      @@eq
@@fr:   MOVZX   EBX,BYTE PTR [ESI+EBX]
        ADD     EDI,EBX
        SUB     ECX,EBX
        JG      @@lp1
        JMP     @@qt3
@@eq:   TEST    EDX,EDX
        JE      @@fd
        MOV     EAX,[ESP]
        PUSH    EDI
        SUB     EDI,EDX
        PUSH    EBX
        MOVZX   EBX,BYTE PTR [EDI]
        MOV     BL,BYTE PTR [EBX+ToUpperChars]
        CMP     BYTE PTR [EAX],BL
        JNE     @@ms1
        PUSH    ECX
        LEA     ECX,[EDX-1]
        TEST    ECX,ECX
        JE      @@ok
@@lp2:  MOVZX   EBX,BYTE PTR [EDI+ECX]
        MOV     BL,BYTE PTR [EBX+ToUpperChars]
        CMP     BYTE PTR [EAX+ECX],BL
        JNE     @@ms0
        DEC     ECX
        JNE     @@lp2
@@ok:   ADD     ESP,12
@@fd:   POP     ECX
        POP     EBX
        INC     EDI
        POP     ESI
        SUB     EDI,ESI
        MOV     [EBX],EDI
        MOV     EAX,1
@@qt1:  POP     ESI
        POP     EDI
        POP     EBX
@@qt0:  RET
@@zp:   XOR     EBX,EBX
        JMP     @@rt
@@ms0:  POP     ECX
@@ms1:  POP     EBX
        POP     EDI
        MOV     EAX,EBX
        JMP     @@fr
@@qt3:  POP     ECX
@@qt2:  XOR     EAX,EAX
        ADD     ESP,8
        POP     ESI
        POP     EDI
        POP     EBX
end;

function Q_ReplaceStr(const SourceString, FindString, ReplaceString: string): string;
var
  P,PS: PChar;
  L,L1,L2,Cnt: Integer;
  I,J,K,M: Integer;
begin
  L1 := Length(FindString);
  Cnt := 0;
  I := Q_PosStr(FindString,SourceString,1);
  while I <> 0 do
  begin
    Inc(I,L1);
    asm
      PUSH    I
    end;
    Inc(Cnt);
    I := Q_PosStr(FindString,SourceString,I);
  end;
  if Cnt <> 0 then
  begin
    L := Length(SourceString);
    L2 := Length(ReplaceString);
    J := L+1;
    Inc(L,(L2-L1)*Cnt);
    if L <> 0 then
    begin
      SetString(Result,nil,L);
      P := Pointer(Result);
      Inc(P, L);
      PS := Pointer(LongWord(SourceString)-1);
      if L2 <= 32 then
        for I := 0 to Cnt-1 do
        begin
          asm
            POP     K
          end;
          M := J-K;
          if M > 0 then
          begin
            Dec(P,M);
            Q_CopyMem(@PS[K],P,M);
          end;
          Dec(P,L2);
          Q_TinyCopy(Pointer(ReplaceString),P,L2);
          J := K-L1;
        end
      else
        for I := 0 to Cnt-1 do
        begin
          asm
            POP     K
          end;
          M := J-K;
          if M > 0 then
          begin
            Dec(P,M);
            Q_CopyMem(@PS[K],P,M);
          end;
          Dec(P,L2);
          Q_CopyMem(Pointer(ReplaceString),P,L2);
          J := K-L1;
        end;
      Dec(J);
      if J > 0 then
        Q_CopyMem(Pointer(SourceString),Pointer(Result),J);
    end else
      Result := '';
  end else
    Result := SourceString;
end;

function Q_ReplaceText(const SourceString, FindString, ReplaceString: string): string;
var
  P,PS: PChar;
  L,L1,L2,Cnt: Integer;
  I,J,K,M: Integer;
begin
  L1 := Length(FindString);
  Cnt := 0;
  I := Q_PosText(FindString,SourceString,1);
  while I <> 0 do
  begin
    Inc(I,L1);
    asm
      PUSH    I
    end;
    Inc(Cnt);
    I := Q_PosText(FindString,SourceString,I);
  end;
  if Cnt <> 0 then
  begin
    L := Length(SourceString);
    L2 := Length(ReplaceString);
    J := L+1;
    Inc(L,(L2-L1)*Cnt);
    if L <> 0 then
    begin
      SetString(Result,nil,L);
      P := Pointer(Result);
      Inc(P, L);
      PS := Pointer(LongWord(SourceString)-1);
      if L2 <= 32 then
        for I := 0 to Cnt-1 do
        begin
          asm
            POP     K
          end;
          M := J-K;
          if M > 0 then
          begin
            Dec(P,M);
            Q_CopyMem(@PS[K],P,M);
          end;
          Dec(P,L2);
          Q_TinyCopy(Pointer(ReplaceString),P,L2);
          J := K-L1;
        end
      else
        for I := 0 to Cnt-1 do
        begin
          asm
            POP     K
          end;
          M := J-K;
          if M > 0 then
          begin
            Dec(P,M);
            Q_CopyMem(@PS[K],P,M);
          end;
          Dec(P,L2);
          Q_CopyMem(Pointer(ReplaceString),P,L2);
          J := K-L1;
        end;
      Dec(J);
      if J > 0 then
        Q_CopyMem(Pointer(SourceString),Pointer(Result),J);
    end else
      Result := '';
  end else
    Result := SourceString;
end;

function Q_ReplaceFirstStr(var S: string; const FindString, ReplaceString: string;
  StartPos: Integer): Integer;
begin
  Result := Q_PosStr(FindString, S, StartPos);
  if Result <> 0 then
    Q_PasteStr(S, Result, Length(FindString), ReplaceString);
end;

function Q_ReplaceFirstText(var S: string; const FindString, ReplaceString: string;
  StartPos: Integer): Integer;
begin
  Result := Q_PosText(FindString, S, StartPos);
  if Result <> 0 then
    Q_PasteStr(S, Result, Length(FindString), ReplaceString);
end;

function Q_ReplaceLastStr(var S: string; const FindString, ReplaceString: string;
  LastPos: Integer): Integer;
begin
  Result := Q_PosLastStr(FindString, S, LastPos);
  if Result <> 0 then
    Q_PasteStr(S, Result, Length(FindString), ReplaceString);
end;

function Q_ReplaceLastText(var S: string; const FindString, ReplaceString: string;
  LastPos: Integer): Integer;
begin
  Result := Q_PosLastText(FindString, S, LastPos);
  if Result <> 0 then
    Q_PasteStr(S, Result, Length(FindString), ReplaceString);
end;

function Q_DeleteStr(var S: string; const SubStrToDel: string): Integer;
var
  I,L1: Integer;
begin
  L1 := Length(SubStrToDel);
  I := Q_PosStr(SubStrToDel,S,1);
  Result := 0;
  while I <> 0 do
  begin
    Q_Delete(S,I,L1);
    I := Q_PosStr(SubStrToDel,S,I);
    Inc(Result);
  end;
end;

function Q_DeleteText(var S: string; const SubStrToDel: string): Integer;
var
  I,L1: Integer;
begin
  L1 := Length(SubStrToDel);
  I := Q_PosText(SubStrToDel,S,1);
  Result := 0;
  while I <> 0 do
  begin
    Q_Delete(S,I,L1);
    I := Q_PosText(SubStrToDel,S,I);
    Inc(Result);
  end;
end;

function Q_DeleteFirstStr(var S: string; const SubStrToDel: string;
  StartPos: Integer): Integer;
begin
  Result := Q_PosStr(SubStrToDel, S, StartPos);
  if Result <> 0 then
    Q_Delete(S, Result, Length(SubStrToDel));
end;

function Q_DeleteFirstText(var S: string; const SubStrToDel: string;
  StartPos: Integer): Integer;
begin
  Result := Q_PosText(SubStrToDel, S, StartPos);
  if Result <> 0 then
    Q_Delete(S, Result, Length(SubStrToDel));
end;

function Q_DeleteLastStr(var S: string; const SubStrToDel: string;
  LastPos: Integer): Integer;
begin
  Result := Q_PosLastStr(SubStrToDel, S, LastPos);
  if Result <> 0 then
    Q_Delete(S, Result, Length(SubStrToDel));
end;

function Q_DeleteLastText(var S: string; const SubStrToDel: string;
  LastPos: Integer): Integer;
begin
  Result := Q_PosLastText(SubStrToDel, S, LastPos);
  if Result <> 0 then
    Q_Delete(S, Result, Length(SubStrToDel));
end;

function Q_ReplaceChar(var S: string; ChOld, ChNew: Char): Integer;
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,ECX
        MOV     ESI,EDX
        CALL    UniqueString
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,EBX
        MOV     EDX,ESI
        MOV     EBX,[EAX-4]
        TEST    EBX,EBX
        JE      @@zq
        LEA     ESI,[EAX-1]
        XOR     EAX,EAX
@@lp:   CMP     DL,BYTE PTR [ESI+EBX]
        JE      @@fn
        DEC     EBX
        JNE     @@lp
        POP     ESI
        POP     EBX
        RET
@@fn:   MOV     BYTE PTR [ESI+EBX],CL
        INC     EAX
        DEC     EBX
        JNE     @@lp
        POP     ESI
        POP     EBX
        RET
@@zq:   XOR     EAX,EAX
@@qt:   POP     ESI
        POP     EBX
end;

procedure Int256Chars(P: Pointer);
asm
        MOV     ECX,8
        MOV     EDX,$03020100
@@lp:   MOV     [EAX],EDX
        ADD     EDX,$04040404
        MOV     [EAX+4],EDX
        ADD     EDX,$04040404
        MOV     [EAX+8],EDX
        ADD     EDX,$04040404
        MOV     [EAX+12],EDX
        ADD     EDX,$04040404
        MOV     [EAX+16],EDX
        ADD     EDX,$04040404
        MOV     [EAX+20],EDX
        ADD     EDX,$04040404
        MOV     [EAX+24],EDX
        ADD     EDX,$04040404
        MOV     [EAX+28],EDX
        ADD     EDX,$04040404
        ADD     EAX,32
        DEC     ECX
        JNE     @@lp
end;

procedure Q_ReplaceChars(var S: string; const StrChOld, StrChNew: string);
var
  Map: array[#0..#255] of Char;
  I,J: Integer;
  P: PChar;
begin
  J := Length(StrChOld);
  Int256Chars(@Map);
  if J <> Length(StrChNew) then
    raise Exception.Create('Неправильный вызов функции Q_ReplaceChars');
  for I := 1 to J do
    Map[StrChOld[I]] := StrChNew[I];
  if J > 0 then
  begin
    UniqueString(S);
    P := Pointer(S);
    for I := 1 to Length(S) do
    begin
      P^ := Map[P^];
      Inc(P);
    end;
  end;
end;

procedure Q_ReplaceCharsByOneChar(var S: string; const ChOldSet: TCharSet; ChNew: Char);
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EDX
        MOV     BL,CL
        CALL    UniqueString
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        MOV     EDI,EAX
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EAX
@@lp1:  MOVZX   EDX,BYTE PTR [EAX]
        BT      [ESI],EDX
        JC      @@rp
@@nx1:  MOV     BYTE PTR [EDI],DL
        INC     EAX
        INC     EDI
        DEC     ECX
        JNE     @@lp1
@@nx2:  POP     EAX
        MOV     BYTE PTR [EDI],0
        SUB     EDI,EAX
        MOV     [EAX-4],EDI
@@qt:   POP     EDI
        POP     ESI
        POP     EBX
        RET
@@rp:   MOV     BYTE PTR [EDI],BL
        INC     EAX
        INC     EDI
        DEC     ECX
        JE      @@nx2
@@lp2:  MOVZX   EDX,BYTE PTR [EAX]
        BT      [ESI],EDX
        JNC     @@nx1
        INC     EAX
        DEC     ECX
        JNE     @@lp2
        JMP     @@nx2
end;

function Q_StrScan(const S: string; Ch: Char; StartPos: Integer): Integer;
asm
        TEST    EAX,EAX
        JE      @@qt
        PUSH    EDI
        MOV     EDI,EAX
        LEA     EAX,[ECX-1]
        MOV     ECX,[EDI-4]
        SUB     ECX,EAX
        JLE     @@m1
        PUSH    EDI
        ADD     EDI,EAX
        MOV     EAX,EDX
        POP     EDX
        REPNE   SCASB
        JNE     @@m1
        MOV     EAX,EDI
        SUB     EAX,EDX
        POP     EDI
        RET
@@m1:   POP     EDI
        XOR     EAX,EAX
@@qt:
end;

function Q_PStrScan(P: Pointer; Ch: Char; StartPos: Integer): Integer;
asm
        TEST    EAX,EAX
        JE      @@qt
        PUSH    EDI
        MOV     EDI,EAX
        LEA     EAX,[ECX-1]
        MOV     ECX,[EDI-4]
        SUB     ECX,EAX
        JLE     @@m1
        PUSH    EDI
        ADD     EDI,EAX
        MOV     EAX,EDX
        POP     EDX
        REPNE   SCASB
        JNE     @@m1
        MOV     EAX,EDI
        SUB     EAX,EDX
        POP     EDI
        RET
@@m1:   POP     EDI
        XOR     EAX,EAX
@@qt:
end;

function Q_StrRScan(const S: string; Ch: Char; LastPos: Integer): Integer;
asm
        TEST    EAX,EAX
        JE      @@qt
        PUSH    EBX
        DEC     ECX
        JS      @@m1
        MOV     EBX,[EAX-4]
        PUSH    EDI
        CMP     ECX,EBX
        JA      @@ch
	TEST	ECX,ECX
	JE	@@m2
@@nx:   LEA     EDI,[EAX+ECX-1]
        STD
        XCHG    EAX,EDX
        REPNE   SCASB
        INC     EDI
        CLD
        CMP     AL,BYTE PTR [EDI]
        JNE     @@m2
        SUB     EDI,EDX
        MOV     EAX,EDI
        POP     EDI
        INC     EAX
        POP     EBX
        RET
@@ch:   MOV     ECX,EBX
	TEST	EBX,EBX
        JNE	@@nx
@@m2:   POP     EDI
@@m1:   XOR     EAX,EAX
	POP     EBX
@@qt:
end;

function Q_PStrRScan(P: Pointer; Ch: Char; LastPos: Integer): Integer;
asm
        TEST    EAX,EAX
        JE      @@qt
        PUSH    EBX
        DEC     ECX
        JS      @@m1
        MOV     EBX,[EAX-4]
        PUSH    EDI
        CMP     ECX,EBX
        JA      @@ch
	TEST	ECX,ECX
	JE	@@m2
@@nx:   LEA     EDI,[EAX+ECX-1]
        STD
        XCHG    EAX,EDX
        REPNE   SCASB
        INC     EDI
        CLD
        CMP     AL,BYTE PTR [EDI]
        JNE     @@m2
        SUB     EDI,EDX
        MOV     EAX,EDI
        POP     EDI
        INC     EAX
        POP     EBX
        RET
@@ch:   MOV     ECX,EBX
	TEST	EBX,EBX
        JNE	@@nx
@@m2:   POP     EDI
@@m1:   XOR     EAX,EAX
	POP     EBX
@@qt:
end;

function Q_StrSpn(const S, Delimiters: string; StartPos: Cardinal): Integer; overload;
var
  DelimsMap: PDelimsMap;
  I: Integer;
  L: LongWord;
  P: PChar;
begin
  asm
        CALL    SysInit.@GetTls
        LEA     EAX,[EAX+UDelimsMap]
        MOV     DelimsMap,EAX
  end;
  L := Length(Delimiters);
  if L <> 0 then
  begin
    Q_FillLong(0,DelimsMap,64);
    for I := 1 to L do
      DelimsMap^[Delimiters[I]] := True;
  end;
  L := Length(S);
  if L >= StartPos then
  begin
    P := Pointer(S);
    Inc(L,LongWord(P));
    Inc(P,StartPos-1);
    while (LongWord(P)<L) and DelimsMap^[P^] do Inc(P);
    if LongWord(P) < L then
      Result := Integer(P)-Integer(Pointer(S))+1
    else
      Result := 0;
  end else
    Result := 0;
end;

function Q_StrSpn(const S: string; StartPos: Cardinal; const Delimiters: TCharSet): Integer; overload;
var
  L: LongWord;
  P: PChar;
begin
  L := Length(S);
  if L >= StartPos then
  begin
    P := Pointer(S);
    Inc(L,LongWord(P));
    Inc(P,StartPos-1);
    while (LongWord(P)<L) and (P^ in Delimiters) do Inc(P);
    if LongWord(P) < L then
      Result := Integer(P)-Integer(Pointer(S))+1
    else
      Result := 0;
  end else
    Result := 0;
end;

function Q_StrCSpn(const S, Delimiters: string; StartPos: Cardinal): Integer; overload;
var
  DelimsMap: PDelimsMap;
  I: Integer;
  L: LongWord;
  P: PChar;
begin
  asm
        CALL    SysInit.@GetTls
        LEA     EAX,[EAX+UDelimsMap]
        MOV     DelimsMap,EAX
  end;
  L := Length(Delimiters);
  if L <> 0 then
  begin
    Q_FillLong(0,DelimsMap,64);
    for I := 1 to L do
      DelimsMap^[Delimiters[I]] := True;
  end;
  L := Length(S);
  if L >= StartPos then
  begin
    P := Pointer(S);
    Inc(L,LongWord(P));
    Inc(P,StartPos-1);
    while (LongWord(P)<L) and not DelimsMap^[P^] do Inc(P);
    if LongWord(P) < L then
      Result := Integer(P)-Integer(Pointer(S))+1
    else
      Result := 0;
  end else
    Result := 0;
end;

function Q_StrCSpn(const S: string; StartPos: Cardinal; const Delimiters: TCharSet): Integer; overload;
var
  L: LongWord;
  P: PChar;
begin
  L := Length(S);
  if L >= StartPos then
  begin
    P := Pointer(S);
    Inc(L,LongWord(P));
    Inc(P,StartPos-1);
    while (LongWord(P)<L) and not (P^ in Delimiters) do Inc(P);
    if LongWord(P) < L then
      Result := Integer(P)-Integer(Pointer(S))+1
    else
      Result := 0;
  end else
    Result := 0;
end;

procedure Q_DelCharInPlace(var S: string; Ch: Char);
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EAX
        MOV     EBX,EDX
        CALL    UniqueString
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        MOV     EDX,EAX
        TEST    ECX,ECX
        JE      @@zq0
@@lp1:  MOV     AL,BYTE PTR [EDX]
        CMP     BL,AL
        JE      @@cf
        INC     EDX
        DEC     ECX
        JNE     @@lp1
        JMP     @@qt
@@cf:   MOV     ESI,EDX
        INC     EDX
        DEC     ECX
        JE      @@rt
@@lp2:  MOV     AL,BYTE PTR [EDX]
        CMP     BL,AL
        JE      @@nx
        MOV     BYTE PTR [ESI],AL
        INC     ESI
@@nx:   INC     EDX
        DEC     ECX
        JNE     @@lp2
@@rt:   POP     EAX
        MOV     EBX,[EAX]
        MOV     BYTE PTR [ESI],0
        SUB     ESI,EBX
        JE      @@zq1
        MOV     [EBX-4],ESI
        POP     ESI
        POP     EBX
        RET
@@qt:   POP     ECX
        POP     ESI
        POP     EBX
        RET
@@zq0:  POP     EAX
@@zq1:  CALL    System.@LStrClr
        POP     ESI
        POP     EBX
end;

function Q_DelChar(const S: string; Ch: Char): string;
asm
        PUSH    ESI
        PUSH    EBX
        PUSH    EDI
        MOV     ESI,ECX
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@qt
        MOV     EBX,EAX
        MOV     EDI,EDX
        XOR     EDX,EDX
        MOV     EAX,ESI
        CALL    System.@LStrFromPCharLen
        MOV     EDX,EDI
        MOV     ECX,[EBX-4]
        MOV     EDI,[ESI]
@@lp:   MOV     AL,BYTE PTR [EBX]
        CMP     DL,AL
        JE      @@nx
        MOV     BYTE PTR [EDI],AL
        INC     EDI
@@nx:   INC     EBX
        DEC     ECX
        JNE     @@lp
        MOV     EAX,[ESI]
        MOV     BYTE PTR [EDI],0
        SUB     EDI,EAX
        JE      @@qt
        MOV     [EAX-4],EDI
        POP     EDI
        POP     EBX
        POP     ESI
        RET
@@qt:   MOV     EAX,ESI
        CALL    System.@LStrClr
        POP     EDI
        POP     EBX
        POP     ESI
end;

procedure Q_Delete(var S: string; Index, Count: Integer);
asm
        PUSH    EBX
        PUSH    ESI
        XOR     EBX,EBX
        CMP     ECX,EBX
        JLE     @@qt
        MOV     EBX,[EAX]
        TEST    EBX,EBX
        JE      @@qt
        MOV     ESI,[EBX-4]
        DEC     EDX
        JS      @@qt
        SUB     ESI,EDX
        JNG     @@qt
        SUB     ESI,ECX
        JLE     @@zq
        PUSH    ECX
        MOV     EBX,EDX
        CALL    UniqueString
        POP     ECX
        PUSH    EAX
        MOV     EDX,ESI
        ADD     EAX,EBX
        SHR     ESI,2
        JE      @@nx
@@lp:   MOV     BL,[EAX+ECX]
        MOV     [EAX],BL
        MOV     BL,[EAX+ECX+1]
        MOV     [EAX+1],BL
        MOV     BL,[EAX+ECX+2]
        MOV     [EAX+2],BL
        MOV     BL,[EAX+ECX+3]
        MOV     [EAX+3],BL
        ADD     EAX,4
        DEC     ESI
        JNE     @@lp
@@nx:   AND     EDX,3
        JMP     DWORD PTR @@tV[EDX*4]
@@zq:   CALL    System.@LStrClr
@@qt:   POP     ESI
        POP     EBX
        RET
@@tV:   DD      @@t0,@@t1,@@t2,@@t3
@@t1:   MOV     BL,[EAX+ECX]
        MOV     [EAX],BL
        INC     EAX
        JMP     @@t0
@@t2:   MOV     BL,[EAX+ECX]
        MOV     [EAX],BL
        MOV     BL,[EAX+ECX+1]
        MOV     [EAX+1],BL
        ADD     EAX,2
        JMP     @@t0
@@t3:   MOV     BL,[EAX+ECX]
        MOV     [EAX],BL
        MOV     BL,[EAX+ECX+1]
        MOV     [EAX+1],BL
        MOV     BL,[EAX+ECX+2]
        MOV     [EAX+2],BL
        ADD     EAX,3
@@t0:   POP     EDX
        MOV     BYTE PTR [EAX],0
        SUB     EAX,EDX
        MOV     [EDX-4],EAX
        POP     ESI
        POP     EBX
end;

procedure Q_DelChars(var S: string; const CharsToRemove: string); overload;
var
  Map: array[#0..#255] of Boolean;
  I,L: Integer;
  P,P1: PChar;
  PK: ^LongWord;
begin
  Q_FillLong(0,@Map,64);
  for I := 1 to Length(CharsToRemove) do
    Map[CharsToRemove[I]] := True;
  I := 1;
  L := Length(S);
  while (I<=L) and not Map[S[I]] do Inc(I);
  if I <= L then
  begin
    UniqueString(S);
    P := Pointer(S);
    PK := Pointer(S);
    Inc(P,I-1);
    P1 := P;
    Inc(LongWord(PK),L);
    Inc(P1);
    while P1 < PK do
    begin
      if not Map[P1^] then
      begin
        P^ := P1^;
        Inc(P);
      end;
      Inc(P1);
    end;
    P1 := Pointer(S);
    if P <> P1 then
    begin
      PK := Pointer(S);
      P^ := #0;
      Dec(PK);
      PK^ := LongWord(P)-LongWord(P1);
    end else
      S := '';
  end;
end;

procedure Q_DelChars(var S: string; const CharsToRemove: TCharSet); overload;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EDX
        PUSH    EAX
        CALL    UniqueString
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        MOV     EDI,EAX
        TEST    ECX,ECX
        JE      @@zq0
@@lp1:  MOVZX   EDX,BYTE PTR [EAX]
        BT      [ESI],EDX
        JC      @@rp
@@nx1:  MOV     BYTE PTR [EDI],DL
        INC     EAX
        INC     EDI
        DEC     ECX
        JNE     @@lp1
@@nx2:  POP     EAX
        MOV     ECX,[EAX]
        MOV     BYTE PTR [EDI],0
        SUB     EDI,ECX
        JE      @@zq1
        MOV     [ECX-4],EDI
        POP     EDI
        POP     ESI
        RET
@@qt:   POP     ECX
        POP     EDI
        POP     ESI
        RET
@@zq0:  POP     EAX
@@zq1:  CALL    System.@LStrClr
        POP     EDI
        POP     ESI
        RET
@@rp:   INC     EAX
        DEC     ECX
        JE      @@nx2
@@lp2:  MOVZX   EDX,BYTE PTR [EAX]
        BT      [ESI],EDX
        JNC     @@nx1
        INC     EAX
        DEC     ECX
        JNE     @@lp2
        JMP     @@nx2
end;

procedure Q_KeepChars(var S: string; const CharsToKeep: string); overload;
var
  Map: array[#0..#255] of Boolean;
  I,L: Integer;
  P,P1: PChar;
  PK: ^LongWord;
begin
  Q_FillLong(0,@Map,64);
  for I := 1 to Length(CharsToKeep) do
    Map[CharsToKeep[I]] := True;
  I := 1;
  L := Length(S);
  while (I<=L) and Map[S[I]] do Inc(I);
  if I <= L then
  begin
    UniqueString(S);
    P := Pointer(S);
    PK := Pointer(S);
    Inc(P,I-1);
    P1 := P;
    Inc(LongWord(PK),L);
    Inc(P1);
    while P1 < PK do
    begin
      if Map[P1^] then
      begin
        P^ := P1^;
        Inc(P);
      end;
      Inc(P1);
    end;
    P1 := Pointer(S);
    if P <> P1 then
    begin
      PK := Pointer(S);
      P^ := #0;
      Dec(PK);
      PK^ := LongWord(P)-LongWord(P1);
    end else
      S := '';
  end;
end;

procedure Q_KeepChars(var S: string; const CharsToKeep: TCharSet); overload;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EDX
        PUSH    EAX
        CALL    UniqueString
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        MOV     EDI,EAX
        TEST    ECX,ECX
        JE      @@zq0
@@lp1:  MOVZX   EDX,BYTE PTR [EAX]
        BT      [ESI],EDX
        JNC     @@rp
@@nx1:  MOV     BYTE PTR [EDI],DL
        INC     EAX
        INC     EDI
        DEC     ECX
        JNE     @@lp1
@@nx2:  POP     EAX
        MOV     ECX,[EAX]
        MOV     BYTE PTR [EDI],0
        SUB     EDI,ECX
        JE      @@zq1
        MOV     [ECX-4],EDI
        POP     EDI
        POP     ESI
        RET
@@qt:   POP     ECX
        POP     EDI
        POP     ESI
        RET
@@zq0:  POP     EAX
@@zq1:  CALL    System.@LStrClr
        POP     EDI
        POP     ESI
        RET
@@rp:   INC     EAX
        DEC     ECX
        JE      @@nx2
@@lp2:  MOVZX   EDX,BYTE PTR [EAX]
        BT      [ESI],EDX
        JC      @@nx1
        INC     EAX
        DEC     ECX
        JNE     @@lp2
        JMP     @@nx2
end;

function Q_ApplyMask(const Mask, SourceStr: string; MaskChar: Char): string;
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,[ESP+20]
        MOV     EBX,ECX
        TEST    EAX,EAX
        JE      @@zq
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@zq
        PUSH    EDX
        MOV     EAX,EDI
        XOR     EDX,EDX
        CALL    System.@LStrFromPCharLen
        POP     EDX
        MOV     EDI,[EDI]
        MOV     ECX,[EDX-4]
        LEA     EDX,[EDX+ECX-1]
        MOV     ECX,[ESI-4]
        DEC     ECX
@@lp:   MOV     AL,BYTE PTR [ESI+ECX]
        CMP     BL,AL
        JNE     @@mb
        MOV     AL,BYTE PTR [EDX]
        MOV     BYTE PTR [EDI+ECX],AL
        DEC     EDX
        DEC     ECX
        JNS     @@lp
        JMP     @@qt
@@mb:   MOV     BYTE PTR [EDI+ECX],AL
        DEC     ECX
        JNS     @@lp
        JMP     @@qt
@@zq:   MOV     EAX,EDI
        CALL    System.@LStrClr
@@qt:   POP     EDI
        POP     ESI
        POP     EBX
end;

procedure Q_ApplyMaskInPlace(var Mask: string; const SourceStr: string; MaskChar: Char);
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,ECX
        MOV     ESI,EDX
        CALL    UniqueString
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        DEC     ECX
        JS      @@qt
        MOV     EDX,[ESI-4]
        LEA     ESI,[ESI+EDX-1]
@@lp:   CMP     BL,BYTE PTR [EAX+ECX]
        JNE     @@nx1
        MOV     DL,BYTE PTR [ESI]
        MOV     BYTE PTR [EAX+ECX],DL
        DEC     ESI
@@nx1:  DEC     ECX
        JS      @@qt
        CMP     BL,BYTE PTR [EAX+ECX]
        JNE     @@nx2
        MOV     DL,BYTE PTR [ESI]
        MOV     BYTE PTR [EAX+ECX],DL
        DEC     ESI
@@nx2:  DEC     ECX
        JNS     @@lp
@@qt:   POP     ESI
        POP     EBX
end;

function Q_ExtractByMask(const S, Mask: string; MaskChar: Char): string;
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP
        MOV     ESI,EAX
        MOV     EBX,ECX
        MOV     EDI,[ESP+24]
        TEST    EDX,EDX
        JE      @@zq
        MOV     ECX,[EDX-4]
        XOR     EBP,EBP
        DEC     ECX
        JS      @@zq
@@lpp:  CMP     BL,BYTE PTR [EDX+ECX]
        JNE     @@nx1
        INC     EBP
@@nx1:  DEC     ECX
        JS      @@ex
        CMP     BL,BYTE PTR [EDX+ECX]
        JNE     @@nx2
        INC     EBP
@@nx2:  DEC     ECX
        JS      @@ex
        CMP     BL,BYTE PTR [EDX+ECX]
        JNE     @@nx3
        INC     EBP
@@nx3:  DEC     ECX
        JS      @@ex
        CMP     BL,BYTE PTR [EDX+ECX]
        JNE     @@nx4
        INC     EBP
@@nx4:  DEC     ECX
        JNS     @@lpp
@@ex:   TEST    EBP,EBP
        JE      @@zq
        MOV     ECX,EBP
        PUSH    EDX
        MOV     EAX,EDI
        XOR     EDX,EDX
        CALL    System.@LStrFromPCharLen
        POP     EDX
        MOV     EDI,[EDI]
        LEA     EDI,[EDI+EBP-1]
        MOV     ECX,[EDX-4]
        DEC     ECX
@@lp:   CMP     BL,BYTE PTR [EDX+ECX]
        JNE     @@ne1
        MOV     AL,BYTE PTR [ESI+ECX]
        MOV     BYTE PTR [EDI],AL
        DEC     EDI
@@ne1:  DEC     ECX
        JS      @@qt
        CMP     BL,BYTE PTR [EDX+ECX]
        JNE     @@ne2
        MOV     AL,BYTE PTR [ESI+ECX]
        MOV     BYTE PTR [EDI],AL
        DEC     EDI
@@ne2:  DEC     ECX
        JNS     @@lp
        JMP     @@qt
@@zq:   MOV     EAX,EDI
        CALL    System.@LStrClr
@@qt:   POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure Q_ExtractByMaskInPlace(var S: string; const Mask: string; MaskChar: Char);
asm
        PUSH    EDI
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,ECX
        MOV     ESI,EDX
        PUSH    EAX
        CALL    UniqueString
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@zq0
        MOV     EDI,EAX
@@lp:   CMP     BL,BYTE PTR [ESI]
        JNE     @@nx1
        MOV     DL,BYTE PTR [EAX]
        MOV     BYTE PTR [EDI],DL
        INC     EDI
@@nx1:  INC     EAX
        INC     ESI
        DEC     ECX
        JE      @@ex
        CMP     BL,BYTE PTR [ESI]
        JNE     @@nx2
        MOV     DL,BYTE PTR [EAX]
        MOV     BYTE PTR [EDI],DL
        INC     EDI
@@nx2:  INC     EAX
        INC     ESI
        DEC     ECX
        JNE     @@lp
@@ex:   POP     EAX
        MOV     ECX,[EAX]
        MOV     BYTE PTR [EDI],0
        SUB     EDI,ECX
        JE      @@zq1
        MOV     [ECX-4],EDI
        POP     ESI
        POP     EBX
        POP     EDI
        RET
@@qt:   POP     ECX
        POP     ESI
        POP     EBX
        POP     EDI
        RET
@@zq0:  POP     EAX
@@zq1:  CALL    System.@LStrClr
        POP     ESI
        POP     EBX
        POP     EDI
end;


procedure Q_TrimInPlace(var S: string);
asm
        PUSH    EBX
        PUSH    EAX
        CALL    UniqueString
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        MOV     EBX,EAX
        DEC     ECX
        JS      @@zq0
        MOV     EDX,EAX
@@lp0:  CMP     BYTE PTR [EAX+ECX],$20
        JA      @@nx0
        DEC     ECX
        JNS     @@lp0
        JMP     @@nx3
@@nx0:  INC     ECX
@@lp1:  CMP     BYTE PTR [EBX],$20
        JA      @@nx1
        INC     EBX
        DEC     ECX
        JMP     @@lp1
@@nx1:  CMP     EAX,EBX
        JE      @@qx
        TEST    ECX,3
        JE      @@nx2
@@lp2:  MOV     AL,BYTE PTR [EBX]
        DEC     ECX
        MOV     BYTE PTR [EDX],AL
        INC     EBX
        INC     EDX
        TEST    ECX,3
        JNE     @@lp2
@@nx2:  SHR     ECX,2
        JE      @@nx3
@@lp3:  MOV     AL,BYTE PTR [EBX]
        MOV     BYTE PTR [EDX],AL
        MOV     AL,BYTE PTR [EBX+1]
        MOV     BYTE PTR [EDX+1],AL
        MOV     AL,BYTE PTR [EBX+2]
        MOV     BYTE PTR [EDX+2],AL
        MOV     AL,BYTE PTR [EBX+3]
        MOV     BYTE PTR [EDX+3],AL
        ADD     EBX,4
        ADD     EDX,4
        DEC     ECX
        JNE     @@lp3
@@nx3:  POP     EAX
        MOV     EBX,[EAX]
        MOV     BYTE PTR [EDX],0
        SUB     EDX,EBX
        JE      @@zq1
        MOV     [EBX-4],EDX
        POP     EBX
        RET
@@qt:   POP     ECX
        POP     EBX
        RET
@@zq0:  POP     EAX
@@zq1:  CALL    System.@LStrClr
        POP     EBX
        RET
@@qx:   MOV     BYTE PTR [EAX+ECX],0
        MOV     [EAX-4],ECX
        POP     EDX
        POP     EBX
end;

procedure Q_TrimLeftInPlace(var S: string);
asm
        PUSH    EBX
        PUSH    EAX
        CALL    UniqueString
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        MOV     EBX,EAX
        TEST    ECX,ECX
        JE      @@zq0
        MOV     EDX,EAX
@@lp1:  CMP     BYTE PTR [EBX],$20
        JA      @@nx1
        INC     EBX
        DEC     ECX
        JNE     @@lp1
        JMP     @@nx3
@@nx1:  CMP     EAX,EBX
        JE      @@qt
        TEST    ECX,3
        JE      @@nx2
@@lp2:  MOV     AL,BYTE PTR [EBX]
        DEC     ECX
        MOV     BYTE PTR [EDX],AL
        INC     EBX
        INC     EDX
        TEST    ECX,3
        JNE     @@lp2
@@nx2:  SHR     ECX,2
        JE      @@nx3
@@lp3:  MOV     AL,BYTE PTR [EBX]
        MOV     BYTE PTR [EDX],AL
        MOV     AL,BYTE PTR [EBX+1]
        MOV     BYTE PTR [EDX+1],AL
        MOV     AL,BYTE PTR [EBX+2]
        MOV     BYTE PTR [EDX+2],AL
        MOV     AL,BYTE PTR [EBX+3]
        MOV     BYTE PTR [EDX+3],AL
        ADD     EBX,4
        ADD     EDX,4
        DEC     ECX
        JNE     @@lp3
@@nx3:  POP     EAX
        MOV     EBX,[EAX]
        MOV     BYTE PTR [EDX],0
        SUB     EDX,EBX
        JE      @@zq1
        MOV     [EBX-4],EDX
        POP     EBX
        RET
@@qt:   POP     ECX
        POP     EBX
        RET
@@zq0:  POP     EAX
@@zq1:  CALL    System.@LStrClr
        POP     EBX
end;

procedure Q_TrimRightInPlace(var S: string);
asm
        PUSH    EAX
        CALL    UniqueString
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        DEC     ECX
        JS      @@zq
@@lp:   CMP     BYTE PTR [EAX+ECX],$20
        JA      @@nx
        DEC     ECX
        JNS     @@lp
@@zq:   POP     EAX
        CALL    System.@LStrClr
        RET
        JMP     @@zq
@@nx:   INC     ECX
        MOV     BYTE PTR [EAX+ECX],0
        MOV     [EAX-4],ECX
@@qt:   POP     EAX
end;

function Q_TrimChar(const S: string; Ch: Char): string;
asm
        PUSH    ESI
        MOV     ESI,ECX
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EBX
        PUSH    EDI
        MOV     EBX,EAX
        MOV     EDI,EDX
        XOR     EDX,EDX
        MOV     EAX,ESI
        CALL    System.@LStrFromPCharLen
        MOV     EDX,EDI
        MOV     ECX,[EBX-4]
@@lp1:  CMP     DL,BYTE PTR [EBX]
        JNE     @@ex1
        INC     EBX
        DEC     ECX
        JNE     @@lp1
        MOV     EDX,[ESI]
        JMP     @@wq
@@ex1:  DEC     ECX
@@lp2:  CMP     DL,BYTE PTR [EBX+ECX]
        JNE     @@ex2
        DEC     ECX
        JMP     @@lp2
@@ex2:  MOV     EDI,[ESI]
        LEA     EDX,[EDI+ECX+1]
@@lp3:  MOV     AL,BYTE PTR [EBX+ECX]
        MOV     BYTE PTR [EDI+ECX],AL
        DEC     ECX
        JNS     @@lp3
@@wq:   MOV     EAX,[ESI]
        MOV     BYTE PTR [EDX],0
        SUB     EDX,EAX
        MOV     [EAX-4],EDX
        POP     EDI
        POP     EBX
        POP     ESI
        RET
@@qt:   MOV     EAX,ESI
        CALL    System.@LStrClr
        POP     ESI
end;

function Q_TrimCharLeft(const S: string; Ch: Char): string;
asm
        PUSH    ESI
        MOV     ESI,ECX
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EBX
        PUSH    EDI
        MOV     EBX,EAX
        MOV     EDI,EDX
        XOR     EDX,EDX
        MOV     EAX,ESI
        CALL    System.@LStrFromPCharLen
        MOV     EDX,EDI
        MOV     ECX,[EBX-4]
@@lp1:  CMP     DL,BYTE PTR [EBX]
        JNE     @@ex1
        INC     EBX
        DEC     ECX
        JNE     @@lp1
        MOV     EDX,[ESI]
        JMP     @@wq
@@ex1:  MOV     EDI,[ESI]
        LEA     EDX,[EDI+ECX]
        DEC     ECX
@@lp2:  MOV     AL,BYTE PTR [EBX+ECX]
        MOV     BYTE PTR [EDI+ECX],AL
        DEC     ECX
        JNS     @@lp2
@@wq:   MOV     EAX,[ESI]
        MOV     BYTE PTR [EDX],0
        SUB     EDX,EAX
        MOV     [EAX-4],EDX
        POP     EDI
        POP     EBX
        POP     ESI
        RET
@@qt:   MOV     EAX,ESI
        CALL    System.@LStrClr
        POP     ESI
end;

function Q_TrimCharRight(const S: string; Ch: Char): string;
asm
        PUSH    ESI
        MOV     ESI,ECX
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EBX
        PUSH    EDI
        MOV     EBX,EAX
        MOV     EDI,EDX
        XOR     EDX,EDX
        MOV     EAX,ESI
        CALL    System.@LStrFromPCharLen
        MOV     EDX,EDI
        MOV     ECX,[EBX-4]
        DEC     ECX
@@lp1:  CMP     DL,BYTE PTR [EBX+ECX]
        JNE     @@ex1
        DEC     ECX
        JNS     @@lp1
        MOV     EDX,[ESI]
        JMP     @@wq
@@ex1:  MOV     EDI,[ESI]
        LEA     EDX,[EDI+ECX+1]
@@lp2:  MOV     AL,BYTE PTR [EBX+ECX]
        MOV     BYTE PTR [EDI+ECX],AL
        DEC     ECX
        JNS     @@lp2
@@wq:   MOV     EAX,[ESI]
        MOV     BYTE PTR [EDX],0
        SUB     EDX,EAX
        MOV     [EAX-4],EDX
        POP     EDI
        POP     EBX
        POP     ESI
        RET
@@qt:   MOV     EAX,ESI
        CALL    System.@LStrClr
        POP     ESI
end;

function Q_KeepOneChar(const S: string; Ch: Char): string;
asm
        PUSH    ESI
        MOV     ESI,ECX
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EBX
        PUSH    EDI
        MOV     EBX,EAX
        MOV     EDI,EDX
        XOR     EDX,EDX
        MOV     EAX,ESI
        CALL    System.@LStrFromPCharLen
        MOV     EDX,EDI
        MOV     ECX,[EBX-4]
        MOV     EDI,[ESI]
@@lp:   MOV     AL,BYTE PTR [EBX]
        MOV     BYTE PTR [EDI],AL
        INC     EBX
        INC     EDI
        CMP     AL,DL
        JE      @@me
@@nx:   DEC     ECX
        JNE     @@lp
@@wq:   MOV     EAX,[ESI]
        MOV     BYTE PTR [EDI],0
        SUB     EDI,EAX
        MOV     [EAX-4],EDI
        POP     EDI
        POP     EBX
        POP     ESI
        RET
@@me:   CMP     DL,BYTE PTR [EBX]
        JNE     @@nx
        INC     EBX
        DEC     ECX
        JNE     @@me
        JMP     @@wq
@@qt:   MOV     EAX,ESI
        CALL    System.@LStrClr
        POP     ESI
end;

procedure Q_SpaceCompressInPlace(var S: string);
asm
        PUSH    EBX
        PUSH    EAX
        CALL    UniqueString
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        MOV     EBX,EAX
        DEC     ECX
        JS      @@qt
        MOV     EDX,EAX
@@lp0:  CMP     BYTE PTR [EAX+ECX],$20
        JA      @@lp1
        DEC     ECX
        JNS     @@lp0
        JMP     @@nx4
@@lp1:  CMP     BYTE PTR [EBX],$20
        JA      @@lp3
        INC     EBX
        DEC     ECX
        JMP     @@lp1
@@lp3:  MOV     AL,BYTE PTR [EBX]
        INC     EBX
        CMP     AL,$20
        JBE     @@me
@@nx3:  MOV     BYTE PTR [EDX],AL
        INC     EDX
        DEC     ECX
        JNS     @@lp3
@@nx4:  POP     EAX
        MOV     EBX,[EAX]
        MOV     BYTE PTR [EDX],0
        SUB     EDX,EBX
        MOV     [EBX-4],EDX
        POP     EBX
        RET
@@me:   MOV     BYTE PTR [EDX],$20
        INC     EDX
        DEC     ECX
        JS      @@nx4
@@ml:   MOV     AL,BYTE PTR [EBX]
        INC     EBX
        CMP     AL,$20
        JA      @@nx3
        DEC     ECX
        JNS     @@ml
        JMP     @@nx4
@@qt:   POP     ECX
        POP     EBX
end;

function Q_SpaceCompress(const S: string): string;
asm
        PUSH    ESI
        MOV     ESI,EDX
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EBX
        MOV     EBX,EAX
        XOR     EDX,EDX
        MOV     EAX,ESI
        CALL    System.@LStrFromPCharLen
        MOV     ECX,[EBX-4]
        MOV     EDX,[ESI]
@@lp1:  CMP     BYTE PTR [EBX],$20
        JA      @@ex1
        INC     EBX
        DEC     ECX
        JNE     @@lp1
        JMP     @@wq
@@ex1:  DEC     ECX
@@lp2:  CMP     BYTE PTR [EBX+ECX],$20
        JA      @@lp3
        DEC     ECX
        JMP     @@lp2
@@lp3:  MOV     AL,BYTE PTR [EBX]
        INC     EBX
        CMP     AL,$20
        JBE     @@me
@@nx:   MOV     BYTE PTR [EDX],AL
        INC     EDX
        DEC     ECX
        JNS     @@lp3
@@wq:   MOV     EAX,[ESI]
        MOV     BYTE PTR [EDX],0
        SUB     EDX,EAX
        MOV     [EAX-4],EDX
        POP     EBX
        POP     ESI
        RET
@@me:   MOV     BYTE PTR [EDX],$20
        INC     EDX
        DEC     ECX
        JS      @@wq
@@ml:   MOV     AL,BYTE PTR [EBX]
        INC     EBX
        CMP     AL,$20
        JA      @@nx
        DEC     ECX
        JNS     @@ml
        JMP     @@wq
@@qt:   MOV     EAX,ESI
        CALL    System.@LStrClr
        POP     ESI
end;

function Q_PadLeft(const S: string; Length: Integer; PadCh: Char;
  Cut: Boolean): string;
var
  K,L: Integer;
  P: ^Byte;
begin
  L := System.Length(S);
  K := Length - L;
  if K > 0 then
  begin
    SetString(Result, nil, Length);
    P := Pointer(Result);
    Q_FillChar(P,K,PadCh);
    if L > 0 then
    begin
      Inc(P,K);
      Q_CopyMem(Pointer(S),P,L);
    end;
  end
  else if (K<>0) and Cut then
    Result := Copy(S, 1, Length)
  else
    Result := S;
end;

function Q_PadRight(const S: string; Length: Integer; PadCh: Char;
  Cut: Boolean): string;
var
  K,L: Integer;
  P: ^Byte;
begin
  L := System.Length(S);
  K := Length - L;
  if K > 0 then
  begin
    SetString(Result, nil, Length);
    P := Pointer(Result);
    if L > 0 then
    begin
      Q_CopyMem(Pointer(S),P,L);
      Inc(P,L);
    end;
    Q_FillChar(P,K,PadCh);
  end
  else if (K<>0) and Cut then
    Result := Copy(S, 1, Length)
  else
    Result := S;
end;

function Q_CenterStr(const S: string; Length: Integer; PadCh: Char;
  Cut: Boolean): string;
var
  K,L: Integer;
  P: ^Byte;
begin
  L := System.Length(S);
  K := Length - L;
  if K > 0 then
  begin
    SetString(Result, nil, Length);
    P := Pointer(Result);
    Q_FillChar(P,K shr 1,PadCh);
    Inc(P,K shr 1);
    if L > 0 then
    begin
      Q_CopyMem(Pointer(S),P,L);
      Inc(P,L);
    end;
    Q_FillChar(P,K-K shr 1,PadCh);
  end
  else if (K<>0) and Cut then
    Result := Copy(S, 1, Length)
  else
    Result := S;
end;

function Q_PadInside(const S: string; Length: Integer; PadCh: Char;
  Cut: Boolean): string;
var
  N,I,K: Integer;
  P: ^Char;
  C: Char;
begin
  I := Length - System.Length(S);
  if I > 0 then
  begin
    N := Q_CharCount(S, PadCh);
    if N <> 0 then
    begin
      SetString(Result, nil, Length);
      P := Pointer(Result);
      K := I div N;
      N := I - K*N;
      for I := 1 to System.Length(S) do
      begin
        C := S[I];
        P^ := C;
        Inc(P);
        if C = PadCh then
        begin
          Q_FillChar(P,K,PadCh);
          Inc(P,K);
          if N <> 0 then
          begin
            Dec(N);
            P^ := PadCh;
            Inc(P);
          end;
        end;
      end;
    end else
      Result := S;
  end
  else if (I<>0) and Cut then
    Result := Copy(S, 1, Length)
  else
    Result := S;
end;

function Q_TabsToSpaces(const S: string; TabStop: Integer): string;
var
  I,L,T: Integer;
  P: ^Char;
begin
  T := TabStop;
  L := 0;
  for I := 1 to Length(S) do
    if S[I] <> #9 then
    begin
      Inc(L);
      Dec(T);
      if T = 0 then
        T := TabStop;
    end else
    begin
      Inc(L,T);
      T := TabStop;
    end;
  SetString(Result,nil,L);
  T := TabStop;
  P := Pointer(Result);
  for I := 1 to Length(S) do
    if S[I] <> #9 then
    begin
      P^ := S[I];
      Dec(T);
      Inc(P);
      if T = 0 then
        T := TabStop;
    end else
    begin
      repeat
        P^ := ' ';
        Dec(T);
        Inc(P);
      until T = 0;
      T := TabStop;
    end;
end;

function Q_SpacesToTabs(const S: string; TabStop: Integer): string;
var
  I,L,SC,T: Integer;
  P: ^Char;
  C: Char;
begin
  L := 0;
  T := TabStop;
  SC := 0;
  for I := 1 to Length(S) do
  begin
    if T = 0 then
    begin
      Dec(SC);
      T := TabStop;
      if SC > 0 then
        Dec(L,SC);
      SC := 0;
    end;
    Inc(L);
    C := S[I];
    Dec(T);
    if C <> ' ' then
    begin
      if C = #9 then
        T := TabStop;
      SC := 0;
    end else
      Inc(SC);
  end;
  if T = 0 then
  begin
    Dec(SC);
    if SC > 0 then
      Dec(L,SC);
  end;
  SetString(Result,nil,L);
  T := TabStop;
  P := Pointer(Result);
  SC := 0;
  for I := 1 to Length(S) do
  begin
    if T = 0 then
    begin
      T := TabStop;
      if SC <> 0 then
      begin
        if SC > 1 then
          P^ := #9
        else
          P^ := ' ';
        Inc(P);
        SC := 0;
      end;
    end;
    C := S[I];
    Dec(T);
    if C <> ' ' then
    begin
      while SC <> 0 do
      begin
        P^ := ' ';
        Dec(SC);
        Inc(P);
      end;
      P^ := C;
      if C = #9 then
        T := TabStop;
      Inc(P);
    end else
      Inc(SC);
  end;
  if SC <> 0 then
    if T <> 0 then
      repeat
        P^ := ' ';
        Dec(SC);
        Inc(P);
      until SC = 0
    else if SC > 1 then
      P^ := #9
    else
      P^ := ' ';
end;

function Q_StrTok(var S: string; const Delimiters: string): string; overload;
var
  DelimsMap: PDelimsMap;
  I: Integer;
  P1,P2: PChar;
  L: LongWord;
begin
  asm
        CALL    SysInit.@GetTls
        LEA     EAX,[EAX+UDelimsMap]
        MOV     DelimsMap,EAX
  end;
  L := Length(Delimiters);
  if L <> 0 then
  begin
    Q_FillLong(0,DelimsMap,64);
    for I := 1 to L do
      DelimsMap^[Delimiters[I]] := True;
  end;
  L := Length(S);
  if L <> 0 then
  begin
    P2 := Pointer(S);
    Inc(L,LongWord(P2));
    while (LongWord(P2)<L) and DelimsMap^[P2^] do Inc(P2);
    if LongWord(P2) >= L then
    begin
      S := '';
      Result := '';
      Exit;
    end;
    P1 := P2;
    while (LongWord(P2)<L) and not DelimsMap^[P2^] do Inc(P2);
    I := LongWord(P2)-LongWord(P1);
    SetString(Result,nil,I);
    Q_CopyMem(P1,Pointer(Result),I);
    P1 := Pointer(S);
    Q_CutLeft(S,LongWord(P2)-LongWord(P1));
  end else
    Result := '';
end;

function Q_StrTok(var S: string; const Delimiters: TCharSet): string; overload;
var
  I: Integer;
  P1,P2: PChar;
  L: LongWord;
begin
  L := Length(S);
  if L <> 0 then
  begin
    P2 := Pointer(S);
    Inc(L,LongWord(P2));
    while (LongWord(P2)<L) and (P2^ in Delimiters) do Inc(P2);
    if LongWord(P2) >= L then
    begin
      S := '';
      Result := '';
      Exit;
    end;
    P1 := P2;
    while (LongWord(P2)<L) and not (P2^ in Delimiters) do Inc(P2);
    I := LongWord(P2)-LongWord(P1);
    SetString(Result,nil,I);
    Q_CopyMem(P1,Pointer(Result),I);
    P1 := Pointer(S);
    Q_CutLeft(S,LongWord(P2)-LongWord(P1));
  end else
    Result := '';
end;

function Q_StrTok1(var S: string; const Delimiters: string): string; overload;
var
  DelimsMap: PDelimsMap;
  I,L: LongWord;
  P: PChar;
begin
  asm
        CALL    SysInit.@GetTls
        LEA     EAX,[EAX+UDelimsMap]
        MOV     DelimsMap,EAX
  end;
  L := Length(Delimiters);
  if L <> 0 then
  begin
    Q_FillLong(0,DelimsMap,64);
    for I := 1 to L do
      DelimsMap^[Delimiters[I]] := True;
  end;
  L := Length(S);
  if L <> 0 then
  begin
    I := 0;
    P := Pointer(S);
    while (I<L) and not DelimsMap^[P^] do
    begin
      Inc(I);
      Inc(P);
    end;
    if I > 0 then
    begin
      SetString(Result,nil,I);
      Q_CopyMem(Pointer(S),Pointer(Result),I);
    end else
      Result := '';
    if I < L then
      Q_CutLeft(S,I+1)
    else
      S := '';
  end else
    Result := '';
end;

function Q_StrTok1(var S: string; const Delimiters: TCharSet): string; overload;
var
  I,L: LongWord;
begin
  L := Length(S);
  if L <> 0 then
  begin
    I := 0;
    while (I<L) and not Q_BitTest(@Delimiters,Byte(S[I+1])) do Inc(I);
    if I > 0 then
    begin
      SetString(Result,nil,I);
      Q_CopyMem(Pointer(S),Pointer(Result),I);
    end else
      Result := '';
    if I < L then
      Q_CutLeft(S,I+1)
    else
      S := '';
  end else
    Result := '';
end;

function Q_WordAtPos(const S: string; Pos: Integer; const Delimiters: string): string; overload;
var
  DelimsMap: PDelimsMap;
  I: Integer;
  P1,P2: PChar;
  L: Integer;
begin
  asm
        CALL    SysInit.@GetTls
        LEA     EAX,[EAX+UDelimsMap]
        MOV     DelimsMap,EAX
  end;
  L := Length(Delimiters);
  if L <> 0 then
  begin
    Q_FillLong(0,DelimsMap,64);
    for I := 1 to L do
      DelimsMap^[Delimiters[I]] := True;
  end;
  L := Length(S);
  if (Pos>0) and (Pos<=L) and not DelimsMap^[S[Pos]] then
  begin
    P1 := Pointer(S);
    P2 := Pointer(S);
    Inc(P1,Pos-2);
    Inc(P2,Pos);
    for I := Pos-1 downto 1 do
      if not DelimsMap^[P1^] then
        Dec(P1)
      else
        Break;
    Inc(P1);
    for I := Pos+1 to L do
      if not DelimsMap^[P2^] then
        Inc(P2)
      else
        Break;
    SetString(Result,P1,LongWord(P2)-LongWord(P1));
  end else
    Result := '';
end;

function Q_WordAtPos(const S: string; Pos: Integer; const Delimiters: TCharSet): string; overload;
var
  I: Integer;
  P1,P2: PChar;
  L: Integer;
begin
  L := Length(S);
  if (Pos>0) and (Pos<=L) and not (S[Pos] in Delimiters) then
  begin
    P1 := Pointer(S);
    P2 := Pointer(S);
    Inc(P1,Pos-2);
    Inc(P2,Pos);
    for I := Pos-1 downto 1 do
      if not (P1^ in Delimiters) then
        Dec(P1)
      else
        Break;
    Inc(P1);
    for I := Pos+1 to L do
      if not (P2^ in Delimiters) then
        Inc(P2)
      else
        Break;
    SetString(Result,P1,LongWord(P2)-LongWord(P1));
  end else
    Result := '';
end;

function Q_GetWordN(OrdN: Integer; const S, Delimiters: string): string; overload;
var
  DelimsMap: PDelimsMap;
  I,J,N: Integer;
  L: LongWord;
  P: PChar;
  A: Boolean;
begin
  asm
        CALL    SysInit.@GetTls
        LEA     EAX,[EAX+UDelimsMap]
        MOV     DelimsMap,EAX
  end;
  L := Length(Delimiters);
  if L <> 0 then
  begin
    Q_FillLong(0,DelimsMap,64);
    for I := 1 to L do
      DelimsMap^[Delimiters[I]] := True;
  end;
  L := Length(S);
  P := Pointer(S);
  A := False;
  N := 1;
  for I := 1 to L do
  begin
    if not DelimsMap^[P^] then
    begin
      if not A then
      begin
        if N = OrdN then
        begin
          N := L+1;
          Inc(P);
          for J := I+1 to L do
          begin
            if DelimsMap^[P^] then
            begin
              N := J;
              Break;
            end;
            Inc(P);
          end;
          Result := Copy(S,I,N-I);
          Exit;
        end;
        A := True;
        Inc(N);
      end;
    end
    else if A then
      A := False;
    Inc(P);
  end;
  Result := '';
end;

function Q_GetWordN(OrdN: Integer; const S: string; const Delimiters: TCharSet): string; overload;
var
  I,J,N: Integer;
  L: LongWord;
  P: PChar;
  A: Boolean;
begin
  L := Length(S);
  P := Pointer(S);
  A := False;
  N := 1;
  for I := 1 to L do
  begin
    if not (P^ in Delimiters) then
    begin
      if not A then
      begin
        if N = OrdN then
        begin
          N := L+1;
          Inc(P);
          for J := I+1 to L do
          begin
            if P^ in Delimiters then
            begin
              N := J;
              Break;
            end;
            Inc(P);
          end;
          Result := Copy(S,I,N-I);
          Exit;
        end;
        A := True;
        Inc(N);
      end;
    end
    else if A then
      A := False;
    Inc(P);
  end;
  Result := '';
end;

function Q_CopyRange(const S: string; Start, Stop: Integer): string;
begin
  Result := Copy(S,Start,Stop-Start+1);
end;

function Q_CopyFrom(const S: string; Start: Integer): string;
begin
  Result := Copy(S,Start,MaxInt);
end;

function Q_CopyLeft(const S: string; Count: Integer): string;
begin
  Result := Copy(S,1,Count);
end;

function Q_CopyRight(const S: string; Count: Integer): string;
begin
  Result := Copy(S,Length(S)-Count+1,Count);
end;

procedure Q_PasteStr(var Dest: string; Pos, Count: Integer; const Source: string);
var
  L1,L2: Integer;
  P,P1: PByte;
  Temp: string;
begin
  L1 := Length(Dest);
  Dec(Pos);
  if L1 < Count+Pos then
    Count := L1-Pos;
  if (Pos>=0) and (Count>=0) then
  begin
    L2 := Length(Source);
    if L2 <= Count then
    begin
      if (L2>0) or (L1>Count) then
      begin
        UniqueString(Dest);
        P := Pointer(Dest);
        Inc(P,Pos);
        P1 := P;
        if L2 <> 0 then
        begin
          Q_CopyMem(Pointer(Source),P,L2);
          Inc(P,L2);
        end;
        if L2 <> Count then
        begin
          Inc(P1,Count);
          Dec(L1,Count);
          Q_MoveMem(P1,P,L1-Pos+1);
          PLong(LongWord(Dest)-4)^ := L1+L2;
        end;
      end else
        Dest := '';
    end else
    begin
      SetString(Temp,nil,L1-Count+L2);
      P := Pointer(Temp);
      if Pos <> 0 then
      begin
        Q_CopyMem(Pointer(Dest),P,Pos);
        Inc(P,Pos);
      end;
      Q_CopyMem(Pointer(Source),P,L2);
      Inc(P,L2);
      Dec(L1,Count+Pos);
      if L1 <> 0 then
      begin
        P1 := Pointer(Dest);
        Inc(P1,Pos+Count);
        Q_CopyMem(P1,P,L1);
      end;
      Dest := Temp;
    end;
  end;
end;

function Q_CopyDel(var S: string; Start, Length: Integer): string;
begin
  Result := Copy(S,Start,Length);
  Q_Delete(S,Start,Length);
end;

procedure Q_SetDelimiters(const Delimiters: string); overload;
var
  DelimsMap: PDelimsMap;
  I: Integer;
begin
  asm
        CALL    SysInit.@GetTls
        LEA     EAX,[EAX+UDelimsMap]
        MOV     DelimsMap,EAX
  end;
  Q_FillLong(0, DelimsMap, 64);
  for I := 1 to Length(Delimiters) do
    DelimsMap^[Delimiters[I]] := True;
end;

procedure Q_SetDelimiters(const Delimiters: TCharSet); overload;
var
  DelimsMap: PDelimsMap;
  I: Integer;
begin
  asm
        CALL    SysInit.@GetTls
        LEA     EAX,[EAX+UDelimsMap]
        MOV     DelimsMap,EAX
  end;
  Q_FillLong(0, DelimsMap, 64);
  for I := 0 to 255 do
    if Char(I) in Delimiters then
      DelimsMap^[Char(I)] := True;
end;

function Q_GetDelimiters: string;
var
  Map: array[0..255] of Byte;
  DelimsArr: PDelimsArr;
  I,DelCnt: Integer;
begin
  asm
        CALL    SysInit.@GetTls
        LEA     EAX,[EAX+UDelimsMap]
        MOV     DelimsArr,EAX
  end;
  DelCnt := 0;
  for I := 0 to $FF do
    if DelimsArr^[I] then
    begin
      Map[DelCnt] := I;
      Inc(DelCnt);
    end;
  SetString(Result,PChar(@Map),DelCnt);
end;

procedure Q_StrMoveL(const Source: string; var Dest: string; MaxL: Cardinal);
asm
        MOV     EDX,[EDX]
        TEST    EAX,EAX
        JE      @@1
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EAX,[EAX-4]
        TEST    EAX,EAX
        JE      @@3
        CMP     ECX,EAX
        JB      @@0
        MOV     ECX,EAX
@@0:    MOV     [EDI-4],ECX
        MOV     BYTE PTR [EDI+ECX],$00
        MOV     EDX,ECX
        SHR     ECX,2
        AND     EDX,3
        CMP     ECX,8
        JB      @@cw
        REP     MOVSD
        JMP     DWORD PTR @@tV[EDX*4]
@@3:    POP     EDI
        POP     ESI
@@1:    TEST    EDX,EDX
        JE      @@2
        MOV     [EDX-4],EAX
        MOV     BYTE PTR [EDX],AL
@@2:    RET
@@cw:   JMP     DWORD PTR @@wV[ECX*4]
@@wV:   DD      @@w0, @@w1, @@w2, @@w3
        DD      @@w4, @@w5, @@w6, @@w7
@@w7:   MOV     EAX,[ESI+ECX*4-28]
        MOV     [EDI+ECX*4-28],EAX
@@w6:   MOV     EAX,[ESI+ECX*4-24]
        MOV     [EDI+ECX*4-24],EAX
@@w5:   MOV     EAX,[ESI+ECX*4-20]
        mov     [EDI+ECX*4-20],EAX
@@w4:   MOV     EAX,[ESI+ECX*4-16]
        MOV     [EDI+ECX*4-16],EAX
@@w3:   MOV     EAX,[ESI+ECX*4-12]
        MOV     [EDI+ECX*4-12],EAX
@@w2:   MOV     EAX,[ESI+ECX*4-8]
        MOV     [EDI+ECX*4-8],EAX
@@w1:   MOV     EAX,[ESI+ECX*4-4]
        MOV     [EDI+ECX*4-4],EAX
        LEA     EAX,[ECX*4]
        ADD     ESI,EAX
        ADD     EDI,EAX
@@w0:   JMP     DWORD PTR @@tV[EDX*4]
@@tV:   DD      @@t0, @@t1, @@t2, @@t3
@@t0:   POP     EDI
        POP     ESI
        RET
@@t1:   MOV     AL,[ESI]
        MOV     [EDI],AL
        POP     EDI
        POP     ESI
        RET
@@t2:   MOV     AL,[ESI]
        MOV     [EDI],AL
        MOV     AL,[ESI+1]
        MOV     [EDI+1],AL
        POP     EDI
        POP     ESI
        RET
@@t3:   MOV     AL,[ESI]
        MOV     [EDI],AL
        MOV     AL,[ESI+1]
        MOV     [EDI+1],AL
        MOV     AL,[ESI+2]
        MOV     [EDI+2],AL
        POP     EDI
        POP     ESI
end;

procedure Q_StrReverse(var S: string);
asm
        CALL    UniqueString
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        LEA     EDX,[EAX+ECX-1]
@@lp:   CMP     EAX,EDX
        JAE     @@qt
        MOV     CH,BYTE PTR [EAX]
        MOV     CL,BYTE PTR [EDX]
        MOV     BYTE PTR [EDX],CH
        MOV     BYTE PTR [EAX],CL
        INC     EAX
        DEC     EDX
        JMP     @@lp
@@qt:
end;

function Q_PStrReverse(P: Pointer): Pointer;
asm
        TEST    EAX,EAX
        JE      @@qt1
        MOV     ECX,[EAX-4]
        LEA     EDX,[EAX+ECX-1]
        PUSH    EAX
@@lp:   CMP     EAX,EDX
        JAE     @@qt0
        MOV     CH,BYTE PTR [EAX]
        MOV     CL,BYTE PTR [EDX]
        MOV     BYTE PTR [EDX],CH
        MOV     BYTE PTR [EAX],CL
        INC     EAX
        DEC     EDX
        JMP     @@lp
@@qt0:  POP     EAX
@@qt1:
end;

procedure Q_CutLeft(var S: string; CharCount: Integer);
var
  L: Integer;
  P: ^Integer;
begin
  if CharCount > 0 then
  begin
    L := Length(S)-CharCount;
    if L > 0 then
    begin
      UniqueString(S);
      P := Pointer(S);
      Dec(P);
      P^ := L;
      Inc(LongWord(P),CharCount+4);
      if CharCount > 3 then
        Q_MoveMem(P,Pointer(S),L)
      else
        Q_MoveBytes(P,Pointer(S),L);
      P := Pointer(S);
      Inc(LongWord(P),L);
      PByte(P)^ := 0;
    end else
      S := '';
  end
  else if CharCount < 0 then
    Q_CutRight(S,-CharCount);
end;

procedure Q_CutRight(var S: string; CharCount: Integer);
var
  L: Integer;
  P: ^Integer;
begin
  if CharCount > 0 then
  begin
    L := Length(S)-CharCount;
    if L > 0 then
    begin
      UniqueString(S);
      P := Pointer(S);
      Dec(P);
      P^ := L;
      Inc(LongWord(P),L+4);
      PByte(P)^ := 0;
    end else
      S := '';
  end
  else if CharCount < 0 then
    Q_CutLeft(S,-CharCount);
end;

procedure IntShortRtLeft(P: Pointer; Shift, Len: LongWord);
var
  P1: Pointer;
  T: LongWord;
begin
  P1 := P;
  case Shift of
    1:
      begin
        T := PByte(P1)^;
        Inc(LongWord(P1));
        Q_MoveBytes(P1,P,Len-1);
        Inc(LongWord(P1),Len-2);
        PByte(P1)^ := Byte(T);
      end;
    2:
      begin
        T := PWord(P1)^;
        Inc(LongWord(P1),2);
        Q_MoveWords(P1,P,(Len-1) shr 1);
        Inc(LongWord(P1),Len-4);
        PWord(P1)^ := Word(T);
      end;
    3:
      begin
        T := PLong(P1)^;
        Inc(LongWord(P1),3);
        Q_MoveWords(P1,P,(Len-2) shr 1);
        Inc(LongWord(P1),Len-6);
        PWord(P1)^ := Word(T);
        Inc(LongWord(P1),2);
        PByte(P1)^ := Byte(T shr 16);
      end;
    4:
      begin
        T := PLong(P1)^;
        Inc(LongWord(P1),4);
        Q_MoveMem(P1,P,Len-4);
        Inc(LongWord(P1),Len-8);
        PLong(P1)^ := T;
      end;
  end;
end;

procedure IntShortRtRight(P: Pointer; Shift, Len: LongWord);
var
  P1: Pointer;
  T: LongWord;
begin
  P1 := P;
  case Shift of
    1:
      begin
        T := PByte(LongWord(P1)+Len-1)^;
        Inc(LongWord(P1));
        Q_MoveBytes(P,P1,Len-1);
        PByte(P)^ := Byte(T);
      end;
    2:
      begin
        T := PWord(LongWord(P1)+Len-2)^;
        Inc(LongWord(P1),2);
        Q_MoveWords(P,P1,(Len-1) shr 1);
        PByte(LongWord(P)+Len)^ := 0;
        PWord(P)^ := Word(T);
      end;
    3:
      begin
        T := PLong(LongWord(P1)+Len-3)^;
        Inc(LongWord(P1),3);
        Q_MoveWords(P,P1,(Len-2) shr 1);
        PByte(LongWord(P)+Len)^ := 0;
        PWord(P)^ := Word(T);
        Dec(LongWord(P1));
        PByte(P1)^ := Byte(T shr 16);
      end;
    4:
      begin
        T := PLong(LongWord(P1)+Len-4)^;
        Inc(LongWord(P1),4);
        Q_MoveMem(P,P1,Len-4);
        PLong(P)^ := T;
      end;
  end;
end;

procedure IntMediumRtLeft(P: Pointer; Shift, Len: LongWord);
var
  M: array[0..255] of Byte;
begin
  Dec(Len,Shift);
  Q_CopyMem(P,@M,Shift);
  Q_MoveMem(PByte(LongWord(P)+Shift),P,Len);
  Inc(LongWord(P),Len);
  Q_CopyMem(@M,P,Shift);
end;

procedure IntMediumRtRight(P: Pointer; Shift, Len: LongWord);
var
  M: array[0..255] of Byte;
begin
  Dec(Len,Shift);
  Q_CopyMem(Pointer(LongWord(P)+Len),@M,Shift);
  Q_MoveMem(P,PByte(LongWord(P)+Shift),Len);
  Q_CopyMem(@M,P,Shift);
end;

procedure IntLongRotateStr(P: Pointer; LShift, Len: Integer);
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        PUSH    EBP
        MOV     EBX,ECX
        SUB     EAX,4
        PUSH    EAX
        SHR     ECX,1
        CMP     EDX,ECX
        JA      @@ri
@@le:   MOV     ESI,[ESP]
        LEA     EDI,[ESI+EDX]
@@ln:   MOV     ECX,EDX
        AND     ECX,$FFFFFFFC
        JE      @@tqi
@@lp1:  MOV     EAX,DWORD PTR [ESI+ECX]
        MOV     EBP,DWORD PTR [EDI+ECX]
        MOV     DWORD PTR [EDI+ECX],EAX
        MOV     DWORD PTR [ESI+ECX],EBP
        SUB     ECX,4
        JNE     @@lp1
@@tqi:  LEA     ECX,[EDX+4]
        TEST    ECX,3
        JE      @@tqo
@@ls1:  DEC     ECX
        MOV     AL,BYTE PTR [ESI+ECX]
        MOV     AH,BYTE PTR [EDI+ECX]
        MOV     BYTE PTR [EDI+ECX],AL
        MOV     BYTE PTR [ESI+ECX],AH
        TEST    ECX,3
        JNE     @@ls1
@@tqo:  SUB     EBX,EDX
        MOV     ESI,EDI
        MOV     [ESP],EDI
        ADD     EDI,EDX
        MOV     ECX,EDX
        SHL     ECX,1
        CMP     EBX,ECX
        JL      @@ri
        JMP     @@ln
@@ri:   SUB     EDX,EBX
        TEST    EDX,EDX
        JE      @@qt
        NEG     EDX
        MOV     ECX,[ESP]
        LEA     EDI,[EBX+ECX]
        SUB     EDI,EDX
        MOV     ESI,EDI
        SUB     ESI,EDX
@@rn:   MOV     ECX,EDX
        AND     ECX,$FFFFFFFC
        JE      @@fqi
@@lp2:  MOV     EAX,DWORD PTR [ESI+ECX]
        MOV     EBP,DWORD PTR [EDI+ECX]
        MOV     DWORD PTR [EDI+ECX],EAX
        MOV     DWORD PTR [ESI+ECX],EBP
        SUB     ECX,4
        JNE     @@lp2
@@fqi:  LEA     ECX,[EDX+4]
        TEST    ECX,3
        JE      @@fqo
@@ls2:  DEC     ECX
        MOV     AL,BYTE PTR [ESI+ECX]
        MOV     AH,BYTE PTR [EDI+ECX]
        MOV     BYTE PTR [EDI+ECX],AL
        MOV     BYTE PTR [ESI+ECX],AH
        TEST    ECX,3
        JNE     @@ls2
@@fqo:  SUB     EBX,EDX
        MOV     EDI,ESI
        SUB     ESI,EDX
        CMP     [ESP],ESI
        JA      @@tl
        JMP     @@rn
@@tl:   SUB     EDX,EBX
        TEST    EDX,EDX
        JE      @@qt
        NEG     EDX
        JMP     @@le
@@qt:   POP     ECX
        POP     EBP
        POP     EBX
        POP     EDI
        POP     ESI
end;

procedure IntRtLeft(var S: string; Shift, Len: Integer);
begin
  if Shift > 0 then
  begin
    UniqueString(S);
    if Shift <= 4 then
      IntShortRtLeft(Pointer(S),Shift,Len)
    else if Shift <= 256 then
      IntMediumRtLeft(Pointer(S),Shift,Len)
    else
      IntLongRotateStr(Pointer(S),Shift,Len);
  end;
end;

procedure IntRtRight(var S: string; Shift, Len: Integer);
begin
  if Shift > 0 then
  begin
    UniqueString(S);
    if Shift <= 4 then
      IntShortRtRight(Pointer(S),Shift,Len)
    else if Shift <= 256 then
      IntMediumRtRight(Pointer(S),Shift,Len)
    else
      IntLongRotateStr(Pointer(S),Len-Shift,Len);
  end;
end;

procedure Q_RotateLeft(var S: string; Shift: Integer);
var
  L: Integer;
begin
  L := Length(S);
  if L >= 2 then
  begin
    Shift := Shift mod L;
    if Shift < 0 then
      Inc(Shift,L);
    if Shift <= L shr 1 then
      IntRtLeft(S,Shift,L)
    else
      IntRtRight(S,L-Shift,L);
  end;
end;

procedure Q_RotateRight(var S: string; Shift: Integer);
var
  L: Integer;
begin
  L := Length(S);
  if L >= 2 then
  begin
    Shift := Shift mod L;
    if Shift < 0 then
      Inc(Shift,L);
    if Shift <= L shr 1 then
      IntRtRight(S,Shift,L)
    else
      IntRtLeft(S,L-Shift,L);
  end;
end;

function Q_Duplicate(const S: string; Count: Integer): string;
var
  I,L: Integer;
  P,P1: PChar;
begin
  L := Length(S);
  if (L>0) and (Count>0) then
  begin
    SetString(Result, nil, L*Count);
    P := Pointer(S);
    P1 := Pointer(Result);
    for I := 0 to Count-1 do
    begin
      Q_CopyMem(P, P1, L);
      Inc(P1, L);
    end;
  end else
    Result := '';
end;

const
  ToBase64: array[0..63] of Char =
    ('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R',
     'S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j',
     'k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','0','1',
     '2','3','4','5','6','7','8','9','+','/');

  FromBase64: array[0..79] Of Byte =
    (62,0,0,0,63,52,53,54,55,56,57,58,59,60,61,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,
     9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,0,0,0,0,0,0,26,27,28,29,
     30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51);

procedure IntBase64Encode(P1, P2: Pointer; L: Integer);
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     ESI,ECX
        MOV     EDI,EDX
@@lp:   SUB     ESI,3
        JS      @@nx
        XOR     EAX,EAX
        MOV     AH,BYTE PTR [EBX]
        MOV     AL,BYTE PTR [EBX+1]
        MOVZX   EDX,BYTE PTR [EBX+2]
        SHL     EAX,8
        OR      EAX,EDX
        ROL     EAX,14
        MOVZX   EDX,AL
        AND     EAX,$FFFFFF00
        ROL     EAX,6
        MOV     CL,BYTE PTR [EDX+ToBase64]
        MOV     BYTE PTR [EDI],CL
        MOVZX   EDX,AL
        AND     EAX,$FFFF0000
        ROL     EAX,6
        MOV     CL,BYTE PTR [EDX+ToBase64]
        MOV     BYTE PTR [EDI+1],CL
        MOVZX   EDX,AL
        AND     EAX,$FF000000
        ROL     EAX,6
        MOV     CL,BYTE PTR [EDX+ToBase64]
        MOV     BYTE PTR [EDI+2],CL
        MOVZX   EDX,AL
        ADD     EBX,3
        MOV     CL,BYTE PTR [EDX+ToBase64]
        MOV     BYTE PTR [EDI+3],CL
        ADD     EDI,4
        JMP     @@lp
@@nx:   ADD     ESI,3
        JMP     DWORD PTR @@tV[ESI*4]
@@tV:   DD      @@tu0,@@tu1,@@tu2
@@tu0:  POP     EDI
        POP     ESI
        POP     EBX
        RET
@@tu1:  XOR     EAX,EAX
        MOV     AL,BYTE PTR [EBX]
        ROR     EAX,2
        MOVZX   EDX,AL
        AND     EAX,$FFFFFF00
        ROL     EAX,6
        MOV     CL,BYTE PTR [EDX+ToBase64]
        MOV     BYTE PTR [EDI],CL
        MOVZX   EDX,AL
        MOV     BYTE PTR [EDI+2],61
        MOV     BYTE PTR [EDI+3],61
        MOV     CL,BYTE PTR [EDX+ToBase64]
        MOV     BYTE PTR [EDI+1],CL
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@tu2:  XOR     EAX,EAX
        MOV     AH,BYTE PTR [EBX]
        MOV     AL,BYTE PTR [EBX+1]
        ROR     EAX,10
        MOVZX   EDX,AL
        AND     EAX,$FFFFFF00
        ROL     EAX,6
        MOV     CL,BYTE PTR [EDX+ToBase64]
        MOV     BYTE PTR [EDI],CL
        MOVZX   EDX,AL
        AND     EAX,$FFFF0000
        ROL     EAX,6
        MOV     CL,BYTE PTR [EDX+ToBase64]
        MOV     BYTE PTR [EDI+1],CL
        MOVZX   EDX,AL
        MOV     BYTE PTR [EDI+3],61
        MOV     CL,BYTE PTR [EDX+ToBase64]
        MOV     BYTE PTR [EDI+2],CL
        POP     EDI
        POP     ESI
        POP     EBX
end;

function Q_Base64Encode(const S: string): string; overload;
var
  L: Integer;
begin
  L := Length(S);
  if L <> 0 then
  begin
    SetString(Result,nil,((L+2) div 3) shl 2);
    IntBase64Encode(Pointer(S),Pointer(Result),L);
  end else
    Result := '';
end;

function Q_Base64Encode(P: Pointer; L: Cardinal): string; overload;
begin
  if L <> 0 then
  begin
    SetString(Result,nil,((L+2) div 3) shl 2);
    IntBase64Encode(P,Pointer(Result),L);
  end else
    Result := '';
end;

procedure IntBase64Decode(P1, P2: Pointer; L: Integer);
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     ESI,ECX
        PUSH    EDX
        MOV     EDI,EDX
@@lp:   DEC     ESI
        JE      @@nx0
        MOV     EDX,[EBX]
        MOVZX   EAX,DL
        SUB     EAX,43
        XOR     ECX,ECX
        MOV     CL,BYTE PTR [EAX+FromBase64]
        ROL     ECX,6
        MOVZX   EAX,DH
        SUB     EAX,43
        OR      CL,BYTE PTR [EAX+FromBase64]
        SHR     EDX,16
        MOVZX   EAX,DL
        SUB     EAX,43
        ROL     ECX,6
        OR      CL,BYTE PTR [EAX+FromBase64]
        MOVZX   EAX,DH
        SUB     EAX,43
        ROL     ECX,6
        MOVZX   EDX,BYTE PTR [EAX+FromBase64]
        OR      ECX,EDX
        ADD     EBX,4
        ROR     ECX,8
        MOV     BYTE PTR [EDI],CH
        MOV     BYTE PTR [EDI+1],CL
        SHR     ECX,24
        MOV     BYTE PTR [EDI+2],CL
        ADD     EDI,3
        JMP     @@lp
@@nx0:  MOV     EDX,[EBX]
        MOVZX   EAX,DL
        SUB     EAX,43
        XOR     ECX,ECX
        MOV     CL,BYTE PTR [EAX+FromBase64]
        ROL     ECX,6
        MOVZX   EAX,DH
        SUB     EAX,43
        OR      CL,BYTE PTR [EAX+FromBase64]
        SHR     EDX,16
        CMP     DH,61
        JE      @@nx1
        MOVZX   EAX,DL
        SUB     EAX,43
        ROL     ECX,6
        OR      CL,BYTE PTR [EAX+FromBase64]
        MOVZX   EAX,DH
        SUB     EAX,43
        ROL     ECX,6
        MOVZX   EDX,BYTE PTR [EAX+FromBase64]
        OR      ECX,EDX
        ADD     EBX,4
        ROR     ECX,8
        MOV     BYTE PTR [EDI],CH
        MOV     BYTE PTR [EDI+1],CL
        SHR     ECX,24
        MOV     BYTE PTR [EDI+2],CL
        POP     ECX
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@nx1:  CMP     DL,61
        JE      @@nx2
        MOVZX   EAX,DL
        ROL     ECX,6
        SUB     EAX,43
        MOVZX   EDX,BYTE PTR [EAX+FromBase64]
        OR      ECX,EDX
        SHR     ECX,2
        MOV     BYTE PTR [EDI],CH
        MOV     BYTE PTR [EDI+1],CL
        MOV     BYTE PTR [EDI+2],0
        POP     EDX
        DEC     DWORD PTR [EDX-4]
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@nx2:  SHR     ECX,4
        MOV     BYTE PTR [EDI],CL
        MOV     BYTE PTR [EDI+1],0
        POP     EDX
        SUB     DWORD PTR [EDX-4],2
        POP     EDI
        POP     ESI
        POP     EBX
end;

function Q_Base64Decode(const S: string): string;
var
  L: Integer;
begin
  L := Length(S);
  if L <> 0 then
  begin
    if L and 3 <> 0 then
      raise EConvertError.Create('Формат строки не соответствует способу кодирования Base64.');
    L := L shr 2;
    SetString(Result,nil,L*3);
    IntBase64Decode(Pointer(S),Pointer(Result),L);
  end else
    Result := '';
end;

type
  PRLECnt = ^TRLECnt;
  TRLECnt = array[0..5] of Byte;

const
  RLECC3 = $16;
  RLECC4 = $17;
  RLECC5 = $18;
  RLECC6 = $19;
  RLECC7 = $1D;
  RLECC8 = $1E;
  RLECCN = $7F;

  ExVals: set of Byte = [RLECC3,RLECC4,RLECC5,RLECC6,RLECC7,RLECC8,RLECCN];

function Q_GetPackRLESize(Source: Pointer; SrcL: Cardinal): Cardinal;
var
  P1: PByte;
  Cnt: Cardinal;
  B: Byte;
begin
  P1 := Source;
  Result := 0;
  while SrcL <> 0 do
  begin
    Cnt := 1;
    B := P1^;
    Dec(SrcL);
    Inc(P1);
    while SrcL <> 0 do
      if P1^ = B then
      begin
        Inc(P1);
        Dec(SrcL);
        Inc(Cnt);
      end else
        Break;
    if not (B in ExVals) then
    begin
      if Cnt = 1 then
        Inc(Result)
      else if Cnt < 9 then
        Inc(Result,2)
      else
      begin
        Inc(Result,3);
        while Cnt > 127 do
        begin
          Inc(Result);
          Cnt := Cnt shr 7;
        end;
      end;
    end else
    begin
      if Cnt < 3 then
        Inc(Result,2)
      else
      begin
        Inc(Result,3);
        while Cnt > 127 do
        begin
          Inc(Result);
          Cnt := Cnt shr 7;
        end;
      end;
    end;
  end;
end;

function Q_GetUnpackRLESize(Source: Pointer; SrcL: Cardinal): Cardinal;
var
  P1: PByte;
  N,N1,Cnt,Q: Cardinal;
  B: Byte;
begin
  P1 := Source;
  Result := 0;
  while SrcL <> 0 do
  begin
    B := P1^;
    Dec(SrcL);
    Inc(P1);
    if not (B in ExVals) then
      Inc(Result)
    else if B = RLECCN then
    begin
      N := 0;
      repeat
        Inc(N);
      until PRLECnt(P1)^[N] and $80 = 0;
      Cnt := 0;
      N1 := N+1;
      repeat
        Q := PRLECnt(P1)^[N];
        Cnt := (Cnt shl 7) or (Q and $7F);
        Dec(N);
      until N = 0;
      Inc(Result,Cnt);
      Dec(SrcL,N1);
      Inc(P1,N1);
    end else
    begin
      Q := P1^;
      Dec(SrcL);
      Inc(P1);
      case B of
        RLECC3:
          Inc(Result,3);
        RLECC4:
          begin
            if not (Byte(Q) in ExVals) then
              Inc(Result,4)
            else
              Inc(Result,2);
          end;
        RLECC5:
          Inc(Result,5);
        RLECC6:
          begin
            if Byte(Q) in ExVals then
              Inc(Result)
            else
              Inc(Result,6);
          end;
        RLECC7:
          Inc(Result,7);
        else
          Inc(Result,8);
      end;
    end;
  end;
end;

function Q_PackRLE(Source, Dest: Pointer; SrcL: Cardinal): Cardinal; overload;
var
  P1: PByte;
  P2: PRLECnt;
  Cnt: Cardinal;
  B: Byte;
begin
  P1 := Source;
  P2 := Dest;
  while SrcL <> 0 do
  begin
    Cnt := 1;
    B := P1^;
    Dec(SrcL);
    Inc(P1);
    while SrcL <> 0 do
      if P1^ = B then
      begin
        Inc(P1);
        Dec(SrcL);
        Inc(Cnt);
      end else
        Break;
    if not (B in ExVals) then
    begin
      if Cnt = 1 then
      begin
        PByte(P2)^ := B;
        Inc(PByte(P2));
      end
      else if Cnt > 8 then
      begin
        P2^[0] := RLECCN;
        P2^[1] := B;
        Inc(PByte(P2),2);
        while Cnt > 127 do
        begin
          PByte(P2)^ := Byte(Cnt) or $80;
          Inc(PByte(P2));
          Cnt := Cnt shr 7;
        end;
        PByte(P2)^ := Cnt;
        Inc(PByte(P2));
      end else
      begin
        case Cnt of
          2: P2^[0] := B;
          3: P2^[0] := RLECC3;
          4: P2^[0] := RLECC4;
          5: P2^[0] := RLECC5;
          6: P2^[0] := RLECC6;
          7: P2^[0] := RLECC7;
          8: P2^[0] := RLECC8;
        end;
        P2^[1] := B;
        Inc(PByte(P2),2);
      end;
    end else
    begin
      P2^[1] := B;
      if Cnt = 1 then
      begin
        P2^[0] := RLECC6;
        Inc(PByte(P2),2);
      end
      else if Cnt = 2 then
      begin
        P2^[0] := RLECC4;
        Inc(PByte(P2),2);
      end else
      begin
        P2^[0] := RLECCN;
        Inc(PByte(P2),2);
        while Cnt > 127 do
        begin
          PByte(P2)^ := Byte(Cnt) or $80;
          Inc(PByte(P2));
          Cnt := Cnt shr 7;
        end;
        PByte(P2)^ := Cnt;
        Inc(PByte(P2));
      end;
    end;
  end;
  Result := LongWord(P2);
  Dec(Result,LongWord(Dest));
end;

function Q_UnpackRLE(Source, Dest: Pointer; SrcL: Cardinal): Cardinal; overload;
var
  P1,P2: PByte;
  N,N1,Cnt: Cardinal;
  Q,B: LongWord;
begin
  P1 := Source;
  P2 := Dest;
  while SrcL <> 0 do
  begin
    B := P1^;
    Dec(SrcL);
    Inc(P1);
    if not (Byte(B) in ExVals) then
    begin
      P2^ := B;
      Inc(P2);
    end else
    begin
      if Byte(B) = RLECCN then
      begin
        N := 0;
        repeat
          Inc(N);
        until PRLECnt(P1)^[N] and $80 = 0;
        Cnt := 0;
        N1 := N+1;
        repeat
          Q := PRLECnt(P1)^[N];
          Cnt := (Cnt shl 7) or (Q and $7F);
          Dec(N);
        until N = 0;
        Q_FillChar(P2,Cnt,P1^);
        Inc(P2,Cnt);
        Dec(SrcL,N1);
        Inc(P1,N1);
      end else
      begin
        Q := P1^;
        Dec(SrcL);
        Inc(P1);
        P2^ := Byte(Q);
        if Byte(B) = RLECC4 then
        begin
          if not (Byte(Q) in ExVals) then
          begin
            PRLECnt(P2)^[1] := Byte(Q);
            PRLECnt(P2)^[2] := Byte(Q);
            PRLECnt(P2)^[3] := Byte(Q);
            Inc(P2,4);
          end else
          begin
            PRLECnt(P2)^[1] := Byte(Q);
            Inc(P2,2);
          end;
        end
        else if Byte(B) = RLECC6 then
        begin
          if Byte(Q) in ExVals then
            Inc(P2)
          else
          begin
            PRLECnt(P2)^[1] := Byte(Q);
            PRLECnt(P2)^[2] := Byte(Q);
            PRLECnt(P2)^[3] := Byte(Q);
            PRLECnt(P2)^[4] := Byte(Q);
            PRLECnt(P2)^[5] := Byte(Q);
            Inc(P2,6);
          end;
        end else
        begin
          PRLECnt(P2)^[1] := Byte(Q);
          PRLECnt(P2)^[2] := Byte(Q);
          Inc(P2,3);
          if Byte(B) >= RLECC5 then
          begin
            P2^ := Byte(Q);
            PRLECnt(P2)^[1] := Byte(Q);
            Inc(P2,2);
            if Byte(B) >= RLECC7 then
            begin
              P2^ := Byte(Q);
              PRLECnt(P2)^[1] := Byte(Q);
              Inc(P2,2);
              if Byte(B) = RLECC8 then
              begin
                P2^ := Byte(Q);
                Inc(P2);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  Result := LongWord(P2);
  Dec(Result,LongWord(Dest));
end;

function Q_PackRLE(const S: string; MaxL: Integer): string; overload;
var
  L: Integer;
  P: PByte;
begin
  L := Length(S);
  if MaxL = -1 then
    MaxL := Q_GetPackRLESize(Pointer(S),L);
  SetString(Result,nil,MaxL);
  P := Pointer(Result);
  L := Q_PackRLE(Pointer(S),P,L);
  if L > 0 then
  begin
    Dec(P,4);
    PLong(P)^ := L;
    Inc(P,L+4);
    P^ := 0;
  end else
    Result := '';
end;

function Q_UnpackRLE(const S: string; MaxL: Integer): string; overload;
var
  L: Integer;
  P: PByte;
begin
  L := Length(S);
  if MaxL = -1 then
    MaxL := Q_GetUnpackRLESize(Pointer(S),L);
  SetString(Result,nil,MaxL);
  P := Pointer(Result);
  L := Q_UnpackRLE(Pointer(S),P,L);
  if L > 0 then
  begin
    Dec(P,4);
    PLong(P)^ := L;
    Inc(P,L+4);
    P^ := 0;
  end else
    Result := '';
end;

function Q_PStrLen(P: PChar): Integer;
asm
        TEST    EAX,EAX
        JE      @@qt
        PUSH    EBX
        LEA     EDX,[EAX+1]
@L1:    MOV     EBX,[EAX]
        ADD     EAX,4
        LEA     ECX,[EBX-$01010101]
        NOT     EBX
        AND     ECX,EBX
        AND     ECX,$80808080
        JZ      @L1
        TEST    ECX,$00008080
        JZ      @L2
        SHL     ECX,16
        SUB     EAX,2
@L2:    SHL     ECX,9
        SBB     EAX,EDX
        POP     EBX
@@qt:
end;

function Q_IsEmptyStr(const S, EmptyChars: string): Boolean; overload;
asm
        TEST    EAX,EAX
        JE      @@qt1
        MOV     ECX,[EAX-4]
        DEC     ECX
        JS      @@qt1
        PUSH    EDI
        PUSH    EBX
        PUSH    ESI
        MOV     EDI,EAX
        TEST    EDX,EDX
        JE      @@qt0
        MOV     ESI,[EDX-4]
        DEC     ESI
        JS      @@qt0
@@lp1:  MOV     AL,BYTE PTR [EDI+ECX]
        MOV     EBX,ESI
@@lp2:  CMP     AL,BYTE PTR [EDX+EBX]
        JE      @@nx
        DEC     EBX
        JNS     @@lp2
        JMP     @@qt0
@@nx:   DEC     ECX
        JNS     @@lp1
        POP     ESI
        POP     EBX
        POP     EDI
@@qt1:  MOV     EAX,1
        RET
@@qt0:  XOR     EAX,EAX
        POP     ESI
        POP     EBX
        POP     EDI
end;

function Q_IsEmptyStr(const S: string; const EmptyChars: TCharSet): Boolean; overload;
asm
        TEST    EAX,EAX
        JE      @@qt1
        MOV     ECX,[EAX-4]
        DEC     ECX
        JS      @@qt1
        PUSH    EBX
@@lp:   MOVZX   EBX,BYTE PTR [EAX+ECX]
        BT      [EDX],EBX
        JNC     @@qt0
        DEC     ECX
        JNS     @@lp
        POP     EBX
@@qt1:  MOV     EAX,1
        RET
@@qt0:  POP     EBX
        XOR     EAX,EAX
end;

function Q_IsEmptyStr(const S: string): Boolean; overload;
asm
        TEST    EAX,EAX
        JE      @@qt1
        MOV     ECX,[EAX-4]
        MOV     DL,32
        DEC     ECX
        JS      @@qt1
@@lp:   CMP     DL,BYTE PTR [EAX+ECX]
        JB      @@qt0
        DEC     ECX
        JNS     @@lp
@@qt1:  MOV     EAX,1
        RET
@@qt0:  XOR     EAX,EAX
end;

function Q_CharCount(const S: string; Ch: Char): Integer;
asm
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@zq
        PUSH    EBX
        LEA     EBX,[EAX-1]
        XOR     EAX,EAX
@@lp:   CMP     DL,BYTE PTR [EBX+ECX]
        JE      @@fn
        DEC     ECX
        JNE     @@lp
        POP     EBX
        RET
@@fn:   INC     EAX
        DEC     ECX
        JNE     @@lp
        POP     EBX
        RET
@@zq:   XOR     EAX,EAX
@@qt:
end;

function Q_CharsCount(const S: string; const CharSet: TCharSet): Integer;
asm
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@zq
        PUSH    EBX
        PUSH    ESI
        LEA     EBX,[EAX-1]
        XOR     EAX,EAX
@@lp:   MOVZX   ESI,BYTE PTR [EBX+ECX]
        BT      [EDX],ESI
        JC      @@fn
        DEC     ECX
        JNE     @@lp
        POP     ESI
        POP     EBX
        RET
@@fn:   INC     EAX
        DEC     ECX
        JNE     @@lp
        POP     ESI
        POP     EBX
        RET
@@zq:   XOR     EAX,EAX
@@qt:
end;

function Q_GetCharStr(const S: string): string;
var
  CS: TCharSet;
begin
  CS := Q_GetCharSet(S);
  Result := Q_CharSetToStr(CS);
end;

function Q_CountOfWords(const S, Delimiters: string): Integer; overload;
var
  DelimsMap: PDelimsMap;
  I: Integer;
  L: LongWord;
  P: PChar;
  A: Boolean;
begin
  asm
        CALL    SysInit.@GetTls
        LEA     EAX,[EAX+UDelimsMap]
        MOV     DelimsMap,EAX
  end;
  L := Length(Delimiters);
  if L <> 0 then
  begin
    Q_FillLong(0,DelimsMap,64);
    for I := 1 to L do
      DelimsMap^[Delimiters[I]] := True;
  end;
  P := Pointer(S);
  A := False;
  Result := 0;
  for I := 1 to Length(S) do
  begin
    if not DelimsMap^[P^] then
    begin
      if not A then
      begin
        A := True;
        Inc(Result);
      end;
    end
    else if A then
      A := False;
    Inc(P);
  end;
end;

function Q_CountOfWords(const S: string; const Delimiters: TCharSet): Integer; overload;
asm
        PUSH    EBX
        TEST    EAX,EAX
        JE      @@q0
        MOV     ECX,[EAX-4]
        MOV     EBX,EAX
        DEC     ECX
        JS      @@qz
        PUSH    ESI
        XOR     EAX,EAX
        JMP     @@lp2
@@iw:   INC     EAX
        DEC     ECX
        JS      @@ex
@@lp1:  MOVZX   ESI,BYTE PTR [EBX+ECX]
        BT      [EDX],ESI
        JC      @@nx
        DEC     ECX
        JNS     @@lp1
@@ex:   POP     ESI
        POP     EBX
        RET
@@lp2:  MOVZX   ESI,BYTE PTR [EBX+ECX]
        BT      [EDX],ESI
        JNC     @@iw
@@nx:   DEC     ECX
        JNS     @@lp2
        POP     ESI
        POP     EBX
        RET
@@qz:   XOR     EAX,EAX
@@q0:   POP     EBX
end;

function Q_StrCheckSum(const S: string): LongWord;
asm
        TEST    EAX,EAX
        JE      @@qt
        LEA     EDX,[EAX-1]
        MOV     ECX,[EAX-4]
        XOR     EAX,EAX
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EBX
@@lp:   MOVZX   EBX,BYTE PTR [EDX+ECX]
        ADD     EAX,EBX
        DEC     ECX
        JNE     @@lp
        POP     EBX
@@qt:
end;

function Q_PStrCheckSum(P: Pointer): LongWord;
asm
        TEST    EAX,EAX
        JE      @@qt
        LEA     EDX,[EAX-1]
        MOV     ECX,[EAX-4]
        XOR     EAX,EAX
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EBX
@@lp:   MOVZX   EBX,BYTE PTR [EDX+ECX]
        ADD     EAX,EBX
        DEC     ECX
        JNE     @@lp
        POP     EBX
@@qt:
end;

function Q_StrCheckXOR(const S: string): LongWord;
asm
        TEST    EAX,EAX
        JE      @@qt
        PUSH    EBX
        MOV     EBX,EAX
        MOV     ECX,[EAX-4]
        MOV     EDX,ECX
        XOR     EAX,EAX
        SHR     ECX,3
        JE      @@nx
@@lp:   XOR     EAX,[EBX]
        XOR     EAX,[EBX+4]
        ADD     EBX,8
        DEC     ECX
        JNE     @@lp
@@nx:   AND     EDX,7
        JMP     DWORD PTR @@tV[EDX*4]
@@tV:   DD      @@ex,@@t1,@@t2,@@t3
        DD      @@t3,@@t5,@@t6,@@t7
@@t1:   XOR     AL,BYTE PTR [EBX]
        JMP     @@ex
@@t2:   XOR     AX,WORD PTR [EBX]
        JMP     @@ex
@@t3:   XOR     EAX,DWORD PTR [EBX]
        JMP     @@ex
@@t5:   XOR     EAX,DWORD PTR [EBX]
        XOR     AL,BYTE PTR [EBX+4]
        JMP     @@ex
@@t6:   XOR     EAX,DWORD PTR [EBX]
        XOR     AX,WORD PTR [EBX+4]
        JMP     @@ex
@@t7:   XOR     EAX,DWORD PTR [EBX]
        XOR     EAX,DWORD PTR [EBX+4]
@@ex:   POP     EBX
@@qt:
end;

function Q_PStrCheckXOR(P: Pointer): LongWord;
asm
        TEST    EAX,EAX
        JE      @@qt
        PUSH    EBX
        MOV     EBX,EAX
        MOV     ECX,[EAX-4]
        MOV     EDX,ECX
        XOR     EAX,EAX
        SHR     ECX,3
        JE      @@nx
@@lp:   XOR     EAX,[EBX]
        XOR     EAX,[EBX+4]
        ADD     EBX,8
        DEC     ECX
        JNE     @@lp
@@nx:   AND     EDX,7
        JMP     DWORD PTR @@tV[EDX*4]
@@tV:   DD      @@ex,@@t1,@@t2,@@t3
        DD      @@t3,@@t5,@@t6,@@t7
@@t1:   XOR     AL,BYTE PTR [EBX]
        JMP     @@ex
@@t2:   XOR     AX,WORD PTR [EBX]
        JMP     @@ex
@@t3:   XOR     EAX,DWORD PTR [EBX]
        JMP     @@ex
@@t5:   XOR     EAX,DWORD PTR [EBX]
        XOR     AL,BYTE PTR [EBX+4]
        JMP     @@ex
@@t6:   XOR     EAX,DWORD PTR [EBX]
        XOR     AX,WORD PTR [EBX+4]
        JMP     @@ex
@@t7:   XOR     EAX,DWORD PTR [EBX]
        XOR     EAX,DWORD PTR [EBX+4]
@@ex:   POP     EBX
@@qt:
end;

function Q_StrHashKey(const S: string): LongWord;
asm
        TEST    EAX,EAX
        JE      @@qt
        LEA     EDX,[EAX-1]
        MOV     ECX,[EAX-4]
        XOR     EAX,EAX
        TEST    ECX,ECX
        JE      @@qt
        PUSH    ESI
        PUSH    EBX
@@lp:   MOV     ESI,EAX
        SHL     EAX,5
        MOVZX   EBX,BYTE PTR [EDX+ECX]
        ADD     EAX,ESI
        ADD     EAX,EBX
        DEC     ECX
        JNE     @@lp
        POP     EBX
        POP     ESI
@@qt:
end;

function Q_PStrHashKey(P: Pointer): LongWord;
asm
        TEST    EAX,EAX
        JE      @@qt
        LEA     EDX,[EAX-1]
        MOV     ECX,[EAX-4]
        XOR     EAX,EAX
        TEST    ECX,ECX
        JE      @@qt
        PUSH    ESI
        PUSH    EBX
@@lp:   MOV     ESI,EAX
        SHL     EAX,5
        MOVZX   EBX,BYTE PTR [EDX+ECX]
        ADD     EAX,ESI
        ADD     EAX,EBX
        DEC     ECX
        JNE     @@lp
        POP     EBX
        POP     ESI
@@qt:
end;

function Q_TextHashKey(const S: string): LongWord;
asm
        TEST    EAX,EAX
        JE      @@qt
        LEA     EDX,[EAX-1]
        MOV     ECX,[EAX-4]
        XOR     EAX,EAX
        TEST    ECX,ECX
        JE      @@qt
        PUSH    ESI
        PUSH    EBX
@@lp:   MOV     ESI,EAX
        SHL     EAX,5
        MOVZX   EBX,BYTE PTR [EDX+ECX]
        AND     EBX,$DF
        ADD     EAX,ESI
        ADD     EAX,EBX
        DEC     ECX
        JNE     @@lp
        POP     EBX
        POP     ESI
@@qt:
end;

function Q_PTextHashKey(P: Pointer): LongWord;
asm
        TEST    EAX,EAX
        JE      @@qt
        LEA     EDX,[EAX-1]
        MOV     ECX,[EAX-4]
        XOR     EAX,EAX
        TEST    ECX,ECX
        JE      @@qt
        PUSH    ESI
        PUSH    EBX
@@lp:   MOV     ESI,EAX
        SHL     EAX,5
        MOVZX   EBX,BYTE PTR [EDX+ECX]
        AND     EBX,$DF
        ADD     EAX,ESI
        ADD     EAX,EBX
        DEC     ECX
        JNE     @@lp
        POP     EBX
        POP     ESI
@@qt:
end;

const
  CRC32_Table: array[0..255] of LongWord =
    ($00000000,$77073096,$EE0E612C,$990951BA,$076DC419,$706AF48F,$E963A535,$9E6495A3,
     $0EDB8832,$79DCB8A4,$E0D5E91E,$97D2D988,$09B64C2B,$7EB17CBD,$E7B82D07,$90BF1D91,
     $1DB71064,$6AB020F2,$F3B97148,$84BE41DE,$1ADAD47D,$6DDDE4EB,$F4D4B551,$83D385C7,
     $136C9856,$646BA8C0,$FD62F97A,$8A65C9EC,$14015C4F,$63066CD9,$FA0F3D63,$8D080DF5,
     $3B6E20C8,$4C69105E,$D56041E4,$A2677172,$3C03E4D1,$4B04D447,$D20D85FD,$A50AB56B,
     $35B5A8FA,$42B2986C,$DBBBC9D6,$ACBCF940,$32D86CE3,$45DF5C75,$DCD60DCF,$ABD13D59,
     $26D930AC,$51DE003A,$C8D75180,$BFD06116,$21B4F4B5,$56B3C423,$CFBA9599,$B8BDA50F,
     $2802B89E,$5F058808,$C60CD9B2,$B10BE924,$2F6F7C87,$58684C11,$C1611DAB,$B6662D3D,
     $76DC4190,$01DB7106,$98D220BC,$EFD5102A,$71B18589,$06B6B51F,$9FBFE4A5,$E8B8D433,
     $7807C9A2,$0F00F934,$9609A88E,$E10E9818,$7F6A0DBB,$086D3D2D,$91646C97,$E6635C01,
     $6B6B51F4,$1C6C6162,$856530D8,$F262004E,$6C0695ED,$1B01A57B,$8208F4C1,$F50FC457,
     $65B0D9C6,$12B7E950,$8BBEB8EA,$FCB9887C,$62DD1DDF,$15DA2D49,$8CD37CF3,$FBD44C65,
     $4DB26158,$3AB551CE,$A3BC0074,$D4BB30E2,$4ADFA541,$3DD895D7,$A4D1C46D,$D3D6F4FB,
     $4369E96A,$346ED9FC,$AD678846,$DA60B8D0,$44042D73,$33031DE5,$AA0A4C5F,$DD0D7CC9,
     $5005713C,$270241AA,$BE0B1010,$C90C2086,$5768B525,$206F85B3,$B966D409,$CE61E49F,
     $5EDEF90E,$29D9C998,$B0D09822,$C7D7A8B4,$59B33D17,$2EB40D81,$B7BD5C3B,$C0BA6CAD,
     $EDB88320,$9ABFB3B6,$03B6E20C,$74B1D29A,$EAD54739,$9DD277AF,$04DB2615,$73DC1683,
     $E3630B12,$94643B84,$0D6D6A3E,$7A6A5AA8,$E40ECF0B,$9309FF9D,$0A00AE27,$7D079EB1,
     $F00F9344,$8708A3D2,$1E01F268,$6906C2FE,$F762575D,$806567CB,$196C3671,$6E6B06E7,
     $FED41B76,$89D32BE0,$10DA7A5A,$67DD4ACC,$F9B9DF6F,$8EBEEFF9,$17B7BE43,$60B08ED5,
     $D6D6A3E8,$A1D1937E,$38D8C2C4,$4FDFF252,$D1BB67F1,$A6BC5767,$3FB506DD,$48B2364B,
     $D80D2BDA,$AF0A1B4C,$36034AF6,$41047A60,$DF60EFC3,$A867DF55,$316E8EEF,$4669BE79,
     $CB61B38C,$BC66831A,$256FD2A0,$5268E236,$CC0C7795,$BB0B4703,$220216B9,$5505262F,
     $C5BA3BBE,$B2BD0B28,$2BB45A92,$5CB36A04,$C2D7FFA7,$B5D0CF31,$2CD99E8B,$5BDEAE1D,
     $9B64C2B0,$EC63F226,$756AA39C,$026D930A,$9C0906A9,$EB0E363F,$72076785,$05005713,
     $95BF4A82,$E2B87A14,$7BB12BAE,$0CB61B38,$92D28E9B,$E5D5BE0D,$7CDCEFB7,$0BDBDF21,
     $86D3D2D4,$F1D4E242,$68DDB3F8,$1FDA836E,$81BE16CD,$F6B9265B,$6FB077E1,$18B74777,
     $88085AE6,$FF0F6A70,$66063BCA,$11010B5C,$8F659EFF,$F862AE69,$616BFFD3,$166CCF45,
     $A00AE278,$D70DD2EE,$4E048354,$3903B3C2,$A7672661,$D06016F7,$4969474D,$3E6E77DB,
     $AED16A4A,$D9D65ADC,$40DF0B66,$37D83BF0,$A9BCAE53,$DEBB9EC5,$47B2CF7F,$30B5FFE9,
     $BDBDF21C,$CABAC28A,$53B39330,$24B4A3A6,$BAD03605,$CDD70693,$54DE5729,$23D967BF,
     $B3667A2E,$C4614AB8,$5D681B02,$2A6F2B94,$B40BBE37,$C30C8EA1,$5A05DF1B,$2D02EF8D);

function Q_CRC32(P: Pointer; L: Cardinal): LongWord;
asm
        PUSH    EBX
        MOV     EBX,EAX
        MOV     EAX,$FFFFFFFF
        PUSH    ESI
        TEST    EDX,EDX
        JE      @@qt
@@lp:   MOVZX   ESI,BYTE PTR [EBX]
        MOVZX   ECX,AL
        XOR     ECX,ESI
        SHR     EAX,8
        XOR     EAX,DWORD PTR [ECX*4+CRC32_Table]
        INC     EBX
        DEC     EDX
        JNE     @@lp
@@qt:   POP     ESI
        NOT     EAX
        POP     EBX
end;

function Q_NextCRC32(var CRC32: LongWord; P: Pointer; L: Cardinal): LongWord;
asm
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EBX
        PUSH    EAX
        MOV     EAX,[EAX]
        NOT     EAX
        PUSH    ESI
@@lp:   MOVZX   ESI,BYTE PTR [EDX]
        MOVZX   EBX,AL
        SHR     EAX,8
        XOR     EBX,ESI
        XOR     EAX,DWORD PTR [EBX*4+CRC32_Table]
        INC     EDX
        DEC     ECX
        JNE     @@lp
        POP     ESI
        POP     EDX
        NOT     EAX
        MOV     [EDX],EAX
        POP     EBX
        RET
@@qt:   MOV     EAX,[EAX]
end;

function Q_TimeStamp: Int64;
asm
        DB      $0F,$31
end;

function Q_GetCharSet(const S: string): TCharSet;
asm
        XOR     ECX,ECX
        MOV     [EDX],ECX
        MOV     [EDX+4],ECX
        MOV     [EDX+8],ECX
        MOV     [EDX+12],ECX
        MOV     [EDX+16],ECX
        MOV     [EDX+20],ECX
        MOV     [EDX+24],ECX
        MOV     [EDX+28],ECX
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        PUSH    EBX
        SUB     ECX,8
        JS      @@nx
@@lp:   MOVZX   EBX,BYTE PTR [EAX]
        BTS     [EDX],EBX
        MOVZX   EBX,BYTE PTR [EAX+1]
        BTS     [EDX],EBX
        MOVZX   EBX,BYTE PTR [EAX+2]
        BTS     [EDX],EBX
        MOVZX   EBX,BYTE PTR [EAX+3]
        BTS     [EDX],EBX
        MOVZX   EBX,BYTE PTR [EAX+4]
        BTS     [EDX],EBX
        MOVZX   EBX,BYTE PTR [EAX+5]
        BTS     [EDX],EBX
        MOVZX   EBX,BYTE PTR [EAX+6]
        BTS     [EDX],EBX
        MOVZX   EBX,BYTE PTR [EAX+7]
        BTS     [EDX],EBX
        ADD     EAX,8
        SUB     ECX,8
        JNS     @@lp
@@nx:   JMP     DWORD PTR @@tV[ECX*4+32]
@@tV:   DD      @@ex,@@t1,@@t2,@@t3
        DD      @@t4,@@t5,@@t6,@@t7
@@t7:   MOVZX   EBX,BYTE PTR [EAX+6]
        BTS     [EDX],EBX
@@t6:   MOVZX   EBX,BYTE PTR [EAX+5]
        BTS     [EDX],EBX
@@t5:   MOVZX   EBX,BYTE PTR [EAX+4]
        BTS     [EDX],EBX
@@t4:   MOVZX   EBX,BYTE PTR [EAX+3]
        BTS     [EDX],EBX
@@t3:   MOVZX   EBX,BYTE PTR [EAX+2]
        BTS     [EDX],EBX
@@t2:   MOVZX   EBX,BYTE PTR [EAX+1]
        BTS     [EDX],EBX
@@t1:   MOVZX   EBX,BYTE PTR [EAX]
        BTS     [EDX],EBX
@@ex:   POP     EBX
@@qt:
end;

function Q_CharSetToStr(const CharSet: TCharSet): string;
asm
        PUSH    EBX
        MOV     ECX,$100
        MOV     EBX,EAX
        PUSH    ESI
        MOV     EAX,EDX
        SUB     ESP,ECX
        XOR     ESI,ESI
        XOR     EDX,EDX
@@lp:   BT      [EBX],EDX
        JC      @@mm
@@nx:   INC     EDX
        DEC     ECX
        JNE     @@lp
        MOV     ECX,ESI
        MOV     EDX,ESP
        CALL    System.@LStrFromPCharLen
        ADD     ESP,$100
        POP     ESI
        POP     EBX
        RET
@@mm:   MOV     [ESP+ESI],DL
        INC     ESI
        JMP     @@nx
end;

procedure Q_ComplementChar(var CharSet: TCharSet; Ch: Char);
asm
        MOVZX   ECX,DL
        BTC     [EAX],ECX
end;

procedure Q_ClearCharSet(var CharSet: TCharSet);
asm
        XOR     EDX,EDX
        MOV     [EAX],EDX
        MOV     [EAX+4],EDX
        MOV     [EAX+8],EDX
        MOV     [EAX+12],EDX
        MOV     [EAX+16],EDX
        MOV     [EAX+20],EDX
        MOV     [EAX+24],EDX
        MOV     [EAX+28],EDX
end;

procedure Q_FillCharSet(var CharSet: TCharSet);
asm
        MOV     EDX,$FFFFFFFF
        MOV     [EAX],EDX
        MOV     [EAX+4],EDX
        MOV     [EAX+8],EDX
        MOV     [EAX+12],EDX
        MOV     [EAX+16],EDX
        MOV     [EAX+20],EDX
        MOV     [EAX+24],EDX
        MOV     [EAX+28],EDX
end;

procedure Q_ComplementSet(var CharSet: TCharSet);
asm
        NOT     DWORD PTR [EAX]
        NOT     DWORD PTR [EAX+4]
        NOT     DWORD PTR [EAX+8]
        NOT     DWORD PTR [EAX+12]
        NOT     DWORD PTR [EAX+16]
        NOT     DWORD PTR [EAX+20]
        NOT     DWORD PTR [EAX+24]
        NOT     DWORD PTR [EAX+28]
end;

procedure Q_CloneCharSet(const SourceSet: TCharSet; var DestSet: TCharSet);
asm
        MOV     ECX,[EAX]
        MOV     [EDX],ECX
        MOV     ECX,[EAX+4]
        MOV     [EDX+4],ECX
        MOV     ECX,[EAX+8]
        MOV     [EDX+8],ECX
        MOV     ECX,[EAX+12]
        MOV     [EDX+12],ECX
        MOV     ECX,[EAX+16]
        MOV     [EDX+16],ECX
        MOV     ECX,[EAX+20]
        MOV     [EDX+20],ECX
        MOV     ECX,[EAX+24]
        MOV     [EDX+24],ECX
        MOV     ECX,[EAX+28]
        MOV     [EDX+28],ECX
end;

procedure Q_CharSetUnion(var DestSet: TCharSet; const SourceSet: TCharSet);
asm
        MOV     ECX,[EDX]
        OR      [EAX],ECX
        MOV     ECX,[EDX+4]
        OR      [EAX+4],ECX
        MOV     ECX,[EDX+8]
        OR      [EAX+8],ECX
        MOV     ECX,[EDX+12]
        OR      [EAX+12],ECX
        MOV     ECX,[EDX+16]
        OR      [EAX+16],ECX
        MOV     ECX,[EDX+20]
        OR      [EAX+20],ECX
        MOV     ECX,[EDX+24]
        OR      [EAX+24],ECX
        MOV     ECX,[EDX+28]
        OR      [EAX+28],ECX
end;

procedure Q_CharSetSubtract(var DestSet: TCharSet; const SourceSet: TCharSet);
asm
        MOV     ECX,[EDX]
        NOT     ECX
        AND     [EAX],ECX
        MOV     ECX,[EDX+4]
        NOT     ECX
        AND     [EAX+4],ECX
        MOV     ECX,[EDX+8]
        NOT     ECX
        AND     [EAX+8],ECX
        MOV     ECX,[EDX+12]
        NOT     ECX
        AND     [EAX+12],ECX
        MOV     ECX,[EDX+16]
        NOT     ECX
        AND     [EAX+16],ECX
        MOV     ECX,[EDX+20]
        NOT     ECX
        AND     [EAX+20],ECX
        MOV     ECX,[EDX+24]
        NOT     ECX
        AND     [EAX+24],ECX
        MOV     ECX,[EDX+28]
        NOT     ECX
        AND     [EAX+28],ECX
end;

procedure Q_CharSetIntersect(var DestSet: TCharSet; const SourceSet: TCharSet);
asm
        MOV     ECX,[EDX]
        AND     [EAX],ECX
        MOV     ECX,[EDX+4]
        AND     [EAX+4],ECX
        MOV     ECX,[EDX+8]
        AND     [EAX+8],ECX
        MOV     ECX,[EDX+12]
        AND     [EAX+12],ECX
        MOV     ECX,[EDX+16]
        AND     [EAX+16],ECX
        MOV     ECX,[EDX+20]
        AND     [EAX+20],ECX
        MOV     ECX,[EDX+24]
        AND     [EAX+24],ECX
        MOV     ECX,[EDX+28]
        AND     [EAX+28],ECX
end;

procedure Q_CharSetXOR(var DestSet: TCharSet; const SourceSet: TCharSet);
asm
        MOV     ECX,[EDX]
        XOR     [EAX],ECX
        MOV     ECX,[EDX+4]
        XOR     [EAX+4],ECX
        MOV     ECX,[EDX+8]
        XOR     [EAX+8],ECX
        MOV     ECX,[EDX+12]
        XOR     [EAX+12],ECX
        MOV     ECX,[EDX+16]
        XOR     [EAX+16],ECX
        MOV     ECX,[EDX+20]
        XOR     [EAX+20],ECX
        MOV     ECX,[EDX+24]
        XOR     [EAX+24],ECX
        MOV     ECX,[EDX+28]
        XOR     [EAX+28],ECX
end;

function Q_IsSubset(const LeftSet, RightSet: TCharSet): Boolean;
asm
        MOV     ECX,[EDX]
        NOT     ECX
        AND     ECX,[EAX]
        JNE     @@q0
        MOV     ECX,[EDX+4]
        NOT     ECX
        AND     ECX,[EAX+4]
        JNE     @@q0
        MOV     ECX,[EDX+8]
        NOT     ECX
        AND     ECX,[EAX+8]
        JNE     @@q0
        MOV     ECX,[EDX+12]
        NOT     ECX
        AND     ECX,[EAX+12]
        JNE     @@q0
        MOV     ECX,[EDX+16]
        NOT     ECX
        AND     ECX,[EAX+16]
        JNE     @@q0
        MOV     ECX,[EDX+20]
        NOT     ECX
        AND     ECX,[EAX+20]
        JNE     @@q0
        MOV     ECX,[EDX+24]
        NOT     ECX
        AND     ECX,[EAX+24]
        JNE     @@q0
        MOV     ECX,[EDX+28]
        NOT     ECX
        AND     ECX,[EAX+28]
        JNE     @@q0
        MOV     EAX,1
        RET
@@q0:   XOR     EAX,EAX
end;

function Q_IsSuperset(const LeftSet, RightSet: TCharSet): Boolean;
asm
        XCHG    EAX,EDX
        CALL    Q_IsSubset
end;

function Q_IsEqualSet(const LeftSet, RightSet: TCharSet): Boolean;
asm
        MOV     ECX,[EDX]
        XOR     ECX,[EAX]
        JNE     @@q0
        MOV     ECX,[EDX+4]
        XOR     ECX,[EAX+4]
        JNE     @@q0
        MOV     ECX,[EDX+8]
        XOR     ECX,[EAX+8]
        JNE     @@q0
        MOV     ECX,[EDX+12]
        XOR     ECX,[EAX+12]
        JNE     @@q0
        MOV     ECX,[EDX+16]
        XOR     ECX,[EAX+16]
        JNE     @@q0
        MOV     ECX,[EDX+20]
        XOR     ECX,[EAX+20]
        JNE     @@q0
        MOV     ECX,[EDX+24]
        XOR     ECX,[EAX+24]
        JNE     @@q0
        MOV     ECX,[EDX+28]
        XOR     ECX,[EAX+28]
        JNE     @@q0
        MOV     EAX,1
        RET
@@q0:   XOR     EAX,EAX
end;

function Q_IsEmptySet(const CharSet: TCharSet): Boolean;
asm
        MOV     EDX,[EAX]
        OR      EDX,[EAX+4]
        OR      EDX,[EAX+8]
        OR      EDX,[EAX+12]
        OR      EDX,[EAX+16]
        OR      EDX,[EAX+20]
        OR      EDX,[EAX+24]
        OR      EDX,[EAX+28]
        JNE     @@zq
        MOV     EAX,1
        RET
@@zq:   XOR     EAX,EAX
end;

const
  BitTable: array[0..255] of Byte =
    (0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
     1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
     1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
     2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
     1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
     2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
     2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
     3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8);

function Q_CharSetCharCount(const CharSet: TCharSet): Integer;
asm
        PUSH    EBX
        MOV     EBX,EAX
        XOR     EAX,EAX
        MOVZX   ECX,BYTE PTR [EBX]
        MOVZX   EDX,BYTE PTR [EBX+1]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   EDX,BYTE PTR [EDX+BitTable]
        ADD     EAX,EDX
        MOVZX   ECX,BYTE PTR [EBX+2]
        MOVZX   EDX,BYTE PTR [EBX+3]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   EDX,BYTE PTR [EDX+BitTable]
        ADD     EAX,EDX
        MOVZX   ECX,BYTE PTR [EBX+4]
        MOVZX   EDX,BYTE PTR [EBX+5]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   EDX,BYTE PTR [EDX+BitTable]
        ADD     EAX,EDX
        MOVZX   ECX,BYTE PTR [EBX+6]
        MOVZX   EDX,BYTE PTR [EBX+7]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   EDX,BYTE PTR [EDX+BitTable]
        ADD     EAX,EDX
        MOVZX   ECX,BYTE PTR [EBX+8]
        MOVZX   EDX,BYTE PTR [EBX+9]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   EDX,BYTE PTR [EDX+BitTable]
        ADD     EAX,EDX
        MOVZX   ECX,BYTE PTR [EBX+10]
        MOVZX   EDX,BYTE PTR [EBX+11]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   EDX,BYTE PTR [EDX+BitTable]
        ADD     EAX,EDX
        MOVZX   ECX,BYTE PTR [EBX+12]
        MOVZX   EDX,BYTE PTR [EBX+13]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   EDX,BYTE PTR [EDX+BitTable]
        ADD     EAX,EDX
        MOVZX   ECX,BYTE PTR [EBX+14]
        MOVZX   EDX,BYTE PTR [EBX+15]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   EDX,BYTE PTR [EDX+BitTable]
        ADD     EAX,EDX
        MOVZX   ECX,BYTE PTR [EBX+16]
        MOVZX   EDX,BYTE PTR [EBX+17]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   EDX,BYTE PTR [EDX+BitTable]
        ADD     EAX,EDX
        MOVZX   ECX,BYTE PTR [EBX+18]
        MOVZX   EDX,BYTE PTR [EBX+19]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   EDX,BYTE PTR [EDX+BitTable]
        ADD     EAX,EDX
        MOVZX   ECX,BYTE PTR [EBX+20]
        MOVZX   EDX,BYTE PTR [EBX+21]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   EDX,BYTE PTR [EDX+BitTable]
        ADD     EAX,EDX
        MOVZX   ECX,BYTE PTR [EBX+22]
        MOVZX   EDX,BYTE PTR [EBX+23]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   EDX,BYTE PTR [EDX+BitTable]
        ADD     EAX,EDX
        MOVZX   ECX,BYTE PTR [EBX+24]
        MOVZX   EDX,BYTE PTR [EBX+25]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   EDX,BYTE PTR [EDX+BitTable]
        ADD     EAX,EDX
        MOVZX   ECX,BYTE PTR [EBX+26]
        MOVZX   EDX,BYTE PTR [EBX+27]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   EDX,BYTE PTR [EDX+BitTable]
        ADD     EAX,EDX
        MOVZX   ECX,BYTE PTR [EBX+28]
        MOVZX   EDX,BYTE PTR [EBX+29]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   EDX,BYTE PTR [EDX+BitTable]
        ADD     EAX,EDX
        MOVZX   ECX,BYTE PTR [EBX+30]
        MOVZX   EDX,BYTE PTR [EBX+31]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   EDX,BYTE PTR [EDX+BitTable]
        ADD     EAX,EDX
        POP     EBX
end;

function Q_IsInteger(const S: string): Boolean;
var
  L,I: Integer;
  P: PChar;
  C: Char;
begin
  P := Pointer(S);
  L := Length(S);
  Result := False;
  while (L>0) and (P^=' ') do
  begin
    Dec(L);
    Inc(P);
  end;
  if L > 0 then
  begin
    C := P^;
    Inc(P);
    if C in ['-','+'] then
    begin
      C := P^;
      Dec(L);
      Inc(P);
      if L = 0 then
        Exit;
    end;
    if ((L<10) and (C in ['0'..'9'])) or ((L=10) and (C in ['0'..'1'])) then
    begin
      for I := 1 to L-1 do
      begin
        if not (P^ in ['0'..'9']) then
          Exit;
        Inc(P);
      end;
      Result := True;
    end
    else if (L=10) and (C='2') then
      Result := Q_BetweenInt64(S,-2147483647-1,2147483647);
  end;
end;

function Q_IsCardinal(const S: string): Boolean;
var
  L,I: Integer;
  P: PChar;
  C: Char;
begin
  P := Pointer(S);
  L := Length(S);
  Result := False;
  while (L>0) and (P^=' ') do
  begin
    Dec(L);
    Inc(P);
  end;
  if L > 0 then
  begin
    C := P^;
    Inc(P);
    if ((L<10) and (C in ['0'..'9'])) or ((L=10) and (C in ['0'..'3'])) then
    begin
      for I := 1 to L-1 do
      begin
        if not (P^ in ['0'..'9']) then
          Exit;
        Inc(P);
      end;
      Result := True;
    end
    else if (C='$') or ((L=10) and (C='4')) then
      Result := Q_BetweenInt64(S,0,4294967295);
  end;
end;

function Q_IsDecimal(const S: string): Boolean;
asm
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        DEC     ECX
        JS      @@zq
@@lp:   MOV     DL,[EAX+ECX]
        ADD     DL,$D0
        SUB     DL,$0A
        JNB     @@zq
        DEC     ECX
        JNS     @@lp
        MOV     EAX,1
@@qt:   RET
@@zq:   XOR     EAX,EAX
end;

function Q_IsHexadecimal(const S: string): Boolean;
const
  HexDigsSet: TCharSet = ['0'..'9','A'..'F','a'..'f'];
asm
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        PUSH    ESI
        DEC     ECX
        JS      @@zq
        LEA     ESI,HexDigsSet
@@lp:   MOVZX   EDX,BYTE PTR [EAX+ECX]
        BT      [ESI],EDX
        JNC     @@zq
        DEC     ECX
        JNS     @@lp
        POP     ESI
        MOV     EAX,1
@@qt:   RET
@@zq:   POP     ESI
        XOR     EAX,EAX
end;

function Q_IsOctal(const S: string): Boolean;
asm
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        DEC     ECX
        JS      @@zq
@@lp:   MOV     DL,[EAX+ECX]
        ADD     DL,$D0
        SUB     DL,$08
        JNB     @@zq
        DEC     ECX
        JNS     @@lp
        MOV     EAX,1
@@qt:   RET
@@zq:   XOR     EAX,EAX
end;

function Q_IsBinary(const S: string): Boolean;
asm
        TEST    EAX,EAX
        JE      @@qt
        MOV     ECX,[EAX-4]
        DEC     ECX
        JS      @@zq
@@lp:   MOV     DL,[EAX+ECX]
        ADD     DL,$D0
        SUB     DL,$02
        JNB     @@zq
        DEC     ECX
        JNS     @@lp
        MOV     EAX,1
@@qt:   RET
@@zq:   XOR     EAX,EAX
end;

function Q_IsFloat(const S: string): Boolean;
var
  L: Integer;
  P: PChar;
begin
  P := Pointer(S);
  L := Length(S);
  Result := False;
  while (L>0) and (P^=' ') do
  begin
    Dec(L);
    Inc(P);
  end;
  if L > 0 then
  begin
    if P^ in ['-','+'] then
    begin
      Dec(L);
      Inc(P);
      if L = 0 then
        Exit;
    end;
    if not (P^ in ['0'..'9']) then
      Exit;
    repeat
      Dec(L);
      Inc(P);
    until (L=0) or not (P^ in ['0'..'9']);
    if L = 0 then
    begin
      Result := True;
      Exit;
    end;
    if P^ = DecimalSeparator then
    begin
      Dec(L);
      Inc(P);
      if (L=0) or not (P^ in ['0'..'9']) then
        Exit;
      repeat
        Dec(L);
        Inc(P);
      until (L=0) or not (P^ in ['0'..'9']);
      if L = 0 then
      begin
        Result := True;
        Exit;
      end;
    end;
    if P^ in ['E','e'] then
    begin
      Dec(L);
      Inc(P);
      if (L<>0) and (P^ in ['-','+']) then
      begin
        Dec(L);
        Inc(P);
      end;
      if (L=0) or not (P^ in ['0'..'9']) then
        Exit;
      repeat
        Dec(L);
        Inc(P);
      until (L=0) or not (P^ in ['0'..'9']);
      Result := L = 0;
    end;
  end;
end;

function Q_AdjustSeparator(const S: string): string;
var
  I,L: Integer;
  P: PChar;
begin
  L := Length(S);
  if DecimalSeparator = ',' then
  begin
    I := Q_ScanByte(Byte('.'),Pointer(S),L);
    if I <> -1 then
    begin
      Result := '';
      SetLength(Result,L);
      P := Pointer(Result);
      if L <= 32 then
        Q_TinyCopy(Pointer(S),P,L)
      else
        Q_CopyMem(Pointer(S),P,L);
      Inc(P,I);
      P^ := ',';
    end else
      Result := S;
  end else
  begin
    I := Q_ScanByte(Byte(','),Pointer(S),L);
    if I <> -1 then
    begin
      Result := '';
      SetLength(Result,L);
      P := Pointer(Result);
      if L <= 32 then
        Q_TinyCopy(Pointer(S),P,L)
      else
        Q_CopyMem(Pointer(S),P,L);
      Inc(P,I);
      P^ := '.';
    end else
      Result := S;
  end
end;

function Q_BetweenInt(const S: string; LowBound, HighBound: Integer): Boolean;
var
  N: Integer;
begin
  Result := Q_StrToInt(S,N) and (N>=LowBound) and (N<=HighBound);
end;

function Q_BetweenUInt(const S: string; LowBound, HighBound: LongWord): Boolean;
var
  N: LongWord;
begin
  Result := Q_StrToUInt(S,N) and (N>=LowBound) and (N<=HighBound);
end;

function Q_BetweenInt64(const S: string; LowBound, HighBound: Int64): Boolean;
var
  N: Int64;
begin
  Result := Q_StrToInt64(S,N) and (N>=LowBound) and (N<=HighBound);
end;

function Q_BetweenFloat(const S: string; LowBound, HighBound: Extended): Boolean;
var
  N: Extended;
begin
  Result := TextToFloat(PChar(S),N,fvExtended) and (N>=LowBound) and (N<=HighBound);
end;

function Q_BetweenCurr(const S: string; LowBound, HighBound: Currency): Boolean;
var
  N: Currency;
begin
  Result := TextToFloat(PChar(S),N,fvCurrency) and (N>=LowBound) and (N<=HighBound);
end;

function Q_StrToInt(const S: string; var V: Integer): Boolean;
var
  P: PChar;
  C: Integer;
  Sign: Boolean;
begin
  V := 0;
  P := Pointer(S);
  if not Assigned(P) then
  begin
    Result := False;
    Exit;
  end;
  while P^ = ' ' do
    Inc(P);
  if P^ = '-' then
  begin
    Sign := True;
    Inc(P);
  end else
  begin
    Sign := False;
    if P^ = '+' then
      Inc(P);
  end;
  if P^ <> '$' then
  begin
    if P^ = #0 then
    begin
      Result := False;
      Exit;
    end;
    repeat
      C := Byte(P^);
      if Char(C) in ['0'..'9'] then
        Dec(C,48)
      else
        Break;
      if (V<0) or (V>$CCCCCCC) then
      begin
        Result := False;
        Exit;
      end;
      V := V*10 + C;
      Inc(P);
    until False;
    if V < 0 then
    begin
      Result := (LongWord(V)=$80000000) and Sign and (C=0);
      Exit;
    end;
  end else
  begin
    Inc(P);
    repeat
      C := Byte(P^);
      case Char(C) of
      '0'..'9': Dec(C,48);
      'A'..'F': Dec(C,55);
      'a'..'f': Dec(C,87);
      else
        Break;
      end;
      if LongWord(V) >= $10000000 then
      begin
        Result := False;
        Exit;
      end;
      V := (V shl 4) or C;
      Inc(P);
    until False;
    if Sign and (LongWord(V)=$80000000) then
    begin
      Result := False;
      Exit;
    end;
  end;
  if Sign then
    V := -V;
  Result := C=0;
end;

function Q_StrToUInt(const S: string; var V: LongWord): Boolean;
var
  P: PChar;
  C: LongWord;
begin
  V := 0;
  P := Pointer(S);
  if not Assigned(P) then
  begin
    Result := False;
    Exit;
  end;
  while P^ = ' ' do
    Inc(P);
  if P^ <> '$' then
  begin
    if P^ = #0 then
    begin
      Result := False;
      Exit;
    end;
    repeat
      C := Byte(P^);
      if Char(C) in ['0'..'9'] then
        Dec(C,48)
      else
        Break;
      if (V<$19999999) or ((V=$19999999) and (C<6)) then
        V := V*10 + C
      else
      begin
        Result := False;
        Exit;
      end;
      Inc(P);
    until False;
  end else
  begin
    Inc(P);
    repeat
      C := Byte(P^);
      case Char(C) of
      '0'..'9': Dec(C,48);
      'A'..'F': Dec(C,55);
      'a'..'f': Dec(C,87);
      else
        Break;
      end;
      if LongWord(V) >= $10000000 then
      begin
        Result := False;
        Exit;
      end;
      V := (V shl 4) or C;
      Inc(P);
    until False;
  end;
  Result := C=0;
end;

function Q_StrToInt64(const S: string; var V: Int64): Boolean;
type
  PArr64 = ^TArr64;
  TArr64 = array[0..7] of Byte;
var
  P: PChar;
  C: LongWord;
  Sign: LongBool;
begin
  V := 0;
  P := Pointer(S);
  if not Assigned(P) then
  begin
    Result := False;
    Exit;
  end;
  while P^ = ' ' do
    Inc(P);
  if P^ = '-' then
  begin
    Sign := True;
    Inc(P);
  end else
  begin
    Sign := False;
    if P^ = '+' then
      Inc(P);
  end;
  if P^ <> '$' then
  begin
    if P^ = #0 then
    begin
      Result := False;
      Exit;
    end;
    repeat
      C := Byte(P^);
      if Char(C) in ['0'..'9'] then
        Dec(C,48)
      else
        Break;
      if (V<0) or (V>$CCCCCCCCCCCCCCC) then
      begin
        Result := False;
        Exit;
      end;
      V := V*10 + C;
      Inc(P);
    until False;
    if V < 0 then
    begin
      Result := (V=$8000000000000000) and Sign and (C=0);
      Exit;
    end;
  end else
  begin
    Inc(P);
    repeat
      C := Byte(P^);
      case Char(C) of
      '0'..'9': Dec(C,48);
      'A'..'F': Dec(C,55);
      'a'..'f': Dec(C,87);
      else
        Break;
      end;
      if PArr64(@V)^[7] >= $10 then
      begin
        Result := False;
        Exit;
      end;
      V := V shl 4;
      PLong(@V)^ := PLong(@V)^ or C;
      Inc(P);
    until False;
    if Sign and (V=$8000000000000000) then
    begin
      Result := False;
      Exit;
    end;
  end;
  if Sign then
    V := -V;
  Result := C=0;
end;

function Q_StrToFloat(const S: string; var V: Extended): Boolean;
begin
  Result := TextToFloat(PChar(S), V, fvExtended);
end;

function Q_StrToCurr(const S: string; var V: Currency): Boolean;
begin
  Result := TextToFloat(PChar(S), V, fvCurrency);
end;

procedure Q_ConvertError(const Msg: string);
begin
  raise EConvertError.Create(Msg);
end;

procedure Q_ConvertErrorFmt(const Msg, S: string);
begin
  raise EConvertError.CreateFmt(Msg, [S]);
end;

function Q_IntToStr(N: Integer): string;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EAX,EDX
        XOR     EDX,EDX
        CMP     ESI,1000
        JNL     @@x1
        CMP     ESI,$FFFFFF9C
        JNG     @@x1
        MOV     ECX,3
        JMP     @@do
@@x1:   CMP     ESI,10000000
        JNL     @@x2
        CMP     ESI,$FFF0BDC0
        JNG     @@x2
        MOV     ECX,7
        JMP     @@do
@@x2:   MOV     ECX,$0B
@@do:   CALL    System.@LStrFromPCharLen
        MOV     EAX,ESI
        MOV     ESI,[EDI]
        MOV     EDI,ESI
        TEST    EAX,EAX
        JE      @@eq
        JNS     @@ns
        CMP     EAX,$80000000
        JE      @@mm
        MOV     BYTE PTR [ESI],$2D
        INC     ESI
        NEG     EAX
@@ns:   MOV     ECX,$0A
@@lp1:  XOR     EDX,EDX
        DIV     ECX
        ADD     DL,$30
        MOV     BYTE PTR [ESI],DL
        INC     ESI
        TEST    EAX,EAX
        JNE     @@lp1
        MOV     BYTE PTR [ESI],0
        LEA     ECX,[ESI-1]
        SUB     ESI,EDI
        MOV     DWORD PTR [EDI-4],ESI
        CMP     BYTE PTR [EDI],$2D
        JE      @@ws
@@lp2:  CMP     EDI,ECX
        JAE     @@qt
        MOV     AH,BYTE PTR [EDI]
        MOV     AL,BYTE PTR [ECX]
        MOV     BYTE PTR [ECX],AH
        MOV     BYTE PTR [EDI],AL
        DEC     ECX
@@ws:   INC     EDI
        JMP     @@lp2
@@qt:   POP     EDI
        POP     ESI
        RET
@@eq:   MOV     WORD PTR [ESI],$0030
        MOV     DWORD PTR [ESI-4],1
        POP     EDI
        POP     ESI
        RET
@@mm:   MOV     DWORD PTR [ESI],$3431322D
        MOV     DWORD PTR [ESI+4],$33383437
        MOV     DWORD PTR [ESI+8],$00383436
        MOV     DWORD PTR [ESI-4],11
        POP     EDI
        POP     ESI
end;

procedure Q_IntToStrBuf(N: Integer; var S: string);
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,[EDX]
        MOV     EDI,ESI
        TEST    EAX,EAX
        JE      @@eq
        JNS     @@ns
        CMP     EAX,$80000000
        JE      @@mm
        MOV     BYTE PTR [ESI],$2D
        INC     ESI
        NEG     EAX
@@ns:   MOV     ECX,$0A
@@lp1:  XOR     EDX,EDX
        DIV     ECX
        ADD     DL,$30
        MOV     BYTE PTR [ESI],DL
        INC     ESI
        TEST    EAX,EAX
        JNE     @@lp1
        MOV     BYTE PTR [ESI],0
        LEA     ECX,[ESI-1]
        SUB     ESI,EDI
        MOV     DWORD PTR [EDI-4],ESI
        CMP     BYTE PTR [EDI],$2D
        JE      @@ws
@@lp2:  CMP     EDI,ECX
        JAE     @@qt
        MOV     AH,BYTE PTR [EDI]
        MOV     AL,BYTE PTR [ECX]
        MOV     BYTE PTR [ECX],AH
        MOV     BYTE PTR [EDI],AL
        DEC     ECX
@@ws:   INC     EDI
        JMP     @@lp2
@@qt:   POP     EDI
        POP     ESI
        RET
@@eq:   MOV     WORD PTR [ESI],$0030
        MOV     DWORD PTR [ESI-4],1
        POP     EDI
        POP     ESI
        RET
@@mm:   MOV     DWORD PTR [ESI],$3431322D
        MOV     DWORD PTR [ESI+4],$33383437
        MOV     DWORD PTR [ESI+8],$00383436
        MOV     DWORD PTR [ESI-4],11
        POP     EDI
        POP     ESI
end;

function Q_UIntToStr(N: LongWord): string;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EAX,EDX
        XOR     EDX,EDX
        CMP     ESI,1000
        JNB     @@x1
        MOV     ECX,$03
        JMP     @@do
@@x1:   CMP     ESI,10000000
        JNB     @@x2
        MOV     ECX,$07
        JMP     @@do
@@x2:   MOV     ECX,$0B
@@do:   CALL    System.@LStrFromPCharLen
        MOV     EAX,ESI
        MOV     ESI,[EDI]
        MOV     EDI,ESI
        TEST    EAX,EAX
        JE      @@eq
        MOV     ECX,$0A
@@lp1:  XOR     EDX,EDX
        DIV     ECX
        ADD     DL,$30
        MOV     BYTE PTR [ESI],DL
        INC     ESI
        TEST    EAX,EAX
        JNE     @@lp1
        MOV     BYTE PTR [ESI],0
        LEA     ECX,[ESI-1]
        SUB     ESI,EDI
        MOV     DWORD PTR [EDI-4],ESI
@@lp2:  CMP     EDI,ECX
        JAE     @@qt
        MOV     AH,BYTE PTR [EDI]
        MOV     AL,BYTE PTR [ECX]
        MOV     BYTE PTR [ECX],AH
        MOV     BYTE PTR [EDI],AL
        INC     EDI
        DEC     ECX
        JMP     @@lp2
@@qt:   POP     EDI
        POP     ESI
        RET
@@eq:   MOV     WORD PTR [ESI],$0030
        MOV     DWORD PTR [ESI-4],1
        POP     EDI
        POP     ESI
end;

procedure Q_UIntToStrBuf(N: LongWord; var S: string);
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,[EDX]
        MOV     EDI,ESI
        TEST    EAX,EAX
        JE      @@eq
        MOV     ECX,$0A
@@lp1:  XOR     EDX,EDX
        DIV     ECX
        ADD     DL,$30
        MOV     BYTE PTR [ESI],DL
        INC     ESI
        TEST    EAX,EAX
        JNE     @@lp1
        MOV     BYTE PTR [ESI],0
        LEA     ECX,[ESI-1]
        SUB     ESI,EDI
        MOV     DWORD PTR [EDI-4],ESI
@@lp2:  CMP     EDI,ECX
        JAE     @@qt
        MOV     AH,BYTE PTR [EDI]
        MOV     AL,BYTE PTR [ECX]
        MOV     BYTE PTR [ECX],AH
        MOV     BYTE PTR [EDI],AL
        INC     EDI
        DEC     ECX
        JMP     @@lp2
@@qt:   POP     EDI
        POP     ESI
        RET
@@eq:   MOV     WORD PTR [ESI],$0030
        MOV     DWORD PTR [ESI-4],1
        POP     EDI
        POP     ESI
end;

function Q_UIntToStrL(N: LongWord; Digits: Cardinal): string;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,ECX
        MOV     EBX,EDX
        MOV     EAX,ECX
        MOV     ECX,EDX
        XOR     EDX,EDX
        CALL    System.@LStrFromPCharLen
        MOV     EAX,ESI
        MOV     ESI,[EDI]
        MOV     EDI,ESI
        MOV     ECX,$0A
@@lp1:  DEC     EBX
        JS      @@lp2
        XOR     EDX,EDX
        DIV     ECX
        ADD     DL,$30
        MOV     BYTE PTR [ESI],DL
        INC     ESI
        TEST    EAX,EAX
        JNE     @@lp1
@@bl:   DEC     EBX
        JS      @@lp2
        MOV     BYTE PTR [ESI],$30
        INC     ESI
        JMP     @@bl
@@lp2:  DEC     ESI
        CMP     EDI,ESI
        JAE     @@qt
        MOV     AH,BYTE PTR [EDI]
        MOV     AL,BYTE PTR [ESI]
        MOV     BYTE PTR [ESI],AH
        MOV     BYTE PTR [EDI],AL
        INC     EDI
        JMP     @@lp2
@@qt:   POP     EBX
        POP     EDI
        POP     ESI
end;

procedure Q_UIntToStrLBuf(N: LongWord; Digits: Cardinal; var S: string);
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI,[ECX]
        MOV     EBX,EDX
        MOV     EDI,ESI
        MOV     ECX,$0A
@@lp1:  DEC     EBX
        JS      @@ob
        XOR     EDX,EDX
        DIV     ECX
        ADD     DL,$30
        MOV     BYTE PTR [ESI],DL
        INC     ESI
        TEST    EAX,EAX
        JNE     @@lp1
@@bl:   DEC     EBX
        JS      @@ob
        MOV     BYTE PTR [ESI],$30
        INC     ESI
        JMP     @@bl
@@ob:   MOV     BYTE PTR [ESI],0
        LEA     ECX,[ESI-1]
        SUB     ESI,EDI
        MOV     DWORD PTR [EDI-4],ESI
@@lp2:  CMP     EDI,ECX
        JAE     @@qt
        MOV     AH,BYTE PTR [EDI]
        MOV     AL,BYTE PTR [ECX]
        MOV     BYTE PTR [ECX],AH
        MOV     BYTE PTR [EDI],AL
        INC     EDI
        DEC     ECX
        JMP     @@lp2
@@qt:   POP     EBX
        POP     EDI
        POP     ESI
end;

function Q_IntToRoman(N: Integer): string;
const
  T_s: array[0..5] of string = ('','M','MM','MMM','MMMM','MMMMM');
  S_s: array[0..9] of string = ('','C','CC','CCC','CD','D','DC','DCC','DCCC','CM');
  D_s: array[0..9] of string = ('','X','XX','XXX','XL','L','LX','LXX','LXXX','XC');
  E_s: array[0..9] of string = ('','I','II','III','IV','V','VI','VII','VIII','IX');
var
  T,S,D: Integer;
begin
  if (N>=1) and (N<=5000) then
  begin
    T := 0;
    while N >= 1000 do
    begin
      Dec(N,1000);
      Inc(T);
    end;
    S := 0;
    if N >= 500 then
    begin
      Dec(N,500);
      S := 5;
    end;
    while N >= 100 do
    begin
      Dec(N,100);
      Inc(S);
    end;
    D := 0;
    if N >= 50 then
    begin
      Dec(N,50);
      D := 5;
    end;
    while N >= 10 do
    begin
      Dec(N,10);
      Inc(D);
    end;
    Result := T_s[T]+S_s[S]+D_s[D]+E_s[N];
  end else
    Result := '?????';
end;

function Q_RomanToInt(const S: string): Integer;
var
  I,N,PN: Integer;
begin
  N := 10000;
  Result := 0;
  for I := 1 to Length(S) do
  begin
    PN := N;
    case S[I] of
      'C': N := 100;
      'D': N := 500;
      'I': N := 1;
      'L': N := 50;
      'M': N := 1000;
      'V': N := 5;
      'X': N := 10;
      'c': N := 100;
      'd': N := 500;
      'i': N := 1;
      'l': N := 50;
      'm': N := 1000;
      'v': N := 5;
      'x': N := 10;
    else
      Q_ConvertErrorFmt('Ошибка в ходе преобразования римского числа %s в целое число.',S);
    end;
    if N <= PN then
      Inc(Result,N)
    else
      Inc(Result,N-(PN shl 1));
  end;
end;

function Q_UIntToHex(N: LongWord; Digits: Cardinal): string;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,ECX
        MOV     EBX,EDX
        MOV     EAX,ECX
        MOV     ECX,EDX
        XOR     EDX,EDX
        CALL    System.@LStrFromPCharLen
        MOV     EAX,ESI
        MOV     ESI,[EDI]
        MOV     EDI,ESI
@@lp1:  DEC     EBX
        JS      @@lp2
        MOV     DL,AL
        AND     DL,$0F
        CMP     DL,$09
        JA      @@bd
        ADD     DL,$30
        MOV     BYTE PTR [ESI],DL
        INC     ESI
        SHR     EAX,4
        JNE     @@lp1
        JMP     @@bl
@@bd:   ADD     DL,$37
        MOV     BYTE PTR [ESI],DL
        INC     ESI
        SHR     EAX,4
        JNE     @@lp1
@@bl:   DEC     EBX
        JS      @@lp2
        MOV     BYTE PTR [ESI],$30
        INC     ESI
        JMP     @@bl
@@lp2:  DEC     ESI
        CMP     EDI,ESI
        JAE     @@qt
        MOV     AH,BYTE PTR [EDI]
        MOV     AL,BYTE PTR [ESI]
        MOV     BYTE PTR [ESI],AH
        MOV     BYTE PTR [EDI],AL
        INC     EDI
        JMP     @@lp2
@@qt:   POP     EBX
        POP     EDI
        POP     ESI
end;

procedure Q_UIntToHexBuf(N: LongWord; Digits: Cardinal; var S: string);
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI,[ECX]
        MOV     EDI,ESI
@@lp1:  DEC     EDX
        JS      @@ob
        MOV     BL,AL
        AND     BL,$0F
        CMP     BL,$09
        JA      @@bd
        ADD     BL,$30
        MOV     BYTE PTR [ESI],BL
        INC     ESI
        SHR     EAX,4
        JNE     @@lp1
        JMP     @@bl
@@bd:   ADD     BL,$37
        MOV     BYTE PTR [ESI],BL
        INC     ESI
        SHR     EAX,4
        JNE     @@lp1
@@bl:   DEC     EDX
        JS      @@ob
        MOV     BYTE PTR [ESI],$30
        INC     ESI
        JMP     @@bl
@@ob:   MOV     BYTE PTR [ESI],0
        LEA     ECX,[ESI-1]
        SUB     ESI,EDI
        MOV     DWORD PTR [EDI-4],ESI
@@lp2:  CMP     EDI,ECX
        JAE     @@qt
        MOV     AH,BYTE PTR [EDI]
        MOV     AL,BYTE PTR [ECX]
        MOV     BYTE PTR [ECX],AH
        MOV     BYTE PTR [EDI],AL
        INC     EDI
        DEC     ECX
        JMP     @@lp2
@@qt:   POP     EBX
        POP     EDI
        POP     ESI
end;

function Q_HexToUInt(const S: string): LongWord;
const
  Msg: string = 'Ошибка в ходе преобразования шестнадцатеричного числа %s в целое число.';
asm
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,EAX
        TEST    EAX,EAX
        JE      @@err
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@err
        MOV     EBX,EAX
        XOR     EAX,EAX
@@lp:   MOV     DL,BYTE PTR [EBX]
        SHL     EAX,4
        SUB     DL,$30
        JB      @@err
        CMP     DL,$09
        JBE     @@ct
        SUB     DL,$11
        JB      @@err
        CMP     DL,$05
        JBE     @@pt
        SUB     DL,$20
        JB      @@err
        CMP     DL,$05
        JA      @@err
@@pt:   ADD     DL,$0A
@@ct:   OR      AL,DL
        INC     EBX
        DEC     ECX
        JNE     @@lp
        POP     EBX
        POP     ESI
        RET
@@err:  MOV     EAX,Msg
        MOV     EDX,ESI
        POP     EBX
        POP     ESI
        CALL    Q_ConvertErrorFmt
end;

function Q_UIntToOct(N: LongWord; Digits: Cardinal): string;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,ECX
        MOV     EBX,EDX
        MOV     EAX,ECX
        MOV     ECX,EDX
        XOR     EDX,EDX
        CALL    System.@LStrFromPCharLen
        MOV     EAX,ESI
        MOV     ESI,[EDI]
        MOV     EDI,ESI
@@lp1:  DEC     EBX
        JS      @@lp2
        MOV     DL,AL
        AND     DL,$07
        ADD     DL,$30
        MOV     BYTE PTR [ESI],DL
        INC     ESI
        SHR     EAX,3
        JNE     @@lp1
@@bl:   DEC     EBX
        JS      @@lp2
        MOV     BYTE PTR [ESI],$30
        INC     ESI
        JMP     @@bl
@@lp2:  DEC     ESI
        CMP     EDI,ESI
        JAE     @@qt
        MOV     AH,BYTE PTR [EDI]
        MOV     AL,BYTE PTR [ESI]
        MOV     BYTE PTR [ESI],AH
        MOV     BYTE PTR [EDI],AL
        INC     EDI
        JMP     @@lp2
@@qt:   POP     EBX
        POP     EDI
        POP     ESI
end;

procedure Q_UIntToOctBuf(N: LongWord; Digits: Cardinal; var S: string);
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI,[ECX]
        MOV     EDI,ESI
@@lp1:  DEC     EDX
        JS      @@ob
        MOV     BL,AL
        AND     BL,$07
        ADD     BL,$30
        MOV     BYTE PTR [ESI],BL
        INC     ESI
        SHR     EAX,3
        JNE     @@lp1
@@bl:   DEC     EDX
        JS      @@ob
        MOV     BYTE PTR [ESI],$30
        INC     ESI
        JMP     @@bl
@@ob:   MOV     BYTE PTR [ESI],0
        LEA     ECX,[ESI-1]
        SUB     ESI,EDI
        MOV     DWORD PTR [EDI-4],ESI
@@lp2:  CMP     EDI,ECX
        JAE     @@qt
        MOV     AH,BYTE PTR [EDI]
        MOV     AL,BYTE PTR [ECX]
        MOV     BYTE PTR [ECX],AH
        MOV     BYTE PTR [EDI],AL
        INC     EDI
        DEC     ECX
        JMP     @@lp2
@@qt:   POP     EBX
        POP     EDI
        POP     ESI
end;

function Q_OctToUInt(const S: string): LongWord;
const
  Msg: string = 'Ошибка в ходе преобразования восьмеричного числа %s в целое число.';
asm
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,EAX
        TEST    EAX,EAX
        JE      @@err
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@err
        MOV     EBX,EAX
        XOR     EAX,EAX
@@lp:   MOV     DL,BYTE PTR [EBX]
        SHL     EAX,3
        SUB     DL,$30
        JB      @@err
        CMP     DL,$07
        JA      @@err
        OR      AL,DL
        INC     EBX
        DEC     ECX
        JNE     @@lp
        POP     EBX
        POP     ESI
        RET
@@err:  MOV     EAX,Msg
        MOV     EDX,ESI
        POP     EBX
        POP     ESI
        CALL    Q_ConvertErrorFmt
end;

function Q_UIntToBin(N: LongWord; Digits: Cardinal): string;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,ECX
        MOV     EBX,EDX
        MOV     EAX,ECX
        MOV     ECX,EDX
        XOR     EDX,EDX
        CALL    System.@LStrFromPCharLen
        MOV     EAX,ESI
        MOV     ESI,[EDI]
        MOV     EDI,ESI
@@lp1:  DEC     EBX
        JS      @@lp2
        XOR     EDX,EDX
        SHR     EAX,1
        ADC     DL,$30
        MOV     BYTE PTR [ESI],DL
        INC     ESI
        JMP     @@lp1
@@lp2:  DEC     ESI
        CMP     EDI,ESI
        JAE     @@qt
        MOV     AH,BYTE PTR [EDI]
        MOV     AL,BYTE PTR [ESI]
        MOV     BYTE PTR [ESI],AH
        MOV     BYTE PTR [EDI],AL
        INC     EDI
        JMP     @@lp2
@@qt:   POP     EBX
        POP     EDI
        POP     ESI
end;

procedure Q_UIntToBinBuf(N: LongWord; Digits: Cardinal; var S: string);
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI,[ECX]
        MOV     EDI,ESI
@@lp1:  DEC     EDX
        JS      @@ob
        XOR     EBX,EBX
        SHR     EAX,1
        ADC     BL,$30
        MOV     BYTE PTR [ESI],BL
        INC     ESI
        JMP     @@lp1
@@ob:   MOV     BYTE PTR [ESI],0
        LEA     ECX,[ESI-1]
        SUB     ESI,EDI
        MOV     DWORD PTR [EDI-4],ESI
@@lp2:  CMP     EDI,ECX
        JAE     @@qt
        MOV     AH,BYTE PTR [EDI]
        MOV     AL,BYTE PTR [ECX]
        MOV     BYTE PTR [ECX],AH
        MOV     BYTE PTR [EDI],AL
        INC     EDI
        DEC     ECX
        JMP     @@lp2
@@qt:   POP     EBX
        POP     EDI
        POP     ESI
end;

function Q_BinToUInt(const S: string): LongWord;
const
  Msg: string = 'Ошибка в ходе преобразования двоичного числа %s в целое число.';
asm
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,EAX
        TEST    EAX,EAX
        JE      @@err
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@err
        MOV     EBX,EAX
        XOR     EAX,EAX
@@lp:   MOV     DL,BYTE PTR [EBX]
        SHL     EAX,1
        SUB     DL,$30
        JB      @@err
        TEST    DL,$FE
        JNE     @@err
        OR      AL,DL
        INC     EBX
        DEC     ECX
        JNE     @@lp
        POP     EBX
        POP     ESI
        RET
@@err:  MOV     EAX,Msg
        MOV     EDX,ESI
        POP     EBX
        POP     ESI
        CALL    Q_ConvertErrorFmt
end;

procedure IntMul(P: Pointer; L: Cardinal; N: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EBP
        MOV     ESI,EAX
        MOV     EBP,ECX
        MOV     ECX,EDX
        XOR     EBX,EBX
@@lp:   MOV     EAX,[ESI]
        MUL     EBP
        ADD     EAX,EBX
        MOV     EBX,EDX
        MOV     [ESI],EAX
        ADC     EBX,0
        ADD     ESI,4
        DEC     ECX
        JNE     @@lp
        POP     EBP
        POP     ESI
        POP     EBX
end;

procedure IntAdd(P: Pointer; N: Cardinal);
asm
@@lp:   ADD     [EAX],EDX
        JC      @@nx
        RET
@@nx:   ADD     EAX,4
        MOV     EDX,1
        JMP     @@lp
end;

function IntDiv(P: Pointer; L: Cardinal; N: Cardinal): Cardinal;
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,ECX
        MOV     ESI,EAX
        LEA     ECX,[EDX-1]
        XOR     EDX,EDX
@@lp:   MOV     EAX,[ESI+ECX*4]
        DIV     EBX
        MOV     [ESI+ECX*4],EAX
        DEC     ECX
        JNS     @@lp
        MOV     EAX,EDX
        POP     ESI
        POP     EBX
end;

function Q_ChangeBase(const Number: string; BaseFrom, BaseTo: Cardinal;
  DigitsInGroup: Cardinal; GroupSeparator: Char): string;
const
  Letters: string = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
type
  PDigits = ^TDigits;
  TDigits = array[1..100000000] of Byte;
  PLongs = ^TLongs;
  TLongs = array[1..25000000] of LongWord;
var
  Digits: PDigits;
  DigOut: PDigits;
  D: Double;
  L,I,U,Q: LongWord;
  P: ^Char;
  C: Char;
begin
  L := Length(Number)-Q_CharCount(Number,GroupSeparator);
  if (BaseFrom<2) or (BaseFrom>36) then
    Q_ConvertErrorFmt('Неправильное исходное основание числа: %s',
      Q_UIntToStr(BaseFrom));
  if (BaseTo<2) or (BaseTo>36) then
    Q_ConvertErrorFmt('Неправильное конечное основание числа: %s',
      Q_UIntToStr(BaseTo));
  if L > 0 then
  begin
    GetMem(Digits,L);
    Q := 1;
    for I := 1 to Length(Number) do
    begin
      C := Number[I];
      if C <> GroupSeparator then
      begin
        U := Q_StrScan(Letters,ToUpperChars[Byte(C)]);
        if (U=0) or (U>BaseFrom) then
          Q_ConvertErrorFmt('Невозможно преобразовать символ ''%s'' в функции Q_ChangeBase',C);
        Digits^[Q] := U-1;
        Inc(Q);
      end;
    end;
    D := Log2(BaseFrom)*L;
    Q := Round(D);
    if D = Int(D) then
      Dec(Q);
    U := (Q shr 5)+1;
    GetMem(P, U shl 2);
    Q_FillLong(0,P,U);
    for I := 1 to L do
    begin
      IntMul(P,U,BaseFrom);
      IntAdd(P,Digits^[I]);
    end;
    FreeMem(Digits);
    GetMem(DigOut,Round(Q/Log2(BaseTo))+1);
    DigOut^[1] := 0;
    while (U>0) and (PLongs(P)^[U]=0) do
      Dec(U);
    L := 1;
    while U > 0 do
    begin
      DigOut^[L] := IntDiv(P,U,BaseTo);
      if PLongs(P)^[U] = 0 then
        Dec(U);
      Inc(L);
    end;
    FreeMem(P);
    if L > 1 then
      Dec(L);
    if DigitsInGroup <> 0 then
    begin
      SetString(Result,nil,(L-1) div DigitsInGroup + L);
      P := Pointer(Result);
      repeat
        P^ := Letters[DigOut^[L]+1];
        Dec(L);
        Inc(P);
        if (L mod DigitsInGroup=0) and (L<>0) then
        begin
          P^ := GroupSeparator;
          Inc(P);
        end;
      until L = 0;
    end else
    begin
      SetString(Result,nil,L);
      P := Pointer(Result);
      repeat
        P^ := Letters[DigOut^[L]+1];
        Inc(P);
        Dec(L);
      until L = 0;
    end;
    FreeMem(DigOut);
  end else
    Result := '';
end;

function Q_StrToCodes(const S: string): string;
asm
        TEST    EAX,EAX
        JE      @@cl
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@cl
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EBX,ECX
        MOV     EAX,EDX
        SHL     ECX,1
        XOR     EDX,EDX
        CALL    System.@LStrFromPCharLen
        MOV     EDI,[EDI]
@@lp:   MOV     AL,BYTE PTR [ESI]
        MOV     DL,AL
        SHR     AL,4
        AND     DL,$0F
        CMP     AL,$09
        JA      @@bd1
        ADD     AL,$30
        JMP     @@nx1
@@bd1:  ADD     AL,$37
@@nx1:  MOV     BYTE PTR [EDI],AL
        INC     EDI
        CMP     DL,$09
        JA      @@bd2
        ADD     DL,$30
        JMP     @@nx2
@@bd2:  ADD     DL,$37
@@nx2:  MOV     BYTE PTR [EDI],DL
        INC     ESI
        INC     EDI
        DEC     EBX
        JNE     @@lp
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@cl:   MOV     EAX,EDX
        CALL    System.@LStrClr
end;

function Q_CodesToStr(const S: string): string;
const
  Msg: string = 'Ошибка при преобразовании строки шестнадцатеричных кодов в'+
    ' в строку символов.';
asm
        TEST    EAX,EAX
        JE      @@cl
        MOV     ECX,[EAX-4]
        SHR     ECX,1
        JC      @@err1
        JE      @@cl
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EBX,ECX
        MOV     EAX,EDX
        XOR     EDX,EDX
        CALL    System.@LStrFromPCharLen
        MOV     EDI,[EDI]
@@lp:   MOV     AL,BYTE PTR [ESI]
        MOV     DL,BYTE PTR [ESI+1]
        SUB     AL,$30
        JB      @@err0
        SUB     DL,$30
        JB      @@err0
        CMP     AL,$09
        JBE     @@ct1
        SUB     AL,$11
        JB      @@err0
        CMP     AL,$05
        JBE     @@pt1
        SUB     AL,$20
        JB      @@err0
        CMP     AL,$05
        JA      @@err0
@@pt1:  ADD     AL,$0A
@@ct1:  SHL     AL,4
        CMP     DL,$09
        JBE     @@ct2
        SUB     DL,$11
        JB      @@err0
        CMP     DL,$05
        JBE     @@pt2
        SUB     DL,$20
        JB      @@err0
        CMP     DL,$05
        JA      @@err0
@@pt2:  ADD     DL,$0A
@@ct2:  OR      AL,DL
        MOV     BYTE PTR [EDI],AL
        ADD     ESI,2
        INC     EDI
        DEC     EBX
        JNE     @@lp
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@cl:   MOV     EAX,EDX
        CALL    System.@LStrClr
        RET
@@err0: POP     EDI
        POP     ESI
        POP     EBX
@@err1: MOV     EAX,Msg
        CALL    Q_ConvertError
end;

function ModDiv10(var X: Integer): Integer;
const
  Base10: Integer = 10;
asm
        MOV     ECX,EAX
        MOV     EAX,[EAX]
        XOR     EDX,EDX
        DIV     Base10
        MOV     [ECX],EAX
        MOV     EAX,EDX
end;

function Q_NumToStr(N: Int64; var S: string; FmtFlags: LongWord): Integer;
const
  M_Ed: array [1..9] of string =
    ('один ','два ','три ','четыре ','пять ','шесть ','семь ','восемь ','девять ');
  W_Ed: array [1..9] of string =
    ('одна ','две ','три ','четыре ','пять ','шесть ','семь ','восемь ','девять ');
  S_Ed: array [1..9] of string =
    ('одно ','два ','три ','четыре ','пять ','шесть ','семь ','восемь ','девять ');
  E_Ds: array [0..9] of string =
    ('десять ','одиннадцать ','двенадцать ','тринадцать ','четырнадцать ',
     'пятнадцать ','шестнадцать ','семнадцать ','восемнадцать ','девятнадцать ');
  D_Ds: array [2..9] of string =
    ('двадцать ','тридцать ','сорок ','пятьдесят ','шестьдесят ','семьдесят ',
     'восемьдесят ','девяносто ');
  U_Hd: array [1..9] of string =
    ('сто ','двести ','триста ','четыреста ','пятьсот ','шестьсот ','семьсот ',
     'восемьсот ','девятьсот ');
  M_Tr: array[1..6,0..3] of string =
    (('тыс. ','тысяча ','тысячи ','тысяч '),
     ('млн. ','миллион ','миллиона ','миллионов '),
     ('млрд. ','миллиард ','миллиарда ','миллиардов '),
     ('трлн. ','триллион ','триллиона ','триллионов '),
     ('квадр. ','квадриллион ','квадриллиона ','квадриллионов '),
     ('квинт. ','квинтиллион ','квинтиллиона ','квинтиллионов '));
var
  V1: Int64;
  VArr: array[0..6] of Integer;
  I,E,D,H,Cnt: Integer;
begin
  Result := 3;
  if N <> 0 then
  begin
    if N < 0 then
    begin
      if N <> $8000000000000000 then
      begin
        N := -N;
        S := 'минус ';
      end else
      begin                                 { -9.223.372.036.854.775.808 }
        if FmtFlags and nsShort = 0 then
          S := 'минус девять квинтиллионов двести двадцать три квадриллиона'+
            ' триста семьдесят два триллиона тридцать шесть миллиардов'+
            ' восемьсот пятьдесят четыре миллиона семьсот семьдесят пять'+
            ' тысяч восемьсот восемь '
        else
          S := 'минус девять квинт. двести двадцать три квадр. триста'+
            ' семьдесят два трлн. тридцать шесть млрд. восемьсот пятьдесят'+
            ' четыре млн. семьсот семьдесят пять тыс. восемьсот восемь ';
        Exit;
      end;
    end else
      S := '';
    Cnt := 0;
    repeat
      V1 := N div 1000;
      VArr[Cnt] := N-(V1*1000);
      N := V1;
      Inc(Cnt);
    until V1 = 0;
    for I := Cnt-1 downto 0 do
    begin
      H := VArr[I];
      Result := 3;
      if H <> 0 then
      begin
        E := ModDiv10(H);
        D := ModDiv10(H);
        if D <> 1 then
        begin
          if E = 1 then
            Result := 1
          else if (E>=2) and (E<=4) then
            Result := 2;
          if (H<>0) and (D<>0) then
            S := S + U_Hd[H] + D_Ds[D]
          else if H <> 0 then
            S := S + U_Hd[H]
          else if D <> 0 then
            S := S + D_Ds[D];
          if E <> 0 then
            if I = 0 then
              case FmtFlags and 3 of
                1: S := S + M_Ed[E];
                2: S := S + W_Ed[E];
                3: S := S + S_Ed[E];
              else
                S := S + '#### ';
              end
            else if I = 1 then
              S := S + W_Ed[E]
            else
              S := S + M_Ed[E];
        end else
          if H = 0 then
            S := S + E_Ds[E]
          else
            S := S + U_Hd[H] + E_Ds[E];
        if I <> 0 then
        begin
          if FmtFlags and nsShort = 0 then
            S := S + M_Tr[I,Result]
          else
            S := S + M_Tr[I,0];
        end;
      end;
    end;
  end else
    S := 'ноль ';
end;

function Q_NumToRub(V: Currency; RubFormat, CopFormat: LongWord): string;
var
  V1: Int64;
  S1,S2,S3,S4: string;
  Cp,I: Integer;
  Negative: Boolean;
begin
  if V >= 0 then
    Negative := False
  else
  begin
    Negative := True;
    V := -V;
  end;
  if RubFormat <> nrNone then
  begin
    if CopFormat <> nrNone then
    begin
      V1 := Trunc(V);
      Cp := Round(Frac(V)*100);
      if V1 <> 0 then
      begin
        if RubFormat and 1 = 0 then
        begin
          if RubFormat and 2 <> 0 then
          begin
            case Q_NumToStr(V1,S1,nsMale or (RubFormat and 4)) of
              1: S2 := 'рубль ';
              2: S2 := 'рубля ';
              3: S2 := 'рублей ';
            end;
          end else
          begin
            S1 := IntToStr(V1);
            I := V1 mod 100;
            if (I<10) or (I>20) then
            begin
              case I mod 10 of
                1: S2 := ' рубль ';
                2,3,4: S2 := ' рубля ';
              else
                S2 := ' рублей ';
              end;
            end else
              S2 := ' рублей ';
          end;
        end else
        begin
          if RubFormat and 2 <> 0 then
          begin
            Q_NumToStr(V1,S1,nsMale or (RubFormat and 4));
            S2 := 'руб. ';
          end else
          begin
            S1 := IntToStr(V1);
            S2 := ' руб. ';
          end;
        end;
      end else
      begin
        S1 := '';
        S2 := '';
      end;
      if CopFormat and 1 = 0 then
      begin
        if CopFormat and 2 <> 0 then
        begin
          case Q_NumToStr(Cp,S3,nsFemale) of
            1: S4 := 'копейка';
            2: S4 := 'копейки';
            3: S4 := 'копеек';
          end;
        end else
        begin
          S3 := Q_UIntToStrL(Cp,2);
          I := Cp mod 100;
          if (I<10) or (I>20) then
          begin
            case I mod 10 of
              1: S4 := ' копейка';
              2,3,4: S4 := ' копейки';
            else
              S4 := ' копеек';
            end;
          end else
            S4 := ' копеек';
        end;
      end else
      begin
        if CopFormat and 2 <> 0 then
        begin
          Q_NumToStr(Cp,S3,nsFemale);
          S4 := 'коп.';
        end else
        begin
          S3 := Q_UIntToStrL(Cp,2);
          S4 := ' коп.';
        end;
      end;
      if not Negative then
      begin
        Result := S1+S2+S3+S4;
        Result[1] := ToUpperChars[Byte(Result[1])];
      end
      else
      begin
        Result := '('+S1+S2+S3+S4+')';
        Result[2] := ToUpperChars[Byte(Result[2])];
      end;
    end else
    begin
      V1 := Round(V);
      if V1 <> 0 then
      begin
        if RubFormat and 1 = 0 then
        begin
          if RubFormat and 2 <> 0 then
          begin
            case Q_NumToStr(V1,S1,nsMale or (RubFormat and 4)) of
              1: S2 := 'рубль';
              2: S2 := 'рубля';
              3: S2 := 'рублей';
            end;
          end else
          begin
            S1 := IntToStr(V1);
            I := V1 mod 100;
            if (I<10) or (I>20) then
            begin
              case I mod 10 of
                1: S2 := ' рубль';
                2,3,4: S2 := ' рубля';
              else
                S2 := ' рублей';
              end;
            end else
              S2 := ' рублей';
          end;
        end else
        begin
          if RubFormat and 2 <> 0 then
          begin
            Q_NumToStr(V1,S1,nsMale or (RubFormat and 4));
            S2 := 'руб.';
          end else
          begin
            S1 := IntToStr(V1);
            S2 := ' руб.';
          end;
        end;
        S1[1] := ToUpperChars[Byte(S1[1])];
        if not Negative then
          Result := S1+S2
        else
          Result := '('+S1+S2+')';
      end else
        Result := '';
    end;
  end
  else if CopFormat <> nrNone then
  begin
    V1 := Round(V*100);
    if CopFormat and 1 = 0 then
    begin
      if CopFormat and 2 <> 0 then
      begin
        case Q_NumToStr(V1,S1,nsFemale or (CopFormat and 4)) of
          1: S2 := 'копейка';
          2: S2 := 'копейки';
          3: S2 := 'копеек';
        end;
      end else
      begin
        S1 := IntToStr(V1);
        I := V1 mod 100;
        if (I<10) or (I>20) then
        begin
          case I mod 10 of
            1: S2 := ' копейка';
            2,3,4: S2 := ' копейки';
          else
            S2 := ' копеек';
          end;
        end else
          S2 := ' копеек';
      end;
    end else
    begin
      if CopFormat and 2 <> 0 then
      begin
        Q_NumToStr(V1,S1,nsFemale or (CopFormat and 4));
        S2 := 'коп.';
      end else
      begin
        S1 := IntToStr(V1);
        S2 := ' коп.';
      end;
    end;
    S1[1] := ToUpperChars[Byte(S1[1])];
    if not Negative then
      Result := S1+S2
    else
      Result := '('+S1+S2+')';
  end
  else if not Negative then
    Result := FormatFloat('0.00',V)
  else
    Result := '('+FormatFloat('0.00',V)+')';
end;

procedure Q_ZeroMem(P: Pointer; L: Cardinal);
asm
        CMP     EDX,32
        JA      @@nx
        XOR     ECX,ECX
        CALL    Q_TinyFill
        RET
@@nx:   PUSH    EDI
        MOV     ECX,EAX
        XOR     EAX,EAX
        MOV     EDI,ECX
        NEG     ECX
        AND     ECX,7
        SUB     EDX,ECX
        JMP     DWORD PTR @@bV[ECX*4]
@@bV:   DD      @@bu00, @@bu01, @@bu02, @@bu03
        DD      @@bu04, @@bu05, @@bu06, @@bu07
@@bu07: MOV     [EDI+06],AL
@@bu06: MOV     [EDI+05],AL
@@bu05: MOV     [EDI+04],AL
@@bu04: MOV     [EDI+03],AL
@@bu03: MOV     [EDI+02],AL
@@bu02: MOV     [EDI+01],AL
@@bu01: MOV     [EDI],AL
        ADD     EDI,ECX
@@bu00: MOV     ECX,EDX
        AND     EDX,3
        SHR     ECX,2
        REP     STOSD
        JMP     DWORD PTR @@tV[EDX*4]
@@tV:   DD      @@tu00, @@tu01, @@tu02, @@tu03
@@tu03: MOV     [EDI+02],AL
@@tu02: MOV     [EDI+01],AL
@@tu01: MOV     [EDI],AL
@@tu00: POP     EDI
end;

procedure Q_OnesMem(P: Pointer; L: Cardinal);
asm
        CMP     EDX,32
        JA      @@nx
        MOV     ECX,$FFFFFFFF
        CALL    Q_TinyFill
        RET
@@nx:   PUSH    EDI
        MOV     ECX,EAX
        XOR     EAX,EAX
        MOV     EDI,ECX
        NEG     ECX
        DEC     EAX
        AND     ECX,7
        SUB     EDX,ECX
        JMP     DWORD PTR @@bV[ECX*4]
@@bV:   DD      @@bu00, @@bu01, @@bu02, @@bu03
        DD      @@bu04, @@bu05, @@bu06, @@bu07
@@bu07: MOV     [EDI+06],AL
@@bu06: MOV     [EDI+05],AL
@@bu05: MOV     [EDI+04],AL
@@bu04: MOV     [EDI+03],AL
@@bu03: MOV     [EDI+02],AL
@@bu02: MOV     [EDI+01],AL
@@bu01: MOV     [EDI],AL
        ADD     EDI,ECX
@@bu00: MOV     ECX,EDX
        AND     EDX,3
        SHR     ECX,2
        REP     STOSD
        JMP     DWORD PTR @@tV[EDX*4]
@@tV:   DD      @@tu00, @@tu01, @@tu02, @@tu03
@@tu03: MOV     [EDI+02],AL
@@tu02: MOV     [EDI+01],AL
@@tu01: MOV     [EDI],AL
@@tu00: POP     EDI
end;

procedure Q_FillChar(P: Pointer; L: Cardinal; Ch: Char); overload;
asm
        PUSH    EDI
        MOV     EDI,EAX
        MOVZX   EAX,CL
        CMP     EDX,16
        JB      @@tl
        MOV     ECX,EDI
        NEG     ECX
        AND     ECX,7
        SUB     EDX,ECX
        JMP     DWORD PTR @@bV[ECX*4]
@@bV:   DD      @@bu00, @@bu01, @@bu02, @@bu03
        DD      @@bu04, @@bu05, @@bu06, @@bu07
@@bu07: MOV     [EDI+06],AL
@@bu06: MOV     [EDI+05],AL
@@bu05: MOV     [EDI+04],AL
@@bu04: MOV     [EDI+03],AL
@@bu03: MOV     [EDI+02],AL
@@bu02: MOV     [EDI+01],AL
@@bu01: MOV     [EDI],AL
        ADD     EDI,ECX
@@bu00: MOV     ECX,EAX
        SHL     EAX,8
        ADD     EAX,ECX
        MOV     ECX,EAX
        SHL     EAX,16
        ADD     EAX,ECX
        MOV     ECX,EDX
        AND     EDX,3
        SHR     ECX,2
        REP     STOSD
@@tl:   JMP     DWORD PTR @@tV[EDX*4]
@@tV:   DD      @@tu00, @@tu01, @@tu02, @@tu03
        DD      @@tu04, @@tu05, @@tu06, @@tu07
        DD      @@tu08, @@tu09, @@tu10, @@tu11
        DD      @@tu12, @@tu13, @@tu14, @@tu15
@@tu15: MOV     [EDI+14],AL
@@tu14: MOV     [EDI+13],AL
@@tu13: MOV     [EDI+12],AL
@@tu12: MOV     [EDI+11],AL
@@tu11: MOV     [EDI+10],AL
@@tu10: MOV     [EDI+09],AL
@@tu09: MOV     [EDI+08],AL
@@tu08: MOV     [EDI+07],AL
@@tu07: MOV     [EDI+06],AL
@@tu06: MOV     [EDI+05],AL
@@tu05: MOV     [EDI+04],AL
@@tu04: MOV     [EDI+03],AL
@@tu03: MOV     [EDI+02],AL
@@tu02: MOV     [EDI+01],AL
@@tu01: MOV     [EDI],AL
@@tu00: POP     EDI
end;

procedure Q_FillChar(P: Pointer; L: Cardinal; Ch: Byte); overload;
asm
        PUSH    EDI
        MOV     EDI,EAX
        MOVZX   EAX,CL
        CMP     EDX,16
        JB      @@tl
        MOV     ECX,EDI
        NEG     ECX
        AND     ECX,7
        SUB     EDX,ECX
        JMP     DWORD PTR @@bV[ECX*4]
@@bV:   DD      @@bu00, @@bu01, @@bu02, @@bu03
        DD      @@bu04, @@bu05, @@bu06, @@bu07
@@bu07: MOV     [EDI+06],AL
@@bu06: MOV     [EDI+05],AL
@@bu05: MOV     [EDI+04],AL
@@bu04: MOV     [EDI+03],AL
@@bu03: MOV     [EDI+02],AL
@@bu02: MOV     [EDI+01],AL
@@bu01: MOV     [EDI],AL
        ADD     EDI,ECX
@@bu00: MOV     ECX,EAX
        SHL     EAX,8
        ADD     EAX,ECX
        MOV     ECX,EAX
        SHL     EAX,16
        ADD     EAX,ECX
        MOV     ECX,EDX
        AND     EDX,3
        SHR     ECX,2
        REP     STOSD
@@tl:   JMP     DWORD PTR @@tV[EDX*4]
@@tV:   DD      @@tu00, @@tu01, @@tu02, @@tu03
        DD      @@tu04, @@tu05, @@tu06, @@tu07
        DD      @@tu08, @@tu09, @@tu10, @@tu11
        DD      @@tu12, @@tu13, @@tu14, @@tu15
@@tu15: MOV     [EDI+14],AL
@@tu14: MOV     [EDI+13],AL
@@tu13: MOV     [EDI+12],AL
@@tu12: MOV     [EDI+11],AL
@@tu11: MOV     [EDI+10],AL
@@tu10: MOV     [EDI+09],AL
@@tu09: MOV     [EDI+08],AL
@@tu08: MOV     [EDI+07],AL
@@tu07: MOV     [EDI+06],AL
@@tu06: MOV     [EDI+05],AL
@@tu05: MOV     [EDI+04],AL
@@tu04: MOV     [EDI+03],AL
@@tu03: MOV     [EDI+02],AL
@@tu02: MOV     [EDI+01],AL
@@tu01: MOV     [EDI],AL
@@tu00: POP     EDI
end;

procedure Q_FillLong(Value: LongWord; P: Pointer; Count: Cardinal);
asm
        CMP     ECX,32
        JBE     @@xx
        XCHG    EDI,EDX
        REP     STOSD
        MOV     EDI,EDX
        RET
@@xx:   JMP     DWORD PTR @@wV[ECX*4]
@@wV:   DD      @@w00, @@w01, @@w02, @@w03
        DD      @@w04, @@w05, @@w06, @@w07
        DD      @@w08, @@w09, @@w10, @@w11
        DD      @@w12, @@w13, @@w14, @@w15
        DD      @@w16, @@w17, @@w18, @@w19
        DD      @@w20, @@w21, @@w22, @@w23
        DD      @@w24, @@w25, @@w26, @@w27
        DD      @@w28, @@w29, @@w30, @@w31
        DD      @@w32
@@w32:  MOV     [EDX+124],EAX
@@w31:  MOV     [EDX+120],EAX
@@w30:  MOV     [EDX+116],EAX
@@w29:  MOV     [EDX+112],EAX
@@w28:  MOV     [EDX+108],EAX
@@w27:  MOV     [EDX+104],EAX
@@w26:  MOV     [EDX+100],EAX
@@w25:  MOV     [EDX+96],EAX
@@w24:  MOV     [EDX+92],EAX
@@w23:  MOV     [EDX+88],EAX
@@w22:  MOV     [EDX+84],EAX
@@w21:  MOV     [EDX+80],EAX
@@w20:  MOV     [EDX+76],EAX
@@w19:  MOV     [EDX+72],EAX
@@w18:  MOV     [EDX+68],EAX
@@w17:  MOV     [EDX+64],EAX
@@w16:  MOV     [EDX+60],EAX
@@w15:  MOV     [EDX+56],EAX
@@w14:  MOV     [EDX+52],EAX
@@w13:  MOV     [EDX+48],EAX
@@w12:  MOV     [EDX+44],EAX
@@w11:  MOV     [EDX+40],EAX
@@w10:  MOV     [EDX+36],EAX
@@w09:  MOV     [EDX+32],EAX
@@w08:  MOV     [EDX+28],EAX
@@w07:  MOV     [EDX+24],EAX
@@w06:  MOV     [EDX+20],EAX
@@w05:  MOV     [EDX+16],EAX
@@w04:  MOV     [EDX+12],EAX
@@w03:  MOV     [EDX+8],EAX
@@w02:  MOV     [EDX+4],EAX
@@w01:  MOV     [EDX],EAX
@@w00:
end;

procedure Q_TinyFill(P: Pointer; L: Cardinal; Value: LongWord);
asm
        JMP     DWORD PTR @@tV[EDX*4]
@@tV:   DD      @@tu00, @@tu01, @@tu02, @@tu03
        DD      @@tu04, @@tu05, @@tu06, @@tu07
        DD      @@tu08, @@tu09, @@tu10, @@tu11
        DD      @@tu12, @@tu13, @@tu14, @@tu15
        DD      @@tu16, @@tu17, @@tu18, @@tu19
        DD      @@tu20, @@tu21, @@tu22, @@tu23
        DD      @@tu24, @@tu25, @@tu26, @@tu27
        DD      @@tu28, @@tu29, @@tu30, @@tu31
        DD      @@tu32
@@tu00: RET
@@tu01: MOV     BYTE PTR [EAX],CL
        RET
@@tu02: MOV     WORD PTR [EAX],CX
        RET
@@tu03: MOV     WORD PTR [EAX],CX
        MOV     BYTE PTR [EAX+2],CL
        RET
@@tu04: MOV     DWORD PTR [EAX],ECX
        RET
@@tu05: MOV     DWORD PTR [EAX],ECX
        MOV     BYTE PTR [EAX+4],CL
        RET
@@tu06: MOV     DWORD PTR [EAX],ECX
        MOV     WORD PTR [EAX+4],CX
        RET
@@tu07: MOV     DWORD PTR [EAX],ECX
        MOV     WORD PTR [EAX+4],CX
        MOV     BYTE PTR [EAX+6],CL
        RET
@@tu08: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        RET
@@tu09: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     BYTE PTR [EAX+8],CL
        RET
@@tu10: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     WORD PTR [EAX+8],CX
        RET
@@tu11: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     WORD PTR [EAX+8],CX
        MOV     BYTE PTR [EAX+10],CL
        RET
@@tu12: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        RET
@@tu13: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     BYTE PTR [EAX+12],CL
        RET
@@tu14: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     WORD PTR [EAX+12],CX
        RET
@@tu15: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     WORD PTR [EAX+12],CX
        MOV     BYTE PTR [EAX+14],CL
        RET
@@tu16: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        RET
@@tu17: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        MOV     BYTE PTR [EAX+16],CL
        RET
@@tu18: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        MOV     WORD PTR [EAX+16],CX
        RET
@@tu19: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        MOV     WORD PTR [EAX+16],CX
        MOV     BYTE PTR [EAX+18],CL
        RET
@@tu20: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        MOV     DWORD PTR [EAX+16],ECX
        RET
@@tu21: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        MOV     DWORD PTR [EAX+16],ECX
        MOV     BYTE PTR [EAX+20],CL
        RET
@@tu22: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        MOV     DWORD PTR [EAX+16],ECX
        MOV     WORD PTR [EAX+20],CX
        RET
@@tu23: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        MOV     DWORD PTR [EAX+16],ECX
        MOV     WORD PTR [EAX+20],CX
        MOV     BYTE PTR [EAX+22],CL
        RET
@@tu24: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        MOV     DWORD PTR [EAX+16],ECX
        MOV     DWORD PTR [EAX+20],ECX
        RET
@@tu25: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        MOV     DWORD PTR [EAX+16],ECX
        MOV     DWORD PTR [EAX+20],ECX
        MOV     BYTE PTR [EAX+24],CL
        RET
@@tu26: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        MOV     DWORD PTR [EAX+16],ECX
        MOV     DWORD PTR [EAX+20],ECX
        MOV     WORD PTR [EAX+24],CX
        RET
@@tu27: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        MOV     DWORD PTR [EAX+16],ECX
        MOV     DWORD PTR [EAX+20],ECX
        MOV     WORD PTR [EAX+24],CX
        MOV     BYTE PTR [EAX+26],CL
        RET
@@tu28: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        MOV     DWORD PTR [EAX+16],ECX
        MOV     DWORD PTR [EAX+20],ECX
        MOV     DWORD PTR [EAX+24],ECX
        RET
@@tu29: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        MOV     DWORD PTR [EAX+16],ECX
        MOV     DWORD PTR [EAX+20],ECX
        MOV     DWORD PTR [EAX+24],ECX
        MOV     BYTE PTR [EAX+28],CL
        RET
@@tu30: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        MOV     DWORD PTR [EAX+16],ECX
        MOV     DWORD PTR [EAX+20],ECX
        MOV     DWORD PTR [EAX+24],ECX
        MOV     WORD PTR [EAX+28],CX
        RET
@@tu31: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        MOV     DWORD PTR [EAX+16],ECX
        MOV     DWORD PTR [EAX+20],ECX
        MOV     DWORD PTR [EAX+24],ECX
        MOV     WORD PTR [EAX+28],CX
        MOV     BYTE PTR [EAX+30],CL
        RET
@@tu32: MOV     DWORD PTR [EAX],ECX
        MOV     DWORD PTR [EAX+4],ECX
        MOV     DWORD PTR [EAX+8],ECX
        MOV     DWORD PTR [EAX+12],ECX
        MOV     DWORD PTR [EAX+16],ECX
        MOV     DWORD PTR [EAX+20],ECX
        MOV     DWORD PTR [EAX+24],ECX
        MOV     DWORD PTR [EAX+28],ECX
end;

procedure Q_FillRandom(P: Pointer; L: Cardinal; Seed: LongWord);
asm
        PUSH    ESI
        DEC     EDX
        JS      @@qt
        MOV     ESI,$8088405
        PUSH    EBX
@@lp:   IMUL    ECX,ESI
        INC     ECX
        MOV     EBX,ECX
        SHR     EBX,24
        MOV     BYTE PTR [EAX+EDX],BL
        DEC     EDX
        JNS     @@lp
        POP     EBX
@@qt:   POP     ESI
end;

procedure IntFill16(P: Pointer; V: LongWord);
asm
        MOV     [EAX],EDX
        MOV     [EAX+4],EDX
        MOV     [EAX+8],EDX
        MOV     [EAX+12],EDX
        MOV     [EAX+16],EDX
        MOV     [EAX+20],EDX
        MOV     [EAX+24],EDX
        MOV     [EAX+28],EDX
        MOV     [EAX+32],EDX
        MOV     [EAX+36],EDX
        MOV     [EAX+40],EDX
        MOV     [EAX+44],EDX
        MOV     [EAX+48],EDX
        MOV     [EAX+52],EDX
        MOV     [EAX+56],EDX
        MOV     [EAX+60],EDX
end;

procedure IntFill32(P: Pointer; V: LongWord);
asm
        MOV     [EAX],EDX
        MOV     [EAX+4],EDX
        MOV     [EAX+8],EDX
        MOV     [EAX+12],EDX
        MOV     [EAX+16],EDX
        MOV     [EAX+20],EDX
        MOV     [EAX+24],EDX
        MOV     [EAX+28],EDX
        MOV     [EAX+32],EDX
        MOV     [EAX+36],EDX
        MOV     [EAX+40],EDX
        MOV     [EAX+44],EDX
        MOV     [EAX+48],EDX
        MOV     [EAX+52],EDX
        MOV     [EAX+56],EDX
        MOV     [EAX+60],EDX
        MOV     [EAX+64],EDX
        MOV     [EAX+68],EDX
        MOV     [EAX+72],EDX
        MOV     [EAX+76],EDX
        MOV     [EAX+80],EDX
        MOV     [EAX+84],EDX
        MOV     [EAX+88],EDX
        MOV     [EAX+92],EDX
        MOV     [EAX+96],EDX
        MOV     [EAX+100],EDX
        MOV     [EAX+104],EDX
        MOV     [EAX+108],EDX
        MOV     [EAX+112],EDX
        MOV     [EAX+116],EDX
        MOV     [EAX+120],EDX
        MOV     [EAX+124],EDX
end;

procedure IntCopy16;
asm
        MOV     EAX,[ESI]
        MOV     [EDI],EAX
        MOV     EAX,[ESI+4]
        MOV     [EDI+4],EAX
        MOV     EAX,[ESI+8]
        MOV     [EDI+8],EAX
        MOV     EAX,[ESI+12]
        MOV     [EDI+12],EAX
        MOV     EAX,[ESI+16]
        MOV     [EDI+16],EAX
        MOV     EAX,[ESI+20]
        MOV     [EDI+20],EAX
        MOV     EAX,[ESI+24]
        MOV     [EDI+24],EAX
        MOV     EAX,[ESI+28]
        MOV     [EDI+28],EAX
        MOV     EAX,[ESI+32]
        MOV     [EDI+32],EAX
        MOV     EAX,[ESI+36]
        MOV     [EDI+36],EAX
        MOV     EAX,[ESI+40]
        MOV     [EDI+40],EAX
        MOV     EAX,[ESI+44]
        MOV     [EDI+44],EAX
        MOV     EAX,[ESI+48]
        MOV     [EDI+48],EAX
        MOV     EAX,[ESI+52]
        MOV     [EDI+52],EAX
        MOV     EAX,[ESI+56]
        MOV     [EDI+56],EAX
        MOV     EAX,[ESI+60]
        MOV     [EDI+60],EAX
end;

procedure Q_CopyMem(Source, Dest: Pointer; L: Cardinal);
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EDX
        MOV     EDX,ECX
        MOV     ESI,EAX
        TEST    EDI,3
        JNE     @@cl
        SHR     ECX,2
        AND     EDX,3
        CMP     ECX,16
        JBE     @@cw0
@@lp0:  CALL    IntCopy16
        ADD     ESI,64
        SUB     ECX,16
        ADD     EDI,64
        CMP     ECX,16
        JA      @@lp0
@@cw0:  JMP     DWORD PTR @@wV[ECX*4]
@@cl:   MOV     EAX,EDI
        MOV     EDX,3
        SUB     ECX,4
        JB      @@bc
        AND     EAX,3
        ADD     ECX,EAX
        JMP     DWORD PTR @@lV[EAX*4-4]
@@bc:   JMP     DWORD PTR @@tV[ECX*4+16]
@@lV:   DD      @@l1, @@l2, @@l3
@@l1:   AND     EDX,ECX
        MOV     AL,[ESI]
        MOV     [EDI],AL
        MOV     AL,[ESI+1]
        MOV     [EDI+1],AL
        MOV     AL,[ESI+2]
        SHR     ECX,2
        MOV     [EDI+2],AL
        ADD     ESI,3
        ADD     EDI,3
        CMP     ECX,16
        JBE     @@cw1
@@lp1:  CALL    IntCopy16
        ADD     ESI,64
        SUB     ECX,16
        ADD     EDI,64
        CMP     ECX,16
        JA      @@lp1
@@cw1:  JMP     DWORD PTR @@wV[ECX*4]
@@l2:   AND     EDX,ECX
        MOV     AL,[ESI]
        MOV     [EDI],AL
        MOV     AL,[ESI+1]
        SHR     ECX,2
        MOV     [EDI+1],AL
        ADD     ESI,2
        ADD     EDI,2
        CMP     ECX,16
        JBE     @@cw2
@@lp2:  CALL    IntCopy16
        ADD     ESI,64
        SUB     ECX,16
        ADD     EDI,64
        CMP     ECX,16
        JA      @@lp2
@@cw2:  JMP     DWORD PTR @@wV[ECX*4]
@@l3:   AND     EDX,ECX
        MOV     AL,[ESI]
        MOV     [EDI],AL
        INC     ESI
        SHR     ECX,2
        INC     EDI
        CMP     ECX,16
        JBE     @@cw3
@@lp3:  CALL    IntCopy16
        ADD     ESI,64
        SUB     ECX,16
        ADD     EDI,64
        CMP     ECX,16
        JA      @@lp3
@@cw3:  JMP     DWORD PTR @@wV[ECX*4]
@@wV:   DD      @@w0, @@w1, @@w2, @@w3
        DD      @@w4, @@w5, @@w6, @@w7
        DD      @@w8, @@w9, @@w10, @@w11
        DD      @@w12, @@w13, @@w14, @@w15
        DD      @@w16
@@w16:  MOV     EAX,[ESI+ECX*4-64]
        MOV     [EDI+ECX*4-64],EAX
@@w15:  MOV     EAX,[ESI+ECX*4-60]
        MOV     [EDI+ECX*4-60],EAX
@@w14:  MOV     EAX,[ESI+ECX*4-56]
        MOV     [EDI+ECX*4-56],EAX
@@w13:  MOV     EAX,[ESI+ECX*4-52]
        MOV     [EDI+ECX*4-52],EAX
@@w12:  MOV     EAX,[ESI+ECX*4-48]
        MOV     [EDI+ECX*4-48],EAX
@@w11:  MOV     EAX,[ESI+ECX*4-44]
        MOV     [EDI+ECX*4-44],EAX
@@w10:  MOV     EAX,[ESI+ECX*4-40]
        MOV     [EDI+ECX*4-40],EAX
@@w9:   MOV     EAX,[ESI+ECX*4-36]
        MOV     [EDI+ECX*4-36],EAX
@@w8:   MOV     EAX,[ESI+ECX*4-32]
        MOV     [EDI+ECX*4-32],EAX
@@w7:   MOV     EAX,[ESI+ECX*4-28]
        MOV     [EDI+ECX*4-28],EAX
@@w6:   MOV     EAX,[ESI+ECX*4-24]
        MOV     [EDI+ECX*4-24],EAX
@@w5:   MOV     EAX,[ESI+ECX*4-20]
        MOV     [EDI+ECX*4-20],EAX
@@w4:   MOV     EAX,[ESI+ECX*4-16]
        MOV     [EDI+ECX*4-16],EAX
@@w3:   MOV     EAX,[ESI+ECX*4-12]
        MOV     [EDI+ECX*4-12],EAX
@@w2:   MOV     EAX,[ESI+ECX*4-8]
        MOV     [EDI+ECX*4-8],EAX
@@w1:   MOV     EAX,[ESI+ECX*4-4]
        MOV     [EDI+ECX*4-4],EAX
        SHL     ECX,2
        ADD     ESI,ECX
        ADD     EDI,ECX
@@w0:   JMP     DWORD PTR @@tV[EDX*4]
@@tV:   DD      @@t0, @@t1, @@t2, @@t3
@@t3:   MOV     AL,[ESI+2]
        MOV     [EDI+2],AL
@@t2:   MOV     AL,[ESI+1]
        MOV     [EDI+1],AL
@@t1:   MOV     AL,[ESI]
        MOV     [EDI],AL
@@t0:   POP     ESI
        POP     EDI
end;

procedure Q_CopyLongs(Source, Dest: Pointer; Count: Cardinal);
asm
        CMP     ECX,16
        JBE     @@xx
        PUSH    ESI
        XCHG    EDI,EDX
        MOV     ESI,EAX
@@lp:   CALL    IntCopy16
        ADD     ESI,64
        SUB     ECX,16
        ADD     EDI,64
        CMP     ECX,16
        JA      @@lp
        MOV     EAX,ESI
        XCHG    EDI,EDX
        POP     ESI
@@xx:   JMP     DWORD PTR @@wV[ECX*4]
@@wV:   DD      @@w00, @@w01, @@w02, @@w03
        DD      @@w04, @@w05, @@w06, @@w07
        DD      @@w08, @@w09, @@w10, @@w11
        DD      @@w12, @@w13, @@w14, @@w15
        DD      @@w16
@@w16:  MOV     ECX,[EAX+60]
        MOV     [EDX+60],ECX
@@w15:  MOV     ECX,[EAX+56]
        MOV     [EDX+56],ECX
@@w14:  MOV     ECX,[EAX+52]
        MOV     [EDX+52],ECX
@@w13:  MOV     ECX,[EAX+48]
        MOV     [EDX+48],ECX
@@w12:  MOV     ECX,[EAX+44]
        MOV     [EDX+44],ECX
@@w11:  MOV     ECX,[EAX+40]
        MOV     [EDX+40],ECX
@@w10:  MOV     ECX,[EAX+36]
        MOV     [EDX+36],ECX
@@w09:  MOV     ECX,[EAX+32]
        MOV     [EDX+32],ECX
@@w08:  MOV     ECX,[EAX+28]
        MOV     [EDX+28],ECX
@@w07:  MOV     ECX,[EAX+24]
        MOV     [EDX+24],ECX
@@w06:  MOV     ECX,[EAX+20]
        MOV     [EDX+20],ECX
@@w05:  MOV     ECX,[EAX+16]
        MOV     [EDX+16],ECX
@@w04:  MOV     ECX,[EAX+12]
        MOV     [EDX+12],ECX
@@w03:  MOV     ECX,[EAX+8]
        MOV     [EDX+8],ECX
@@w02:  MOV     ECX,[EAX+4]
        MOV     [EDX+4],ECX
@@w01:  MOV     ECX,[EAX]
        MOV     [EDX],ECX
@@w00:
end;

procedure Q_TinyCopy(Source, Dest: Pointer; L: Cardinal);
asm
        JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@tu00, @@tu01, @@tu02, @@tu03
        DD      @@tu04, @@tu05, @@tu06, @@tu07
        DD      @@tu08, @@tu09, @@tu10, @@tu11
        DD      @@tu12, @@tu13, @@tu14, @@tu15
        DD      @@tu16, @@tu17, @@tu18, @@tu19
        DD      @@tu20, @@tu21, @@tu22, @@tu23
        DD      @@tu24, @@tu25, @@tu26, @@tu27
        DD      @@tu28, @@tu29, @@tu30, @@tu31
        DD      @@tu32
@@tu00: RET
@@tu01: MOV     CL,BYTE PTR [EAX]
        MOV     BYTE PTR [EDX],CL
        RET
@@tu02: MOV     CX,WORD PTR [EAX]
        MOV     WORD PTR [EDX],CX
        RET
@@tu03: MOV     CX,WORD PTR [EAX]
        MOV     WORD PTR [EDX],CX
        MOV     CL,BYTE PTR [EAX+2]
        MOV     BYTE PTR [EDX+2],CL
        RET
@@tu04: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        RET
@@tu05: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     CL,BYTE PTR [EAX+4]
        MOV     BYTE PTR [EDX+4],CL
        RET
@@tu06: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     CX,WORD PTR [EAX+4]
        MOV     WORD PTR [EDX+4],CX
        RET
@@tu07: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     CX,WORD PTR [EAX+4]
        MOV     WORD PTR [EDX+4],CX
        MOV     CL,BYTE PTR [EAX+6]
        MOV     BYTE PTR [EDX+6],CL
        RET
@@tu08: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        RET
@@tu09: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     CL,BYTE PTR [EAX+8]
        MOV     BYTE PTR [EDX+8],CL
        RET
@@tu10: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     CX,WORD PTR [EAX+8]
        MOV     WORD PTR [EDX+8],CX
        RET
@@tu11: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     CX,WORD PTR [EAX+8]
        MOV     WORD PTR [EDX+8],CX
        MOV     CL,BYTE PTR [EAX+10]
        MOV     BYTE PTR [EDX+10],CL
        RET
@@tu12: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        RET
@@tu13: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     CL,BYTE PTR [EAX+12]
        MOV     BYTE PTR [EDX+12],CL
        RET
@@tu14: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     CX,WORD PTR [EAX+12]
        MOV     WORD PTR [EDX+12],CX
        RET
@@tu15: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     CX,WORD PTR [EAX+12]
        MOV     WORD PTR [EDX+12],CX
        MOV     CL,BYTE PTR [EAX+14]
        MOV     BYTE PTR [EDX+14],CL
        RET
@@tu16: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        RET
@@tu17: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        MOV     CL,BYTE PTR [EAX+16]
        MOV     BYTE PTR [EDX+16],CL
        RET
@@tu18: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        MOV     CX,WORD PTR [EAX+16]
        MOV     WORD PTR [EDX+16],CX
        RET
@@tu19: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        MOV     CX,WORD PTR [EAX+16]
        MOV     WORD PTR [EDX+16],CX
        MOV     CL,BYTE PTR [EAX+18]
        MOV     BYTE PTR [EDX+18],CL
        RET
@@tu20: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        MOV     ECX,DWORD PTR [EAX+16]
        MOV     DWORD PTR [EDX+16],ECX
        RET
@@tu21: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        MOV     ECX,DWORD PTR [EAX+16]
        MOV     DWORD PTR [EDX+16],ECX
        MOV     CL,BYTE PTR [EAX+20]
        MOV     BYTE PTR [EDX+20],CL
        RET
@@tu22: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        MOV     ECX,DWORD PTR [EAX+16]
        MOV     DWORD PTR [EDX+16],ECX
        MOV     CX,WORD PTR [EAX+20]
        MOV     WORD PTR [EDX+20],CX
        RET
@@tu23: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        MOV     ECX,DWORD PTR [EAX+16]
        MOV     DWORD PTR [EDX+16],ECX
        MOV     CX,WORD PTR [EAX+20]
        MOV     WORD PTR [EDX+20],CX
        MOV     CL,BYTE PTR [EAX+22]
        MOV     BYTE PTR [EDX+22],CL
        RET
@@tu24: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        MOV     ECX,DWORD PTR [EAX+16]
        MOV     DWORD PTR [EDX+16],ECX
        MOV     ECX,DWORD PTR [EAX+20]
        MOV     DWORD PTR [EDX+20],ECX
        RET
@@tu25: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        MOV     ECX,DWORD PTR [EAX+16]
        MOV     DWORD PTR [EDX+16],ECX
        MOV     ECX,DWORD PTR [EAX+20]
        MOV     DWORD PTR [EDX+20],ECX
        MOV     CL,BYTE PTR [EAX+24]
        MOV     BYTE PTR [EDX+24],CL
        RET
@@tu26: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        MOV     ECX,DWORD PTR [EAX+16]
        MOV     DWORD PTR [EDX+16],ECX
        MOV     ECX,DWORD PTR [EAX+20]
        MOV     DWORD PTR [EDX+20],ECX
        MOV     CX,WORD PTR [EAX+24]
        MOV     WORD PTR [EDX+24],CX
        RET
@@tu27: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        MOV     ECX,DWORD PTR [EAX+16]
        MOV     DWORD PTR [EDX+16],ECX
        MOV     ECX,DWORD PTR [EAX+20]
        MOV     DWORD PTR [EDX+20],ECX
        MOV     CX,WORD PTR [EAX+24]
        MOV     WORD PTR [EDX+24],CX
        MOV     CL,BYTE PTR [EAX+26]
        MOV     BYTE PTR [EDX+26],CL
        RET
@@tu28: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        MOV     ECX,DWORD PTR [EAX+16]
        MOV     DWORD PTR [EDX+16],ECX
        MOV     ECX,DWORD PTR [EAX+20]
        MOV     DWORD PTR [EDX+20],ECX
        MOV     ECX,DWORD PTR [EAX+24]
        MOV     DWORD PTR [EDX+24],ECX
        RET
@@tu29: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        MOV     ECX,DWORD PTR [EAX+16]
        MOV     DWORD PTR [EDX+16],ECX
        MOV     ECX,DWORD PTR [EAX+20]
        MOV     DWORD PTR [EDX+20],ECX
        MOV     ECX,DWORD PTR [EAX+24]
        MOV     DWORD PTR [EDX+24],ECX
        MOV     CL,BYTE PTR [EAX+28]
        MOV     BYTE PTR [EDX+28],CL
        RET
@@tu30: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        MOV     ECX,DWORD PTR [EAX+16]
        MOV     DWORD PTR [EDX+16],ECX
        MOV     ECX,DWORD PTR [EAX+20]
        MOV     DWORD PTR [EDX+20],ECX
        MOV     ECX,DWORD PTR [EAX+24]
        MOV     DWORD PTR [EDX+24],ECX
        MOV     CX,WORD PTR [EAX+28]
        MOV     WORD PTR [EDX+28],CX
        RET
@@tu31: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        MOV     ECX,DWORD PTR [EAX+16]
        MOV     DWORD PTR [EDX+16],ECX
        MOV     ECX,DWORD PTR [EAX+20]
        MOV     DWORD PTR [EDX+20],ECX
        MOV     ECX,DWORD PTR [EAX+24]
        MOV     DWORD PTR [EDX+24],ECX
        MOV     CX,WORD PTR [EAX+28]
        MOV     WORD PTR [EDX+28],CX
        MOV     CL,BYTE PTR [EAX+30]
        MOV     BYTE PTR [EDX+30],CL
        RET
@@tu32: MOV     ECX,DWORD PTR [EAX]
        MOV     DWORD PTR [EDX],ECX
        MOV     ECX,DWORD PTR [EAX+4]
        MOV     DWORD PTR [EDX+4],ECX
        MOV     ECX,DWORD PTR [EAX+8]
        MOV     DWORD PTR [EDX+8],ECX
        MOV     ECX,DWORD PTR [EAX+12]
        MOV     DWORD PTR [EDX+12],ECX
        MOV     ECX,DWORD PTR [EAX+16]
        MOV     DWORD PTR [EDX+16],ECX
        MOV     ECX,DWORD PTR [EAX+20]
        MOV     DWORD PTR [EDX+20],ECX
        MOV     ECX,DWORD PTR [EAX+24]
        MOV     DWORD PTR [EDX+24],ECX
        MOV     ECX,DWORD PTR [EAX+28]
        MOV     DWORD PTR [EDX+28],ECX
end;

procedure Q_MoveMem(Source, Dest: Pointer; L: Cardinal);
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EDX
        MOV     ESI,EAX
        MOV     EAX,ECX
        MOV     EDX,ECX
        ADD     EAX,ESI
        CMP     EDI,ESI
        JBE     @@cpu
        CMP     EDI,EAX
        JB      @@cpd
@@cpu:  TEST    EDI,3
        JNZ     @@clu
        SHR     ECX,2
        AND     EDX,3
        CMP     ECX,16
        JBE     @@cwu
        REP     MOVSD
        JMP     DWORD PTR @@tuV[EDX*4]
@@clu:  MOV     EAX,EDI
        MOV     EDX,3
        SUB     ECX,4
        JB      @@bcu
        AND     EAX,3
        ADD     ECX,EAX
        JMP     DWORD PTR @@luV[EAX*4-4]
@@bcu:  JMP     DWORD PTR @@tuV[ECX*4+16]
@@cwu:  JMP     DWORD PTR @@wuV[ECX*4]
@@luV:  DD      @@lu1, @@lu2, @@lu3
@@lu1:  AND     EDX,ECX
        MOV     AL,[ESI]
        MOV     [EDI],AL
        MOV     AL,[ESI+1]
        MOV     [EDI+1],AL
        MOV     AL,[ESI+2]
        SHR     ECX,2
        MOV     [EDI+2],AL
        ADD     ESI,3
        ADD     EDI,3
        CMP     ECX,16
        JBE     @@cwu
        REP     MOVSD
        JMP     DWORD PTR @@tuV[EDX*4]
@@lu2:  AND     EDX,ECX
        MOV     AL,[ESI]
        MOV     [EDI],AL
        MOV     AL,[ESI+1]
        SHR     ECX,2
        MOV     [EDI+1],AL
        ADD     ESI,2
        ADD     EDI,2
        CMP     ECX,16
        JBE     @@cwu
        REP     MOVSD
        JMP     DWORD PTR @@tuV[EDX*4]
@@lu3:  AND     EDX,ECX
        MOV     AL,[ESI]
        MOV     [EDI],AL
        INC     ESI
        SHR     ECX,2
        INC     EDI
        CMP     ECX,16
        JBE     @@cwu
        REP     MOVSD
        JMP     DWORD PTR @@tuV[EDX*4]
@@wuV:  DD      @@wu0, @@wu1, @@wu2, @@wu3
        DD      @@wu4, @@wu5, @@wu6, @@wu7
        DD      @@wu8, @@wu9, @@wu10, @@wu11
        DD      @@wu12, @@wu13, @@wu14, @@wu15
        DD      @@wu16
@@wu16: MOV     EAX,[ESI+ECX*4-64]
        MOV     [EDI+ECX*4-64],EAX
@@wu15: MOV     EAX,[ESI+ECX*4-60]
        MOV     [EDI+ECX*4-60],EAX
@@wu14: MOV     EAX,[ESI+ECX*4-56]
        MOV     [EDI+ECX*4-56],EAX
@@wu13: MOV     EAX,[ESI+ECX*4-52]
        MOV     [EDI+ECX*4-52],EAX
@@wu12: MOV     EAX,[ESI+ECX*4-48]
        MOV     [EDI+ECX*4-48],EAX
@@wu11: MOV     EAX,[ESI+ECX*4-44]
        MOV     [EDI+ECX*4-44],EAX
@@wu10: MOV     EAX,[ESI+ECX*4-40]
        MOV     [EDI+ECX*4-40],EAX
@@wu9:  MOV     EAX,[ESI+ECX*4-36]
        MOV     [EDI+ECX*4-36],EAX
@@wu8:  MOV     EAX,[ESI+ECX*4-32]
        MOV     [EDI+ECX*4-32],EAX
@@wu7:  MOV     EAX,[ESI+ECX*4-28]
        MOV     [EDI+ECX*4-28],EAX
@@wu6:  MOV     EAX,[ESI+ECX*4-24]
        MOV     [EDI+ECX*4-24],EAX
@@wu5:  MOV     EAX,[ESI+ECX*4-20]
        MOV     [EDI+ECX*4-20],EAX
@@wu4:  MOV     EAX,[ESI+ECX*4-16]
        MOV     [EDI+ECX*4-16],EAX
@@wu3:  MOV     EAX,[ESI+ECX*4-12]
        MOV     [EDI+ECX*4-12],EAX
@@wu2:  MOV     EAX,[ESI+ECX*4-8]
        MOV     [EDI+ECX*4-8],EAX
@@wu1:  MOV     EAX,[ESI+ECX*4-4]
        MOV     [EDI+ECX*4-4],EAX
        LEA     EAX,[ECX*4]
        ADD     ESI,EAX
        ADD     EDI,EAX
@@wu0:  JMP     DWORD PTR @@tuV[EDX*4]
@@tuV:  DD      @@tu0, @@tu1, @@tu2, @@tu3
@@tu0:  POP     ESI
        POP     EDI
        RET
@@tu1:  MOV     AL,[ESI]
        MOV     [EDI],AL
        POP     ESI
        POP     EDI
        RET
@@tu2:  MOV     AL,[ESI]
        MOV     [EDI],AL
        MOV     AL,[ESI+1]
        MOV     [EDI+1],AL
        POP     ESI
        POP     EDI
        RET
@@tu3:  MOV     AL,[ESI]
        MOV     [EDI],AL
        MOV     AL,[ESI+1]
        MOV     [EDI+1],AL
        MOV     AL,[ESI+2]
        MOV     [EDI+2],AL
        POP     ESI
        POP     EDI
        RET
@@cpd:  LEA     ESI,[ESI+ECX-4]
        LEA     EDI,[EDI+ECX-4]
        TEST    EDI,3
        JNZ     @@cld
        SHR     ECX,2
        AND     EDX,3
        CMP     ECX,16
        JBE     @@cwd
        STD
        REP     MOVSD
        CLD
        JMP     DWORD PTR @@tdV[EDX*4]
@@cwd:  NEG     ECX
        JMP     DWORD PTR @@wdV[ECX*4+64]
@@cld:  MOV     EAX,EDI
        MOV     EDX,3
        CMP     ECX,4
        JB      @@bcd
        AND     EAX,3
        SUB     ECX,EAX
        JMP     DWORD PTR @@ldV[EAX*4-4]
@@bcd:  JMP     DWORD PTR @@tdV[ECX*4]
@@ldV:  DD      @@ld1, @@ld2, @@ld3
@@ld1:  MOV     AL,[ESI+3]
        AND     EDX,ECX
        MOV     [EDI+3],AL
        DEC     ESI
        SHR     ECX,2
        DEC     EDI
        CMP     ECX,16
        JBE     @@cwd
        STD
        REP     MOVSD
        CLD
        JMP     DWORD PTR @@tdV[EDX*4]
@@ld2:  MOV     AL,[ESI+3]
        AND     EDX,ECX
        MOV     [EDI+3],AL
        MOV     AL,[ESI+2]
        SHR     ECX,2
        MOV     [EDI+2],AL
        SUB     ESI,2
        SUB     EDI,2
        CMP     ECX,16
        JBE     @@cwd
        STD
        REP     MOVSD
        CLD
        JMP     DWORD PTR @@tdV[EDX*4]
@@ld3:  MOV     AL,[ESI+3]
        AND     EDX,ECX
        MOV     [EDI+3],AL
        MOV     AL,[ESI+2]
        MOV     [EDI+2],AL
        MOV     AL,[ESI+1]
        SHR     ECX,2
        MOV     [EDI+1],AL
        SUB     ESI,3
        SUB     EDI,3
        CMP     ECX,16
        JBE     @@cwd
        STD
        REP     MOVSD
        CLD
        JMP     DWORD PTR @@tdV[EDX*4]
@@wdV:  DD      @@wd16
        DD      @@wd15, @@wd14, @@wd13, @@wd12
        DD      @@wd11, @@wd10, @@wd9, @@wd8
        DD      @@wd7, @@wd6, @@wd5, @@wd4
        DD      @@wd3, @@wd2, @@wd1, @@wd0
@@wd16: MOV     EAX,[ESI+ECX*4+64]
        MOV     [EDI+ECX*4+64],EAX
@@wd15: MOV     EAX,[ESI+ECX*4+60]
        MOV     [EDI+ECX*4+60],EAX
@@wd14: MOV     EAX,[ESI+ECX*4+56]
        MOV     [EDI+ECX*4+56],EAX
@@wd13: MOV     EAX,[ESI+ECX*4+52]
        MOV     [EDI+ECX*4+52],EAX
@@wd12: MOV     EAX,[ESI+ECX*4+48]
        MOV     [EDI+ECX*4+48],EAX
@@wd11: MOV     EAX,[ESI+ECX*4+44]
        MOV     [EDI+ECX*4+44],EAX
@@wd10: MOV     EAX,[ESI+ECX*4+40]
        MOV     [EDI+ECX*4+40],EAX
@@wd9:  MOV     EAX,[ESI+ECX*4+36]
        MOV     [EDI+ECX*4+36],EAX
@@wd8:  MOV     EAX,[ESI+ECX*4+32]
        MOV     [EDI+ECX*4+32],EAX
@@wd7:  MOV     EAX,[ESI+ECX*4+28]
        MOV     [EDI+ECX*4+28],EAX
@@wd6:  MOV     EAX,[ESI+ECX*4+24]
        MOV     [EDI+ECX*4+24],EAX
@@wd5:  MOV     EAX,[ESI+ECX*4+20]
        MOV     [EDI+ECX*4+20],EAX
@@wd4:  MOV     EAX,[ESI+ECX*4+16]
        MOV     [EDI+ECX*4+16],EAX
@@wd3:  MOV     EAX,[ESI+ECX*4+12]
        MOV     [EDI+ECX*4+12],EAX
@@wd2:  MOV     EAX,[ESI+ECX*4+8]
        MOV     [EDI+ECX*4+8],EAX
@@wd1:  MOV     EAX,[ESI+ECX*4+4]
        MOV     [EDI+ECX*4+4],EAX
        LEA     EAX,[ECX*4]
        ADD     ESI,EAX
        ADD     EDI,EAX
@@wd0:  JMP     DWORD PTR @@tdV[EDX*4]
@@tdV:  DD      @@td0, @@td1, @@td2, @@td3
@@td0:  POP     ESI
        POP     EDI
        RET
@@td1:  MOV     AL,[ESI+3]
        MOV     [EDI+3],AL
        POP     ESI
        POP     EDI
        RET
@@td2:  MOV     AL,[ESI+3]
        MOV     [EDI+3],AL
        MOV     AL,[ESI+2]
        MOV     [EDI+2],AL
        POP     ESI
        POP     EDI
        RET
@@td3:  MOV     AL,[ESI+3]
        MOV     [EDI+3],AL
        MOV     AL,[ESI+2]
        MOV     [EDI+2],AL
        MOV     AL,[ESI+1]
        MOV     [EDI+1],AL
        POP     ESI
        POP     EDI
end;

procedure Q_MoveLongs(Source, Dest: Pointer; Count: Cardinal);
asm
        CMP     EDX,EAX
        JA      @@bm
        JE      @@qt
        XCHG    ESI,EAX
        XCHG    EDI,EDX
        REP     MOVSD
        MOV     ESI,EAX
        MOV     EDI,EDX
        RET
@@bm:   PUSH    ESI
        PUSH    EDI
        STD
        LEA     ESI,[EAX+ECX*4-4]
        LEA     EDI,[EDX+ECX*4-4]
        REP     MOVSD
        CLD
        POP     EDI
        POP     ESI
@@qt:
end;

procedure Q_MoveWords(Source, Dest: Pointer; Count: Cardinal);
asm
        CMP     EDX,EAX
        JA      @@bm
        JE      @@qt
        XCHG    ESI,EAX
        XCHG    EDI,EDX
        REP     MOVSW
        MOV     ESI,EAX
        MOV     EDI,EDX
        RET
@@bm:   PUSH    ESI
        PUSH    EDI
        STD
        LEA     ESI,[EAX+ECX*2-2]
        LEA     EDI,[EDX+ECX*2-2]
        REP     MOVSW
        CLD
        POP     EDI
        POP     ESI
@@qt:
end;

procedure Q_MoveBytes(Source, Dest: Pointer; L: Cardinal);
asm
        CMP     EDX,EAX
        JA      @@bm
        JE      @@qt
        XCHG    ESI,EAX
        XCHG    EDI,EDX
        REP     MOVSB
        MOV     ESI,EAX
        MOV     EDI,EDX
        RET
@@bm:   PUSH    ESI
        PUSH    EDI
        STD
        LEA     ESI,[EAX+ECX-1]
        LEA     EDI,[EDX+ECX-1]
        REP     MOVSB
        CLD
        POP     EDI
        POP     ESI
@@qt:
end;

procedure IntSwap8;
asm
        MOV     EAX,[ESI]
        MOV     EBX,[EDI]
        MOV     [EDI],EAX
        MOV     [ESI],EBX
        MOV     EAX,[ESI+4]
        MOV     EBX,[EDI+4]
        MOV     [EDI+4],EAX
        MOV     [ESI+4],EBX
        MOV     EAX,[ESI+8]
        MOV     EBX,[EDI+8]
        MOV     [EDI+8],EAX
        MOV     [ESI+8],EBX
        MOV     EAX,[ESI+12]
        MOV     EBX,[EDI+12]
        MOV     [EDI+12],EAX
        MOV     [ESI+12],EBX
        MOV     EAX,[ESI+16]
        MOV     EBX,[EDI+16]
        MOV     [EDI+16],EAX
        MOV     [ESI+16],EBX
        MOV     EAX,[ESI+20]
        MOV     EBX,[EDI+20]
        MOV     [EDI+20],EAX
        MOV     [ESI+20],EBX
        MOV     EAX,[ESI+24]
        MOV     EBX,[EDI+24]
        MOV     [EDI+24],EAX
        MOV     [ESI+24],EBX
        MOV     EAX,[ESI+28]
        MOV     EBX,[EDI+28]
        MOV     [EDI+28],EAX
        MOV     [ESI+28],EBX
end;

procedure Q_SwapMem(P1, P2: Pointer; L: Cardinal);
asm
        PUSH    EBX
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EDX
        MOV     EDX,ECX
        MOV     ESI,EAX
        TEST    EDI,3
        JNE     @@cl
        SHR     ECX,2
        AND     EDX,3
        CMP     ECX,8
        JBE     @@cw0
@@lp0:  CALL    IntSwap8
        ADD     ESI,32
        SUB     ECX,8
        ADD     EDI,32
        CMP     ECX,8
        JA      @@lp0
@@cw0:  JMP     DWORD PTR @@wV[ECX*4]
@@cl:   MOV     EAX,EDI
        MOV     EDX,3
        SUB     ECX,4
        JB      @@bc
        AND     EAX,3
        ADD     ECX,EAX
        JMP     DWORD PTR @@lV[EAX*4-4]
@@bc:   JMP     DWORD PTR @@tV[ECX*4+16]
@@lV:   DD      @@l1,@@l2,@@l3
@@l1:   AND     EDX,ECX
        MOV     AL,[ESI]
        MOV     BL,[EDI]
        MOV     [EDI],AL
        MOV     [ESI],BL
        MOV     AL,[ESI+1]
        MOV     BL,[EDI+1]
        MOV     [EDI+1],AL
        MOV     [ESI+1],BL
        MOV     AL,[ESI+2]
        MOV     BL,[EDI+2]
        MOV     [EDI+2],AL
        SHR     ECX,2
        MOV     [ESI+2],BL
        ADD     ESI,3
        ADD     EDI,3
        CMP     ECX,8
        JBE     @@cw1
@@lp1:  CALL    IntSwap8
        ADD     ESI,32
        SUB     ECX,8
        ADD     EDI,32
        CMP     ECX,8
        JA      @@lp1
@@cw1:  JMP     DWORD PTR @@wV[ECX*4]
@@l2:   AND     EDX,ECX
        MOV     AL,[ESI]
        MOV     BL,[EDI]
        MOV     [EDI],AL
        MOV     [ESI],BL
        MOV     AL,[ESI+1]
        MOV     BL,[EDI+1]
        MOV     [EDI+1],AL
        SHR     ECX,2
        MOV     [ESI+1],BL
        ADD     ESI,2
        ADD     EDI,2
        CMP     ECX,8
        JBE     @@cw2
@@lp2:  CALL    IntSwap8
        ADD     ESI,32
        SUB     ECX,8
        ADD     EDI,32
        CMP     ECX,8
        JA      @@lp2
@@cw2:  JMP     DWORD PTR @@wV[ECX*4]
@@l3:   AND     EDX,ECX
        MOV     AL,[ESI]
        MOV     BL,[EDI]
        MOV     [EDI],AL
        MOV     [ESI],BL
        INC     ESI
        SHR     ECX,2
        INC     EDI
        CMP     ECX,8
        JBE     @@cw3
@@lp3:  CALL    IntSwap8
        ADD     ESI,32
        SUB     ECX,8
        ADD     EDI,32
        CMP     ECX,8
        JA      @@lp3
@@cw3:  JMP     DWORD PTR @@wV[ECX*4]
@@wV:   DD      @@w0, @@w1, @@w2, @@w3
        DD      @@w4, @@w5, @@w6, @@w7
        DD      @@w8
@@w8:   MOV     EAX,[ESI+ECX*4-32]
        MOV     EBX,[EDI+ECX*4-32]
        MOV     [EDI+ECX*4-32],EAX
        MOV     [ESI+ECX*4-32],EBX
@@w7:   MOV     EAX,[ESI+ECX*4-28]
        MOV     EBX,[EDI+ECX*4-28]
        MOV     [EDI+ECX*4-28],EAX
        MOV     [ESI+ECX*4-28],EBX
@@w6:   MOV     EAX,[ESI+ECX*4-24]
        MOV     EBX,[EDI+ECX*4-24]
        MOV     [EDI+ECX*4-24],EAX
        MOV     [ESI+ECX*4-24],EBX
@@w5:   MOV     EAX,[ESI+ECX*4-20]
        MOV     EBX,[EDI+ECX*4-20]
        MOV     [EDI+ECX*4-20],EAX
        MOV     [ESI+ECX*4-20],EBX
@@w4:   MOV     EAX,[ESI+ECX*4-16]
        MOV     EBX,[EDI+ECX*4-16]
        MOV     [EDI+ECX*4-16],EAX
        MOV     [ESI+ECX*4-16],EBX
@@w3:   MOV     EAX,[ESI+ECX*4-12]
        MOV     EBX,[EDI+ECX*4-12]
        MOV     [EDI+ECX*4-12],EAX
        MOV     [ESI+ECX*4-12],EBX
@@w2:   MOV     EAX,[ESI+ECX*4-8]
        MOV     EBX,[EDI+ECX*4-8]
        MOV     [EDI+ECX*4-8],EAX
        MOV     [ESI+ECX*4-8],EBX
@@w1:   MOV     EAX,[ESI+ECX*4-4]
        MOV     EBX,[EDI+ECX*4-4]
        MOV     [EDI+ECX*4-4],EAX
        MOV     [ESI+ECX*4-4],EBX
        SHL     ECX,2
        ADD     ESI,ECX
        ADD     EDI,ECX
@@w0:   JMP     DWORD PTR @@tV[EDX*4]
@@tV:   DD      @@t0,@@t1,@@t2,@@t3
@@t3:   MOV     AL,[ESI+2]
        MOV     BL,[EDI+2]
        MOV     [EDI+2],AL
        MOV     [ESI+2],BL
@@t2:   MOV     AL,[ESI+1]
        MOV     BL,[EDI+1]
        MOV     [EDI+1],AL
        MOV     [ESI+1],BL
@@t1:   MOV     AL,[ESI]
        MOV     BL,[EDI]
        MOV     [EDI],AL
        MOV     [ESI],BL
@@t0:   POP     ESI
        POP     EDI
        POP     EBX
end;

procedure Q_SwapLongs(P1, P2: Pointer; Count: Cardinal);
asm
        PUSH    EBX
        CMP     ECX,8
        JBE     @@xx
        PUSH    ESI
@@lp:   MOV     EBX,[EDX]
        MOV     ESI,[EAX]
        MOV     [EAX],EBX
        MOV     [EDX],ESI
        MOV     EBX,[EDX+4]
        MOV     ESI,[EAX+4]
        MOV     [EAX+4],EBX
        MOV     [EDX+4],ESI
        MOV     EBX,[EDX+8]
        MOV     ESI,[EAX+8]
        MOV     [EAX+8],EBX
        MOV     [EDX+8],ESI
        MOV     EBX,[EDX+12]
        MOV     ESI,[EAX+12]
        MOV     [EAX+12],EBX
        MOV     [EDX+12],ESI
        MOV     EBX,[EDX+16]
        MOV     ESI,[EAX+16]
        MOV     [EAX+16],EBX
        MOV     [EDX+16],ESI
        MOV     EBX,[EDX+20]
        MOV     ESI,[EAX+20]
        MOV     [EAX+20],EBX
        MOV     [EDX+20],ESI
        MOV     EBX,[EDX+24]
        MOV     ESI,[EAX+24]
        MOV     [EAX+24],EBX
        MOV     [EDX+24],ESI
        MOV     EBX,[EDX+28]
        MOV     ESI,[EAX+28]
        MOV     [EAX+28],EBX
        MOV     [EDX+28],ESI
        ADD     EAX,32
        ADD     EDX,32
        SUB     ECX,8
        JA      @@lp
        POP     ESI
@@xx:   JMP     DWORD PTR @@wV[ECX*4+32]
@@wV:   DD      @@w00,@@w01,@@w02,@@w03
        DD      @@w04,@@w05,@@w06,@@w07
        DD      @@w08
@@w08:  MOV     ECX,[EAX+28]
        MOV     EBX,[EDX+28]
        MOV     [EDX+28],ECX
        MOV     [EAX+28],EBX
@@w07:  MOV     ECX,[EAX+24]
        MOV     EBX,[EDX+24]
        MOV     [EDX+24],ECX
        MOV     [EAX+24],EBX
@@w06:  MOV     ECX,[EAX+20]
        MOV     EBX,[EDX+20]
        MOV     [EDX+20],ECX
        MOV     [EAX+20],EBX
@@w05:  MOV     ECX,[EAX+16]
        MOV     EBX,[EDX+16]
        MOV     [EDX+16],ECX
        MOV     [EAX+16],EBX
@@w04:  MOV     ECX,[EAX+12]
        MOV     EBX,[EDX+12]
        MOV     [EDX+12],ECX
        MOV     [EAX+12],EBX
@@w03:  MOV     ECX,[EAX+8]
        MOV     EBX,[EDX+8]
        MOV     [EDX+8],ECX
        MOV     [EAX+8],EBX
@@w02:  MOV     ECX,[EAX+4]
        MOV     EBX,[EDX+4]
        MOV     [EDX+4],ECX
        MOV     [EAX+4],EBX
@@w01:  MOV     ECX,[EAX]
        MOV     EBX,[EDX]
        MOV     [EDX],ECX
        MOV     [EAX],EBX
@@w00:  POP     EBX
end;

function Q_CompareMem(P1, P2: Pointer; L: Cardinal): Boolean;
asm
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,ECX
        SHR     ECX,4
        JE      @@nx
@@lp:   MOV     EBX,[EAX]
        CMP     EBX,[EDX]
        JNE     @@zq0
        MOV     EBX,[EAX+4]
        CMP     EBX,[EDX+4]
        JNE     @@zq0
        MOV     EBX,[EAX+8]
        CMP     EBX,[EDX+8]
        JNE     @@zq0
        MOV     EBX,[EAX+12]
        CMP     EBX,[EDX+12]
        JNE     @@zq0
        ADD     EAX,16
        ADD     EDX,16
        DEC     ECX
        JNE     @@lp
@@nx:   AND     ESI,$F
        JMP     DWORD PTR @@tV[ESI*4]
@@tV:   DD      @@eq,@@t1,@@t2,@@t3
        DD      @@t4,@@t5,@@t6,@@t7
        DD      @@t8,@@t9,@@t10,@@t11
        DD      @@t12,@@t13,@@t14,@@t15
@@zq0:  POP     EBX
        POP     ESI
        XOR     EAX,EAX
        RET
@@t15:  MOV     BL,BYTE PTR [EAX+14]
        XOR     BL,BYTE PTR [EDX+14]
        JNE     @@zq1
@@t14:  MOV     BL,BYTE PTR [EAX+13]
        XOR     BL,BYTE PTR [EDX+13]
        JNE     @@zq1
@@t13:  MOV     BL,BYTE PTR [EAX+12]
        XOR     BL,BYTE PTR [EDX+12]
        JNE     @@zq1
@@t12:  MOV     EBX,[EAX+8]
        CMP     EBX,[EDX+8]
        JNE     @@zq1
        MOV     EBX,[EAX+4]
        CMP     EBX,[EDX+4]
        JNE     @@zq1
        MOV     EBX,[EAX]
        CMP     EBX,[EDX]
        JNE     @@zq1
@@eq:   POP     EBX
        POP     ESI
        MOV     EAX,1
        RET
@@t11:  MOV     BL,BYTE PTR [EAX+10]
        XOR     BL,BYTE PTR [EDX+10]
        JNE     @@zq1
@@t10:  MOV     BL,BYTE PTR [EAX+9]
        XOR     BL,BYTE PTR [EDX+9]
        JNE     @@zq1
@@t9:   MOV     BL,BYTE PTR [EAX+8]
        XOR     BL,BYTE PTR [EDX+8]
        JNE     @@zq1
@@t8:   MOV     EBX,[EAX+4]
        CMP     EBX,[EDX+4]
        JNE     @@zq1
        MOV     EBX,[EAX]
        CMP     EBX,[EDX]
        JNE     @@zq1
        POP     EBX
        POP     ESI
        MOV     EAX,1
        RET
@@zq1:  POP     EBX
        POP     ESI
        XOR     EAX,EAX
        RET
@@t7:   MOV     BL,BYTE PTR [EAX+6]
        XOR     BL,BYTE PTR [EDX+6]
        JNE     @@zq2
@@t6:   MOV     BL,BYTE PTR [EAX+5]
        XOR     BL,BYTE PTR [EDX+5]
        JNE     @@zq2
@@t5:   MOV     BL,BYTE PTR [EAX+4]
        XOR     BL,BYTE PTR [EDX+4]
        JNE     @@zq2
@@t4:   MOV     EBX,[EAX]
        CMP     EBX,[EDX]
        JNE     @@zq2
        POP     EBX
        POP     ESI
        MOV     EAX,1
        RET
@@t3:   MOV     BL,BYTE PTR [EAX+2]
        XOR     BL,BYTE PTR [EDX+2]
        JNE     @@zq2
@@t2:   MOV     BL,BYTE PTR [EAX+1]
        XOR     BL,BYTE PTR [EDX+1]
        JNE     @@zq2
@@t1:   MOV     BL,BYTE PTR [EAX]
        XOR     BL,BYTE PTR [EDX]
        JNE     @@zq2
        POP     EBX
        POP     ESI
        MOV     EAX,1
        RET
@@zq2:  POP     EBX
        POP     ESI
        XOR     EAX,EAX
end;

function Q_CompLongs(P1, P2: Pointer; Count: Cardinal): Boolean;
asm
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,ECX
        AND     ECX,$7
        SHR     ESI,3
        JE      @@nx
@@lp1:  MOV     EBX,[EAX]
        CMP     EBX,[EDX]
        JNE     @@zq1
        MOV     EBX,[EAX+4]
        CMP     EBX,[EDX+4]
        JNE     @@zq1
        MOV     EBX,[EAX+8]
        CMP     EBX,[EDX+8]
        JNE     @@zq1
        MOV     EBX,[EAX+12]
        CMP     EBX,[EDX+12]
        JNE     @@zq1
        MOV     EBX,[EAX+16]
        CMP     EBX,[EDX+16]
        JNE     @@zq1
        MOV     EBX,[EAX+20]
        CMP     EBX,[EDX+20]
        JNE     @@zq1
        MOV     EBX,[EAX+24]
        CMP     EBX,[EDX+24]
        JNE     @@zq1
        MOV     EBX,[EAX+28]
        CMP     EBX,[EDX+28]
        JNE     @@zq1
        ADD     EAX,32
        ADD     EDX,32
        DEC     ESI
        JNE     @@lp1
@@nx:   JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@tu0, @@tu1, @@tu2, @@tu3
        DD      @@tu4, @@tu5, @@tu6, @@tu7
@@zq1:  POP     EBX
        POP     ESI
        XOR     EAX,EAX
        RET
@@tu7:  MOV     EBX,[EAX+24]
        CMP     EBX,[EDX+24]
        JNE     @@zq2
@@tu6:  MOV     EBX,[EAX+20]
        CMP     EBX,[EDX+20]
        JNE     @@zq2
@@tu5:  MOV     EBX,[EAX+16]
        CMP     EBX,[EDX+16]
        JNE     @@zq2
@@tu4:  MOV     EBX,[EAX+12]
        CMP     EBX,[EDX+12]
        JNE     @@zq2
@@tu3:  MOV     EBX,[EAX+8]
        CMP     EBX,[EDX+8]
        JNE     @@zq2
@@tu2:  MOV     EBX,[EAX+4]
        CMP     EBX,[EDX+4]
        JNE     @@zq2
@@tu1:  MOV     EBX,[EAX]
        CMP     EBX,[EDX]
        JNE     @@zq2
@@tu0:  POP     EBX
        POP     ESI
        MOV     EAX,1
        RET
@@zq2:  POP     EBX
        POP     ESI
        XOR     EAX,EAX
end;

function Q_CompMemS(P1, P2: Pointer; L: Cardinal): Integer;
asm
        PUSH    EBX
        CMP     EAX,EDX
        JE      @@zq
@@lp:   DEC     ECX
        JS      @@zq
        MOV     BL,BYTE PTR [EAX]
        MOV     BH,BYTE PTR [EDX]
        CMP     BL,BH
        JNE     @@ne
        DEC     ECX
        JS      @@zq
        MOV     BL,BYTE PTR [EAX+1]
        MOV     BH,BYTE PTR [EDX+1]
        CMP     BL,BH
        JNE     @@ne
        DEC     ECX
        JS      @@zq
        MOV     BL,BYTE PTR [EAX+2]
        MOV     BH,BYTE PTR [EDX+2]
        CMP     BL,BH
        JNE     @@ne
        DEC     ECX
        JS      @@zq
        MOV     BL,BYTE PTR [EAX+3]
        MOV     BH,BYTE PTR [EDX+3]
        ADD     EAX,4
        ADD     EDX,4
        CMP     BL,BH
        JE      @@lp
@@ne:   MOVZX   EAX,BL
        MOVZX   EDX,BH
        SUB     EAX,EDX
        POP     EBX
        RET
@@zq:   XOR     EAX,EAX
        POP     EBX
end;

function Q_ScanInteger(N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        TEST    ECX,ECX
        JE      @@m1
        PUSH    EDI
        MOV     EDI,EDX
        REPNE   SCASD
        JNE     @@m2
        MOV     EAX,EDI
        SUB     EAX,EDX
        POP     EDI
        SHR     EAX,2
        DEC     EAX
        RET
@@m2:   POP     EDI
@@m1:   MOV     EAX,$FFFFFFFF
end;

function Q_ScanLongWord(N: LongWord; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        TEST    ECX,ECX
        JE      @@m1
        PUSH    EDI
        MOV     EDI,EDX
        REPNE   SCASD
        JNE     @@m2
        MOV     EAX,EDI
        SUB     EAX,EDX
        POP     EDI
        SHR     EAX,2
        DEC     EAX
        RET
@@m2:   POP     EDI
@@m1:   MOV     EAX,$FFFFFFFF
end;

function Q_ScanPointer(P: Pointer; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        TEST    ECX,ECX
        JE      @@m1
        PUSH    EDI
        MOV     EDI,EDX
        REPNE   SCASD
        JNE     @@m2
        MOV     EAX,EDI
        SUB     EAX,EDX
        POP     EDI
        SHR     EAX,2
        DEC     EAX
        RET
@@m2:   POP     EDI
@@m1:   MOV     EAX,$FFFFFFFF
end;

function Q_ScanWord(N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        TEST    ECX,ECX
        JE      @@m1
        PUSH    EDI
        MOV     EDI,EDX
        REPNE   SCASW
        JNE     @@m2
        MOV     EAX,EDI
        SUB     EAX,EDX
        POP     EDI
        SHR     EAX,1
        DEC     EAX
        RET
@@m2:   POP     EDI
@@m1:   MOV     EAX,$FFFFFFFF
end;

function Q_ScanByte(N: Integer; ArrPtr: Pointer; L: Cardinal): Integer;
asm
        TEST    ECX,ECX
        JE      @@m1
        PUSH    EDI
        MOV     EDI,EDX
        REPNE   SCASB
        JNE     @@m2
        MOV     EAX,EDI
        SUB     EAX,EDX
        POP     EDI
        DEC     EAX
        RET
@@m2:   POP     EDI
@@m1:   MOV     EAX,$FFFFFFFF
end;

function Q_ScanGE_Integer(N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        PUSH    EBX
        PUSH    EDX
        MOV     EBX,ECX
        SHR     ECX,3
        JE      @@nx
@@lp:   CMP     EAX,[EDX]
        JLE     @@qt0
        CMP     EAX,[EDX+4]
        JLE     @@qt1
        CMP     EAX,[EDX+8]
        JLE     @@qt2
        CMP     EAX,[EDX+12]
        JLE     @@qt3
        CMP     EAX,[EDX+16]
        JLE     @@qt4
        CMP     EAX,[EDX+20]
        JLE     @@qt5
        CMP     EAX,[EDX+24]
        JLE     @@qt6
        CMP     EAX,[EDX+28]
        JLE     @@qt7
        ADD     EDX,32
        DEC     ECX
        JNE     @@lp
@@nx:   AND     EBX,7
        JMP     DWORD PTR @@tV[EBX*4]
@@qt0:  POP     ECX
        SUB     EDX,ECX
        SHR     EDX,2
        POP     EBX
        MOV     EAX,EDX
        RET
@@qt1:  ADD     EDX,4
        JMP     @@qt0
@@qt2:  ADD     EDX,8
        JMP     @@qt0
@@qt3:  ADD     EDX,12
        JMP     @@qt0
@@qt4:  ADD     EDX,16
        JMP     @@qt0
@@qt5:  ADD     EDX,20
        JMP     @@qt0
@@qt6:  ADD     EDX,24
        JMP     @@qt0
@@qt7:  ADD     EDX,28
        JMP     @@qt0
@@tV:   DD      @@t0,@@t1,@@t2,@@t3
        DD      @@t4,@@t5,@@t6,@@t7
@@t7:   CMP     EAX,[EDX+EBX*4-28]
        JLE     @@xx7
@@t6:   CMP     EAX,[EDX+EBX*4-24]
        JLE     @@xx6
@@t5:   CMP     EAX,[EDX+EBX*4-20]
        JLE     @@xx5
@@t4:   CMP     EAX,[EDX+EBX*4-16]
        JLE     @@xx4
@@t3:   CMP     EAX,[EDX+EBX*4-12]
        JLE     @@xx3
@@t2:   CMP     EAX,[EDX+EBX*4-8]
        JLE     @@xx2
@@t1:   CMP     EAX,[EDX+EBX*4-4]
        JLE     @@xx1
@@t0:   MOV     EAX,$FFFFFFFF
        POP     ECX
        POP     EBX
        RET
@@xx7:  LEA     EAX,[EDX+EBX*4-28]
        JMP     @@tt
@@xx6:  LEA     EAX,[EDX+EBX*4-24]
        JMP     @@tt
@@xx5:  LEA     EAX,[EDX+EBX*4-20]
        JMP     @@tt
@@xx4:  LEA     EAX,[EDX+EBX*4-16]
        JMP     @@tt
@@xx3:  LEA     EAX,[EDX+EBX*4-12]
        JMP     @@tt
@@xx2:  LEA     EAX,[EDX+EBX*4-8]
        JMP     @@tt
@@xx1:  LEA     EAX,[EDX+EBX*4-4]
@@tt:   POP     ECX
        SUB     EAX,ECX
        POP     EBX
        SHR     EAX,2
end;

function Q_ScanGE_LongWord(N: LongWord; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        PUSH    EBX
        PUSH    EDX
        MOV     EBX,ECX
        SHR     ECX,3
        JE      @@nx
@@lp:   CMP     EAX,[EDX]
        JBE     @@qt0
        CMP     EAX,[EDX+4]
        JBE     @@qt1
        CMP     EAX,[EDX+8]
        JBE     @@qt2
        CMP     EAX,[EDX+12]
        JBE     @@qt3
        CMP     EAX,[EDX+16]
        JBE     @@qt4
        CMP     EAX,[EDX+20]
        JBE     @@qt5
        CMP     EAX,[EDX+24]
        JBE     @@qt6
        CMP     EAX,[EDX+28]
        JBE     @@qt7
        ADD     EDX,32
        DEC     ECX
        JNE     @@lp
@@nx:   AND     EBX,7
        JMP     DWORD PTR @@tV[EBX*4]
@@qt0:  POP     ECX
        SUB     EDX,ECX
        SHR     EDX,2
        POP     EBX
        MOV     EAX,EDX
        RET
@@qt1:  ADD     EDX,4
        JMP     @@qt0
@@qt2:  ADD     EDX,8
        JMP     @@qt0
@@qt3:  ADD     EDX,12
        JMP     @@qt0
@@qt4:  ADD     EDX,16
        JMP     @@qt0
@@qt5:  ADD     EDX,20
        JMP     @@qt0
@@qt6:  ADD     EDX,24
        JMP     @@qt0
@@qt7:  ADD     EDX,28
        JMP     @@qt0
@@tV:   DD      @@t0,@@t1,@@t2,@@t3
        DD      @@t4,@@t5,@@t6,@@t7
@@t7:   CMP     EAX,[EDX+EBX*4-28]
        JBE     @@xx7
@@t6:   CMP     EAX,[EDX+EBX*4-24]
        JBE     @@xx6
@@t5:   CMP     EAX,[EDX+EBX*4-20]
        JBE     @@xx5
@@t4:   CMP     EAX,[EDX+EBX*4-16]
        JBE     @@xx4
@@t3:   CMP     EAX,[EDX+EBX*4-12]
        JBE     @@xx3
@@t2:   CMP     EAX,[EDX+EBX*4-8]
        JBE     @@xx2
@@t1:   CMP     EAX,[EDX+EBX*4-4]
        JBE     @@xx1
@@t0:   MOV     EAX,$FFFFFFFF
        POP     ECX
        POP     EBX
        RET
@@xx7:  LEA     EAX,[EDX+EBX*4-28]
        JMP     @@tt
@@xx6:  LEA     EAX,[EDX+EBX*4-24]
        JMP     @@tt
@@xx5:  LEA     EAX,[EDX+EBX*4-20]
        JMP     @@tt
@@xx4:  LEA     EAX,[EDX+EBX*4-16]
        JMP     @@tt
@@xx3:  LEA     EAX,[EDX+EBX*4-12]
        JMP     @@tt
@@xx2:  LEA     EAX,[EDX+EBX*4-8]
        JMP     @@tt
@@xx1:  LEA     EAX,[EDX+EBX*4-4]
@@tt:   POP     ECX
        SUB     EAX,ECX
        POP     EBX
        SHR     EAX,2
end;

function Q_ScanGE_Word(N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        PUSH    EBX
        PUSH    EDX
        MOV     EBX,ECX
        SHR     ECX,3
        JE      @@nx
@@lp:   CMP     AX,[EDX]
        JBE     @@qt0
        CMP     AX,[EDX+2]
        JBE     @@qt1
        CMP     AX,[EDX+4]
        JBE     @@qt2
        CMP     AX,[EDX+6]
        JBE     @@qt3
        CMP     AX,[EDX+8]
        JBE     @@qt4
        CMP     AX,[EDX+10]
        JBE     @@qt5
        CMP     AX,[EDX+12]
        JBE     @@qt6
        CMP     AX,[EDX+14]
        JBE     @@qt7
        ADD     EDX,16
        DEC     ECX
        JNE     @@lp
@@nx:   AND     EBX,7
        JMP     DWORD PTR @@tV[EBX*4]
@@qt0:  POP     ECX
        SUB     EDX,ECX
        SHR     EDX,1
        POP     EBX
        MOV     EAX,EDX
        RET
@@qt1:  ADD     EDX,2
        JMP     @@qt0
@@qt2:  ADD     EDX,4
        JMP     @@qt0
@@qt3:  ADD     EDX,6
        JMP     @@qt0
@@qt4:  ADD     EDX,8
        JMP     @@qt0
@@qt5:  ADD     EDX,10
        JMP     @@qt0
@@qt6:  ADD     EDX,12
        JMP     @@qt0
@@qt7:  ADD     EDX,14
        JMP     @@qt0
@@tV:   DD      @@t0,@@t1,@@t2,@@t3
        DD      @@t4,@@t5,@@t6,@@t7
@@t7:   CMP     AX,[EDX+EBX*2-14]
        JBE     @@xx7
@@t6:   CMP     AX,[EDX+EBX*2-12]
        JBE     @@xx6
@@t5:   CMP     AX,[EDX+EBX*2-10]
        JBE     @@xx5
@@t4:   CMP     AX,[EDX+EBX*2-8]
        JBE     @@xx4
@@t3:   CMP     AX,[EDX+EBX*2-6]
        JBE     @@xx3
@@t2:   CMP     AX,[EDX+EBX*2-4]
        JBE     @@xx2
@@t1:   CMP     AX,[EDX+EBX*2-2]
        JBE     @@xx1
@@t0:   MOV     EAX,$FFFFFFFF
        POP     ECX
        POP     EBX
        RET
@@xx7:  LEA     EAX,[EDX+EBX*2-14]
        JMP     @@tt
@@xx6:  LEA     EAX,[EDX+EBX*2-12]
        JMP     @@tt
@@xx5:  LEA     EAX,[EDX+EBX*2-10]
        JMP     @@tt
@@xx4:  LEA     EAX,[EDX+EBX*2-8]
        JMP     @@tt
@@xx3:  LEA     EAX,[EDX+EBX*2-6]
        JMP     @@tt
@@xx2:  LEA     EAX,[EDX+EBX*2-4]
        JMP     @@tt
@@xx1:  LEA     EAX,[EDX+EBX*2-2]
@@tt:   POP     ECX
        SUB     EAX,ECX
        POP     EBX
        SHR     EAX,1
end;

function Q_ScanLesser_Integer(N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        PUSH    EBX
        PUSH    EDX
        MOV     EBX,ECX
        SHR     ECX,3
        JE      @@nx
@@lp:   CMP     EAX,[EDX]
        JG      @@qt0
        CMP     EAX,[EDX+4]
        JG      @@qt1
        CMP     EAX,[EDX+8]
        JG      @@qt2
        CMP     EAX,[EDX+12]
        JG      @@qt3
        CMP     EAX,[EDX+16]
        JG      @@qt4
        CMP     EAX,[EDX+20]
        JG      @@qt5
        CMP     EAX,[EDX+24]
        JG      @@qt6
        CMP     EAX,[EDX+28]
        JG      @@qt7
        ADD     EDX,32
        DEC     ECX
        JNE     @@lp
@@nx:   AND     EBX,7
        JMP     DWORD PTR @@tV[EBX*4]
@@qt0:  POP     ECX
        SUB     EDX,ECX
        SHR     EDX,2
        POP     EBX
        MOV     EAX,EDX
        RET
@@qt1:  ADD     EDX,4
        JMP     @@qt0
@@qt2:  ADD     EDX,8
        JMP     @@qt0
@@qt3:  ADD     EDX,12
        JMP     @@qt0
@@qt4:  ADD     EDX,16
        JMP     @@qt0
@@qt5:  ADD     EDX,20
        JMP     @@qt0
@@qt6:  ADD     EDX,24
        JMP     @@qt0
@@qt7:  ADD     EDX,28
        JMP     @@qt0
@@tV:   DD      @@t0,@@t1,@@t2,@@t3
        DD      @@t4,@@t5,@@t6,@@t7
@@t7:   CMP     EAX,[EDX+EBX*4-28]
        JG      @@xx7
@@t6:   CMP     EAX,[EDX+EBX*4-24]
        JG      @@xx6
@@t5:   CMP     EAX,[EDX+EBX*4-20]
        JG      @@xx5
@@t4:   CMP     EAX,[EDX+EBX*4-16]
        JG      @@xx4
@@t3:   CMP     EAX,[EDX+EBX*4-12]
        JG      @@xx3
@@t2:   CMP     EAX,[EDX+EBX*4-8]
        JG      @@xx2
@@t1:   CMP     EAX,[EDX+EBX*4-4]
        JG      @@xx1
@@t0:   MOV     EAX,$FFFFFFFF
        POP     ECX
        POP     EBX
        RET
@@xx7:  LEA     EAX,[EDX+EBX*4-28]
        JMP     @@tt
@@xx6:  LEA     EAX,[EDX+EBX*4-24]
        JMP     @@tt
@@xx5:  LEA     EAX,[EDX+EBX*4-20]
        JMP     @@tt
@@xx4:  LEA     EAX,[EDX+EBX*4-16]
        JMP     @@tt
@@xx3:  LEA     EAX,[EDX+EBX*4-12]
        JMP     @@tt
@@xx2:  LEA     EAX,[EDX+EBX*4-8]
        JMP     @@tt
@@xx1:  LEA     EAX,[EDX+EBX*4-4]
@@tt:   POP     ECX
        SUB     EAX,ECX
        POP     EBX
        SHR     EAX,2
end;

function Q_ScanLesser_LongWord(N: LongWord; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        PUSH    EBX
        PUSH    EDX
        MOV     EBX,ECX
        SHR     ECX,3
        JE      @@nx
@@lp:   CMP     EAX,[EDX]
        JA      @@qt0
        CMP     EAX,[EDX+4]
        JA      @@qt1
        CMP     EAX,[EDX+8]
        JA      @@qt2
        CMP     EAX,[EDX+12]
        JA      @@qt3
        CMP     EAX,[EDX+16]
        JA      @@qt4
        CMP     EAX,[EDX+20]
        JA      @@qt5
        CMP     EAX,[EDX+24]
        JA      @@qt6
        CMP     EAX,[EDX+28]
        JA      @@qt7
        ADD     EDX,32
        DEC     ECX
        JNE     @@lp
@@nx:   AND     EBX,7
        JMP     DWORD PTR @@tV[EBX*4]
@@qt0:  POP     ECX
        SUB     EDX,ECX
        SHR     EDX,2
        POP     EBX
        MOV     EAX,EDX
        RET
@@qt1:  ADD     EDX,4
        JMP     @@qt0
@@qt2:  ADD     EDX,8
        JMP     @@qt0
@@qt3:  ADD     EDX,12
        JMP     @@qt0
@@qt4:  ADD     EDX,16
        JMP     @@qt0
@@qt5:  ADD     EDX,20
        JMP     @@qt0
@@qt6:  ADD     EDX,24
        JMP     @@qt0
@@qt7:  ADD     EDX,28
        JMP     @@qt0
@@tV:   DD      @@t0,@@t1,@@t2,@@t3
        DD      @@t4,@@t5,@@t6,@@t7
@@t7:   CMP     EAX,[EDX+EBX*4-28]
        JA      @@xx7
@@t6:   CMP     EAX,[EDX+EBX*4-24]
        JA      @@xx6
@@t5:   CMP     EAX,[EDX+EBX*4-20]
        JA      @@xx5
@@t4:   CMP     EAX,[EDX+EBX*4-16]
        JA      @@xx4
@@t3:   CMP     EAX,[EDX+EBX*4-12]
        JA      @@xx3
@@t2:   CMP     EAX,[EDX+EBX*4-8]
        JA      @@xx2
@@t1:   CMP     EAX,[EDX+EBX*4-4]
        JA      @@xx1
@@t0:   MOV     EAX,$FFFFFFFF
        POP     ECX
        POP     EBX
        RET
@@xx7:  LEA     EAX,[EDX+EBX*4-28]
        JMP     @@tt
@@xx6:  LEA     EAX,[EDX+EBX*4-24]
        JMP     @@tt
@@xx5:  LEA     EAX,[EDX+EBX*4-20]
        JMP     @@tt
@@xx4:  LEA     EAX,[EDX+EBX*4-16]
        JMP     @@tt
@@xx3:  LEA     EAX,[EDX+EBX*4-12]
        JMP     @@tt
@@xx2:  LEA     EAX,[EDX+EBX*4-8]
        JMP     @@tt
@@xx1:  LEA     EAX,[EDX+EBX*4-4]
@@tt:   POP     ECX
        SUB     EAX,ECX
        POP     EBX
        SHR     EAX,2
end;

function Q_ScanLesser_Word(N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        PUSH    EBX
        PUSH    EDX
        MOV     EBX,ECX
        SHR     ECX,3
        JE      @@nx
@@lp:   CMP     AX,[EDX]
        JA      @@qt0
        CMP     AX,[EDX+2]
        JA      @@qt1
        CMP     AX,[EDX+4]
        JA      @@qt2
        CMP     AX,[EDX+6]
        JA      @@qt3
        CMP     AX,[EDX+8]
        JA      @@qt4
        CMP     AX,[EDX+10]
        JA      @@qt5
        CMP     AX,[EDX+12]
        JA      @@qt6
        CMP     AX,[EDX+14]
        JA      @@qt7
        ADD     EDX,16
        DEC     ECX
        JNE     @@lp
@@nx:   AND     EBX,7
        JMP     DWORD PTR @@tV[EBX*4]
@@qt0:  POP     ECX
        SUB     EDX,ECX
        SHR     EDX,1
        POP     EBX
        MOV     EAX,EDX
        RET
@@qt1:  ADD     EDX,2
        JMP     @@qt0
@@qt2:  ADD     EDX,4
        JMP     @@qt0
@@qt3:  ADD     EDX,6
        JMP     @@qt0
@@qt4:  ADD     EDX,8
        JMP     @@qt0
@@qt5:  ADD     EDX,10
        JMP     @@qt0
@@qt6:  ADD     EDX,12
        JMP     @@qt0
@@qt7:  ADD     EDX,14
        JMP     @@qt0
@@tV:   DD      @@t0,@@t1,@@t2,@@t3
        DD      @@t4,@@t5,@@t6,@@t7
@@t7:   CMP     AX,[EDX+EBX*2-14]
        JA      @@xx7
@@t6:   CMP     AX,[EDX+EBX*2-12]
        JA      @@xx6
@@t5:   CMP     AX,[EDX+EBX*2-10]
        JA      @@xx5
@@t4:   CMP     AX,[EDX+EBX*2-8]
        JA      @@xx4
@@t3:   CMP     AX,[EDX+EBX*2-6]
        JA      @@xx3
@@t2:   CMP     AX,[EDX+EBX*2-4]
        JA      @@xx2
@@t1:   CMP     AX,[EDX+EBX*2-2]
        JA      @@xx1
@@t0:   MOV     EAX,$FFFFFFFF
        POP     ECX
        POP     EBX
        RET
@@xx7:  LEA     EAX,[EDX+EBX*2-14]
        JMP     @@tt
@@xx6:  LEA     EAX,[EDX+EBX*2-12]
        JMP     @@tt
@@xx5:  LEA     EAX,[EDX+EBX*2-10]
        JMP     @@tt
@@xx4:  LEA     EAX,[EDX+EBX*2-8]
        JMP     @@tt
@@xx3:  LEA     EAX,[EDX+EBX*2-6]
        JMP     @@tt
@@xx2:  LEA     EAX,[EDX+EBX*2-4]
        JMP     @@tt
@@xx1:  LEA     EAX,[EDX+EBX*2-2]
@@tt:   POP     ECX
        SUB     EAX,ECX
        POP     EBX
        SHR     EAX,1
end;

function Q_CountInteger(N: Integer; ArrPtr: Pointer; Count: Cardinal): Cardinal;
asm
        DEC     ECX
        JS      @@qt
        PUSH    EBX
        MOV     EBX,EAX
        XOR     EAX,EAX
@@lp:   CMP     EBX,DWORD PTR [EDX+ECX*4]
        JE      @@fn
@@nx:   DEC     ECX
        JNS     @@lp
        POP     EBX
        RET
@@fn:   INC     EAX
        JMP     @@nx
@@qt:   XOR     EAX,EAX
end;

function Q_CountLongWord(N: LongWord; ArrPtr: Pointer; Count: Cardinal): Cardinal;
asm
        DEC     ECX
        JS      @@qt
        PUSH    EBX
        MOV     EBX,EAX
        XOR     EAX,EAX
@@lp:   CMP     EBX,DWORD PTR [EDX+ECX*4]
        JE      @@fn
@@nx:   DEC     ECX
        JNS     @@lp
        POP     EBX
        RET
@@fn:   INC     EAX
        JMP     @@nx
@@qt:   XOR     EAX,EAX
end;

function Q_CountPointer(P: Pointer; ArrPtr: Pointer; Count: Cardinal): Cardinal;
asm
        DEC     ECX
        JS      @@qt
        PUSH    EBX
        MOV     EBX,EAX
        XOR     EAX,EAX
@@lp:   CMP     EBX,DWORD PTR [EDX+ECX*4]
        JE      @@fn
@@nx:   DEC     ECX
        JNS     @@lp
        POP     EBX
        RET
@@fn:   INC     EAX
        JMP     @@nx
@@qt:   XOR     EAX,EAX
end;

function Q_CountWord(N: Integer; ArrPtr: Pointer; Count: Cardinal): Cardinal;
asm
        DEC     ECX
        JS      @@qt
        PUSH    EBX
        MOV     EBX,EAX
        XOR     EAX,EAX
@@lp:   CMP     BX,WORD PTR [EDX+ECX*2]
        JE      @@fn
@@nx:   DEC     ECX
        JNS     @@lp
        POP     EBX
        RET
@@fn:   INC     EAX
        JMP     @@nx
@@qt:   XOR     EAX,EAX
end;

function Q_CountByte(N: Integer; ArrPtr: Pointer; L: Cardinal): Cardinal;
asm
        DEC     ECX
        JS      @@qt
        PUSH    EBX
        MOV     EBX,EAX
        XOR     EAX,EAX
@@lp:   CMP     BL,BYTE PTR [EDX+ECX]
        JE      @@fn
@@nx:   DEC     ECX
        JNS     @@lp
        POP     EBX
        RET
@@fn:   INC     EAX
        JMP     @@nx
@@qt:   XOR     EAX,EAX
end;

procedure Q_ReverseLongArr(P: Pointer; Count: Cardinal);
asm
        PUSH    EDI
        LEA     EDI,[EAX+EDX*4-4]
@@lp:   CMP     EAX,EDI
        JGE     @@qt
        MOV     ECX,DWORD PTR [EAX]
        MOV     EDX,DWORD PTR [EDI]
        MOV     DWORD PTR [EDI],ECX
        MOV     DWORD PTR [EAX],EDX
        ADD     EAX,4
        SUB     EDI,4
        JMP     @@lp
@@qt:   POP     EDI
end;

procedure Q_ReverseWordArr(P: Pointer; Count: Cardinal);
asm
        PUSH    EDI
        LEA     EDI,[EAX+EDX*2-2]
@@lp:   CMP     EAX,EDI
        JGE     @@qt
        MOV     CX,WORD PTR [EAX]
        MOV     DX,WORD PTR [EDI]
        MOV     WORD PTR [EDI],CX
        MOV     WORD PTR [EAX],DX
        ADD     EAX,2
        SUB     EDI,2
        JMP     @@lp
@@qt:   POP     EDI
end;

procedure Q_ReverseByteArr(P: Pointer; L: Cardinal);
asm
        LEA     ECX,[EAX+EDX-1]
@@lp:   CMP     EAX,ECX
        JGE     @@qt
        MOV     DH,BYTE PTR [EAX]
        MOV     DL,BYTE PTR [ECX]
        MOV     BYTE PTR [ECX],DH
        MOV     BYTE PTR [EAX],DL
        INC     EAX
        DEC     ECX
        JMP     @@lp
@@qt:
end;

function Q_BSwap(D: LongWord): LongWord;
asm
        BSWAP   EAX
end;

procedure Q_BSwapLongs(P: Pointer; Count: Cardinal);
asm
        PUSH    EBX
        MOV     ECX,EDX
        SHR     EDX,3
        JE      @@nx
@@lp:   MOV     EBX,[EAX]
        BSWAP   EBX
        MOV     [EAX],EBX
        MOV     EBX,[EAX+4]
        BSWAP   EBX
        MOV     [EAX+4],EBX
        MOV     EBX,[EAX+8]
        BSWAP   EBX
        MOV     [EAX+8],EBX
        MOV     EBX,[EAX+12]
        BSWAP   EBX
        MOV     [EAX+12],EBX
        MOV     EBX,[EAX+16]
        BSWAP   EBX
        MOV     [EAX+16],EBX
        MOV     EBX,[EAX+20]
        BSWAP   EBX
        MOV     [EAX+20],EBX
        MOV     EBX,[EAX+24]
        BSWAP   EBX
        MOV     [EAX+24],EBX
        MOV     EBX,[EAX+28]
        BSWAP   EBX
        MOV     [EAX+28],EBX
        ADD     EAX,32
        DEC     EDX
        JNE     @@lp
@@nx:   AND     ECX,7
        JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@tu0, @@tu1, @@tu2, @@tu3
        DD      @@tu4, @@tu5, @@tu6, @@tu7
@@tu7:  MOV     EBX,[EAX+24]
        BSWAP   EBX
        MOV     [EAX+24],EBX
@@tu6:  MOV     EBX,[EAX+20]
        BSWAP   EBX
        MOV     [EAX+20],EBX
@@tu5:  MOV     EBX,[EAX+16]
        BSWAP   EBX
        MOV     [EAX+16],EBX
@@tu4:  MOV     EBX,[EAX+12]
        BSWAP   EBX
        MOV     [EAX+12],EBX
@@tu3:  MOV     EBX,[EAX+8]
        BSWAP   EBX
        MOV     [EAX+8],EBX
@@tu2:  MOV     EBX,[EAX+4]
        BSWAP   EBX
        MOV     [EAX+4],EBX
@@tu1:  MOV     EBX,[EAX]
        BSWAP   EBX
        MOV     [EAX],EBX
@@tu0:  POP     EBX
end;

procedure Q_Exchange(var A,B);
asm
        PUSH    EBX
        MOV     ECX,[EDX]
        MOV     EBX,[EAX]
        MOV     [EAX],ECX
        MOV     [EDX],EBX
        POP     EBX
end;

procedure Q_ExchangeWords(var A,B);
asm
        PUSH    EBX
        MOV     CX,[EDX]
        MOV     BX,[EAX]
        MOV     [EAX],CX
        MOV     [EDX],BX
        POP     EBX
end;

procedure Q_ExchangeBytes(var A,B);
asm
        MOV     CL,[EDX]
        MOV     CH,[EAX]
        MOV     [EAX],CL
        MOV     [EDX],CH
end;

function Q_RemValue_Integer(N: Integer; ArrPtr: Pointer; Count: Cardinal): Cardinal;
asm
        PUSH    EDI
	PUSH	EBX
	PUSH	EDX
        DEC     ECX
        JS      @@qt
@@lp1:  CMP     EAX,[EDX]
        JE      @@nx
        ADD     EDX,4
        DEC     ECX
        JNS     @@lp1
        JMP     @@qt
@@nx:   LEA     EDI,[EDX+4]
        DEC     ECX
        JS      @@qt
@@lp2:  CMP     EAX,[EDI]
        JE      @@sk
        MOV     EBX,[EDI]
        MOV     [EDX],EBX
        ADD     EDX,4
@@sk:   ADD     EDI,4
        DEC     ECX
        JNS     @@lp2
@@qt:   POP	EBX
	SUB	EDX,EBX
	SHR	EDX,2
	MOV	EAX,EDX
	POP	EBX
        POP     EDI
end;

function Q_RemValue_LongWord(N: LongWord; ArrPtr: Pointer; Count: Cardinal): Cardinal;
asm
        PUSH    EDI
	PUSH	EBX
	PUSH	EDX
        DEC     ECX
        JS      @@qt
@@lp1:  CMP     EAX,[EDX]
        JE      @@nx
        ADD     EDX,4
        DEC     ECX
        JNS     @@lp1
        JMP     @@qt
@@nx:   LEA     EDI,[EDX+4]
        DEC     ECX
        JS      @@qt
@@lp2:  CMP     EAX,[EDI]
        JE      @@sk
        MOV     EBX,[EDI]
        MOV     [EDX],EBX
        ADD     EDX,4
@@sk:   ADD     EDI,4
        DEC     ECX
        JNS     @@lp2
@@qt:   POP	EBX
	SUB	EDX,EBX
	SHR	EDX,2
	MOV	EAX,EDX
	POP	EBX
        POP     EDI
end;

function Q_RemValue_Pointer(P: Pointer; ArrPtr: Pointer; Count: Cardinal): Cardinal;
asm
        PUSH    EDI
	PUSH	EBX
	PUSH	EDX
        DEC     ECX
        JS      @@qt
@@lp1:  CMP     EAX,[EDX]
        JE      @@nx
        ADD     EDX,4
        DEC     ECX
        JNS     @@lp1
        JMP     @@qt
@@nx:   LEA     EDI,[EDX+4]
        DEC     ECX
        JS      @@qt
@@lp2:  CMP     EAX,[EDI]
        JE      @@sk
        MOV     EBX,[EDI]
        MOV     [EDX],EBX
        ADD     EDX,4
@@sk:   ADD     EDI,4
        DEC     ECX
        JNS     @@lp2
@@qt:   POP	EBX
	SUB	EDX,EBX
	SHR	EDX,2
	MOV	EAX,EDX
	POP	EBX
        POP     EDI
end;

function Q_RemValue_Word(N: Integer; ArrPtr: Pointer; Count: Cardinal): Cardinal;
asm
        PUSH    EDI
	PUSH	EBX
	PUSH	EDX
        DEC     ECX
        JS      @@qt
@@lp1:  CMP     AX,[EDX]
        JE      @@nx
        ADD     EDX,2
        DEC     ECX
        JNS     @@lp1
        JMP     @@qt
@@nx:   LEA     EDI,[EDX+2]
        DEC     ECX
        JS      @@qt
@@lp2:  CMP     AX,[EDI]
        JE      @@sk
        MOV     BX,[EDI]
        MOV     [EDX],BX
        ADD     EDX,2
@@sk:   ADD     EDI,2
        DEC     ECX
        JNS     @@lp2
@@qt:   POP	EBX
	SUB	EDX,EBX
	SHR	EDX,1
	MOV	EAX,EDX
	POP	EBX
        POP     EDI
end;

function Q_RemValue_Byte(N: Integer; ArrPtr: Pointer; L: Cardinal): Cardinal;
asm
        PUSH    EDI
	PUSH	EBX
	PUSH	EDX
        DEC     ECX
        JS      @@qt
@@lp1:  CMP     AL,[EDX]
        JE      @@nx
        INC     EDX
        DEC     ECX
        JNS     @@lp1
        JMP     @@qt
@@nx:   LEA     EDI,[EDX+1]
        DEC     ECX
        JS      @@qt
@@lp2:  CMP     AL,[EDI]
        JE      @@sk
        MOV     BL,[EDI]
        MOV     [EDX],BL
        INC     EDX
@@sk:   INC     EDI
        DEC     ECX
        JNS     @@lp2
@@qt:   POP	EBX
	SUB	EDX,EBX
	MOV	EAX,EDX
	POP	EBX
        POP     EDI
end;

function Q_RemDuplicates_Int32(ArrPtr: Pointer; Count: Cardinal): Cardinal;
asm
        PUSH    EDI
	PUSH	EAX
        DEC     EDX
        JS      @@qt
        MOV     ECX,[EAX]
        ADD     EAX,4
        DEC     EDX
        JS      @@qt
@@lp1:  MOV     EDI,[EAX]
        CMP     ECX,EDI
        JE      @@nx
        MOV     ECX,EDI
        ADD     EAX,4
        DEC     EDX
        JNS     @@lp1
        JMP     @@qt
@@nx:   LEA     EDI,[EAX+4]
        DEC     EDX
        JS      @@qt
@@lp2:  CMP     ECX,[EDI]
        JE      @@sk
        MOV     ECX,[EDI]
        MOV     [EAX],ECX
        ADD     EAX,4
@@sk:   ADD     EDI,4
        DEC     EDX
        JNS     @@lp2
@@qt:   POP	ECX
	SUB	EAX,ECX
	SHR	EAX,2
        POP     EDI
end;

function Q_RemDuplicates_Int16(ArrPtr: Pointer; Count: Cardinal): Cardinal;
asm
        PUSH    EDI
	PUSH	EAX
        DEC     EDX
        JS      @@qt
        MOV     CX,[EAX]
        ADD     EAX,2
        DEC     EDX
        JS      @@qt
@@lp1:  MOV     DI,[EAX]
        CMP     CX,DI
        JE      @@nx
        MOV     CX,DI
        ADD     EAX,2
        DEC     EDX
        JNS     @@lp1
        JMP     @@qt
@@nx:   LEA     EDI,[EAX+2]
        DEC     EDX
        JS      @@qt
@@lp2:  CMP     CX,[EDI]
        JE      @@sk
        MOV     CX,[EDI]
        MOV     [EAX],CX
        ADD     EAX,2
@@sk:   ADD     EDI,2
        DEC     EDX
        JNS     @@lp2
@@qt:   POP	ECX
	SUB	EAX,ECX
	SHR	EAX,1
        POP     EDI
end;

function Q_RemDuplicates_Byte(ArrPtr: Pointer; L: Cardinal): Cardinal;
asm
        PUSH    EBX
	PUSH	EAX
        DEC     EDX
        JS      @@qt
        MOV     CL,[EAX]
        INC     EAX
        DEC     EDX
        JS      @@qt
@@lp1:  MOV	BL,[EAX]
	CMP     CL,BL
        JE      @@nx
        MOV     CL,BL
        INC     EAX
        DEC     EDX
        JNS     @@lp1
        JMP     @@qt
@@nx:   LEA     EBX,[EAX+1]
        DEC     EDX
        JS      @@qt
@@lp2:  CMP     CL,[EBX]
        JE      @@sk
        MOV     CL,[EBX]
        MOV     [EAX],CL
        INC     EAX
@@sk:   INC     EBX
        DEC     EDX
        JNS     @@lp2
@@qt:   POP	ECX
	SUB     EAX,ECX
        POP     EBX
end;

function Q_ANDSetInPlace_Int32(ArrPtr: Pointer; Count: Cardinal): Cardinal;
asm
        PUSH    ESI
	PUSH	EAX
        DEC     EDX
        JS      @@qt
        MOV     ECX,[EAX]
        LEA     ESI,[EAX+4]
        DEC     EDX
        JS      @@qt
@@lp1:  CMP     ECX,[ESI]
        JE      @@nx
        MOV     ECX,[ESI]
        ADD     ESI,4
        DEC     EDX
        JNS     @@lp1
        JMP     @@qt
@@nx:   MOV     [EAX],ECX
        ADD     ESI,4
        ADD     EAX,4
        DEC     EDX
        JS      @@qt
@@lp2:  CMP     ECX,[ESI]
        JE      @@sk
@@xx:   MOV     ECX,[ESI]
        ADD     ESI,4
        DEC     EDX
        JS      @@qt
        CMP     ECX,[ESI]
        JNE     @@xx
        MOV     [EAX],ECX
        ADD     ESI,4
        ADD     EAX,4
        DEC     EDX
        JNS     @@lp2
        JMP     @@qt
@@sk:   ADD     ESI,4
        DEC     EDX
        JNS     @@lp2
@@qt:   POP	ECX
	SUB	EAX,ECX
	SHR	EAX,2
        POP     ESI
end;

function Q_ANDSetInPlace_Int16(ArrPtr: Pointer; Count: Cardinal): Cardinal;
asm
        PUSH    ESI
	PUSH	EAX
        DEC     EDX
        JS      @@qt
        MOV     CX,[EAX]
        LEA     ESI,[EAX+2]
        DEC     EDX
        JS      @@qt
@@lp1:  CMP     CX,[ESI]
        JE      @@nx
        MOV     CX,[ESI]
        ADD     ESI,2
        DEC     EDX
        JNS     @@lp1
        JMP     @@qt
@@nx:   MOV     [EAX],CX
        ADD     ESI,2
        ADD     EAX,2
        DEC     EDX
        JS      @@qt
@@lp2:  CMP     CX,[ESI]
        JE      @@sk
@@xx:   MOV     CX,[ESI]
        ADD     ESI,2
        DEC     EDX
        JS      @@qt
        CMP     CX,[ESI]
        JNE     @@xx
        MOV     [EAX],CX
        ADD     ESI,2
        ADD     EAX,2
        DEC     EDX
        JNS     @@lp2
        JMP     @@qt
@@sk:   ADD     ESI,2
        DEC     EDX
        JNS     @@lp2
@@qt:   POP	ECX
	SUB	EAX,ECX
	SHR	EAX,1
        POP     ESI
end;

function Q_XORSetInPlace_Int32(ArrPtr: Pointer; Count: Cardinal): Cardinal;
asm
        PUSH    EDI
        PUSH    ESI
	PUSH	EAX
        DEC     EDX
        JS      @@qt
        MOV     ECX,[EAX]
        ADD     EAX,4
        DEC     EDX
        JS      @@qt
@@lp1:  MOV     EDI,[EAX]
        CMP     ECX,EDI
        JE      @@nx
        MOV     ECX,EDI
        ADD     EAX,4
        DEC     EDX
        JNS     @@lp1
        JMP     @@qt
@@nx:   LEA     EDI,[EAX-4]
        LEA     ESI,[EAX+4]
        MOV     EAX,EDI
        DEC     EDX
        JS      @@qt
@@lp2:  CMP     ECX,[ESI]
        JE      @@sk
        MOV     EDI,EAX
        MOV     ECX,[ESI]
        MOV     [EAX],ECX
        ADD     EAX,4
        ADD     ESI,4
        DEC     EDX
        JNS     @@lp2
        JMP     @@qt
@@sk:   MOV     EAX,EDI
        ADD     ESI,4
        DEC     EDX
        JNS     @@lp2
@@qt:   POP	ECX
	SUB	EAX,ECX
	SHR	EAX,2
        POP     ESI
        POP     EDI
end;

function Q_XORSetInPlace_Int16(ArrPtr: Pointer; Count: Cardinal): Cardinal;
asm
        PUSH    EDI
        PUSH    ESI
	PUSH	EAX
        DEC     EDX
        JS      @@qt
        MOV     CX,[EAX]
        ADD     EAX,2
        DEC     EDX
        JS      @@qt
@@lp1:  MOV     DI,[EAX]
        CMP     CX,DI
        JE      @@nx
        MOV     CX,DI
        ADD     EAX,2
        DEC     EDX
        JNS     @@lp1
        JMP     @@qt
@@nx:   LEA     EDI,[EAX-2]
        LEA     ESI,[EAX+2]
        MOV     EAX,EDI
        DEC     EDX
        JS      @@qt
@@lp2:  CMP     CX,[ESI]
        JE      @@sk
        MOV     EDI,EAX
        MOV     CX,[ESI]
        MOV     [EAX],CX
        ADD     EAX,2
        ADD     ESI,2
        DEC     EDX
        JNS     @@lp2
        JMP     @@qt
@@sk:   MOV     EAX,EDI
        ADD     ESI,2
        DEC     EDX
        JNS     @@lp2
@@qt:   POP	ECX
	SUB	EAX,ECX
	SHR	EAX,1
        POP     ESI
        POP     EDI
end;

procedure IntSortIntegers(ArrPtr: Pointer; Count: Cardinal);
asm
        PUSH    EDI
        MOV     EDI,EDX
        PUSH    EBX
@@lp0:  LEA     ECX,[EAX+EDI]
        SHR     ECX,1
        AND     ECX,$FFFFFFFC
        MOV     EBX,EAX
        MOV     ESI,[ECX]
        MOV     EDX,EDI
@@lp1:  CMP     ESI,[EBX]
        JNG     @@lp2
        ADD     EBX,4
        CMP     ESI,[EBX]
        JNG     @@lp2
        ADD     EBX,4
        CMP     ESI,[EBX]
        JNG     @@lp2
        ADD     EBX,4
        CMP     ESI,[EBX]
        JNG     @@lp2
        ADD     EBX,4
        JMP     @@lp1
@@lp2:  CMP     ESI,[EDX]
        JNL     @@nxA
        SUB     EDX,4
        CMP     ESI,[EDX]
        JNL     @@nxA
        SUB     EDX,4
        CMP     ESI,[EDX]
        JNL     @@nxA
        SUB     EDX,4
        CMP     ESI,[EDX]
        JNL     @@nxA
        SUB     EDX,4
        JMP     @@lp2
@@nxA:  CMP     EBX,EDX
        JG      @@nxB
        MOV     ECX,[EBX]
        MOV     EBP,[EDX]
        MOV     [EDX],ECX
        MOV     [EBX],EBP
        ADD     EBX,4
        SUB     EDX,4
        CMP     EBX,EDX
        JNG     @@lp1
@@nxB:  CMP     EAX,EDX
        JNL     @@nxC
        CALL    IntSortIntegers
@@nxC:  MOV     EAX,EBX
        CMP     EBX,EDI
        JL      @@lp0
        POP     EBX
        POP     EDI
end;

procedure Q_Sort_Integer(ArrPtr: Pointer; Count: Cardinal);
asm
        TEST    EDX,$FFFFFFFE
        JE      @@qt
        PUSH    EBP
        PUSH    ESI
        LEA     EDX,[EAX+EDX*4-4]
        CALL    IntSortIntegers
        POP     ESI
        POP     EBP
@@qt:
end;

procedure IntSortLongWords(ArrPtr: Pointer; Count: Cardinal);
asm
        PUSH    EDI
        MOV     EDI,EDX
        PUSH    EBX
@@lp0:  LEA     ECX,[EAX+EDI]
        SHR     ECX,1
        AND     ECX,$FFFFFFFC
        MOV     EBX,EAX
        MOV     ESI,[ECX]
        MOV     EDX,EDI
@@lp1:  CMP     ESI,[EBX]
        JNA     @@lp2
        ADD     EBX,4
        CMP     ESI,[EBX]
        JNA     @@lp2
        ADD     EBX,4
        CMP     ESI,[EBX]
        JNA     @@lp2
        ADD     EBX,4
        CMP     ESI,[EBX]
        JNA     @@lp2
        ADD     EBX,4
        JMP     @@lp1
@@lp2:  CMP     ESI,[EDX]
        JNB     @@nxA
        SUB     EDX,4
        CMP     ESI,[EDX]
        JNB     @@nxA
        SUB     EDX,4
        CMP     ESI,[EDX]
        JNB     @@nxA
        SUB     EDX,4
        CMP     ESI,[EDX]
        JNB     @@nxA
        SUB     EDX,4
        JMP     @@lp2
@@nxA:  CMP     EBX,EDX
        JG      @@nxB
        MOV     ECX,[EBX]
        MOV     EBP,[EDX]
        MOV     [EDX],ECX
        MOV     [EBX],EBP
        ADD     EBX,4
        SUB     EDX,4
        CMP     EBX,EDX
        JNG     @@lp1
@@nxB:  CMP     EAX,EDX
        JNL     @@nxC
        CALL    IntSortLongWords
@@nxC:  MOV     EAX,EBX
        CMP     EBX,EDI
        JL      @@lp0
        POP     EBX
        POP     EDI
end;

procedure Q_Sort_LongWord(ArrPtr: Pointer; Count: Cardinal);
asm
        TEST    EDX,$FFFFFFFE
        JE      @@qt
        PUSH    EBP
        PUSH    ESI
        LEA     EDX,[EAX+EDX*4-4]
        CALL    IntSortLongWords
        POP     ESI
        POP     EBP
@@qt:
end;

procedure IntSortWords(ArrPtr: Pointer; Count: Cardinal);
asm
        PUSH    EDI
        MOV     EDI,EDX
        PUSH    EBX
@@lp0:  LEA     ECX,[EAX+EDI]
        SHR     ECX,1
        AND     ECX,$FFFFFFFE
        MOV     EBX,EAX
        MOV     SI,[ECX]
        MOV     EDX,EDI
@@lp1:  CMP     SI,[EBX]
        JNA     @@lp2
        ADD     EBX,2
        CMP     SI,[EBX]
        JNA     @@lp2
        ADD     EBX,2
        CMP     SI,[EBX]
        JNA     @@lp2
        ADD     EBX,2
        CMP     SI,[EBX]
        JNA     @@lp2
        ADD     EBX,2
        JMP     @@lp1
@@lp2:  CMP     SI,[EDX]
        JNB     @@nxA
        SUB     EDX,2
        CMP     SI,[EDX]
        JNB     @@nxA
        SUB     EDX,2
        CMP     SI,[EDX]
        JNB     @@nxA
        SUB     EDX,2
        CMP     SI,[EDX]
        JNB     @@nxA
        SUB     EDX,2
        JMP     @@lp2
@@nxA:  CMP     EBX,EDX
        JG      @@nxB
        MOV     CX,[EBX]
        MOV     BP,[EDX]
        MOV     [EDX],CX
        MOV     [EBX],BP
        ADD     EBX,2
        SUB     EDX,2
        CMP     EBX,EDX
        JNG     @@lp1
@@nxB:  CMP     EAX,EDX
        JNL     @@nxC
        CALL    IntSortWords
@@nxC:  MOV     EAX,EBX
        CMP     EBX,EDI
        JL      @@lp0
        POP     EBX
        POP     EDI
end;

procedure Q_Sort_Word(ArrPtr: Pointer; Count: Cardinal);
asm
        TEST    EDX,$FFFFFFFE
        JE      @@qt
        PUSH    EBP
        PUSH    ESI
        LEA     EDX,[EAX+EDX*2-2]
        CALL    IntSortWords
        POP     ESI
        POP     EBP
@@qt:
end;

function Q_SearchUnique_Integer(N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        DEC     ECX
        JS      @@zq
        PUSH    ESI
        PUSH    EBX
        XOR     ESI,ESI
@@lp:   LEA     EBX,[ECX+ESI]
        SHR     EBX,1
        CMP     EAX,[EDX+EBX*4]
        JL      @@mm
        JE      @@qt
        LEA     ESI,[EBX+1]
        CMP     ECX,ESI
        JGE     @@lp
        JMP     @@nx
@@mm:   LEA     ECX,[EBX-1]
        CMP     ESI,ECX
        JLE     @@lp
@@nx:   POP     EBX
        POP     ESI
@@zq:   MOV     EAX,$FFFFFFFF
        RET
@@qt:   MOV     EAX,EBX
        POP     EBX
        POP     ESI
end;

function Q_SearchUnique_LongWord(N: LongWord; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        DEC     ECX
        JS      @@zq
        PUSH    ESI
        PUSH    EBX
        XOR     ESI,ESI
@@lp:   LEA     EBX,[ECX+ESI]
        SHR     EBX,1
        CMP     EAX,[EDX+EBX*4]
        JB      @@mm
        JE      @@qt
        LEA     ESI,[EBX+1]
        CMP     ECX,ESI
        JGE     @@lp
        JMP     @@nx
@@mm:   LEA     ECX,[EBX-1]
        CMP     ESI,ECX
        JLE     @@lp
@@nx:   POP     EBX
        POP     ESI
@@zq:   MOV     EAX,$FFFFFFFF
        RET
@@qt:   MOV     EAX,EBX
        POP     EBX
        POP     ESI
end;

function Q_SearchUnique_Word(N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        DEC     ECX
        JS      @@zq
        PUSH    ESI
        PUSH    EBX
        XOR     ESI,ESI
@@lp:   LEA     EBX,[ECX+ESI]
        SHR     EBX,1
        CMP     AX,[EDX+EBX*2]
        JB      @@mm
        JE      @@qt
        LEA     ESI,[EBX+1]
        CMP     ECX,ESI
        JGE     @@lp
        JMP     @@nx
@@mm:   LEA     ECX,[EBX-1]
        CMP     ESI,ECX
        JLE     @@lp
@@nx:   POP     EBX
        POP     ESI
@@zq:   MOV     EAX,$FFFFFFFF
        RET
@@qt:   MOV     EAX,EBX
        POP     EBX
        POP     ESI
end;

function Q_SearchFirst_Integer(N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,$FFFFFFFF
        XOR     ESI,ESI
        PUSH    EBX
        DEC     ECX
        JS      @@qt
@@lp:   LEA     EBX,[ECX+ESI]
        SHR     EBX,1
        CMP     EAX,[EDX+EBX*4]
        JG      @@aa
        JE      @@ee
        LEA     ECX,[EBX-1]
        CMP     ESI,ECX
        JLE     @@lp
        JMP     @@qt
@@aa:   LEA     ESI,[EBX+1]
        CMP     ECX,ESI
        JGE     @@lp
        JMP     @@qt
@@ee:   MOV     EDI,EBX
        LEA     ECX,[EBX-1]
        CMP     ESI,ECX
        JLE     @@lp
@@qt:   POP     EBX
        MOV     EAX,EDI
        POP     ESI
        POP     EDI
end;

function Q_SearchFirst_LongWord(N: LongWord; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,$FFFFFFFF
        XOR     ESI,ESI
        PUSH    EBX
        DEC     ECX
        JS      @@qt
@@lp:   LEA     EBX,[ECX+ESI]
        SHR     EBX,1
        CMP     EAX,[EDX+EBX*4]
        JA      @@aa
        JE      @@ee
        LEA     ECX,[EBX-1]
        CMP     ESI,ECX
        JLE     @@lp
        JMP     @@qt
@@aa:   LEA     ESI,[EBX+1]
        CMP     ECX,ESI
        JGE     @@lp
        JMP     @@qt
@@ee:   MOV     EDI,EBX
        LEA     ECX,[EBX-1]
        CMP     ESI,ECX
        JLE     @@lp
@@qt:   POP     EBX
        MOV     EAX,EDI
        POP     ESI
        POP     EDI
end;

function Q_SearchFirst_Word(N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,$FFFFFFFF
        XOR     ESI,ESI
        PUSH    EBX
        DEC     ECX
        JS      @@qt
@@lp:   LEA     EBX,[ECX+ESI]
        SHR     EBX,1
        CMP     AX,[EDX+EBX*2]
        JA      @@aa
        JE      @@ee
        LEA     ECX,[EBX-1]
        CMP     ESI,ECX
        JLE     @@lp
        JMP     @@qt
@@aa:   LEA     ESI,[EBX+1]
        CMP     ECX,ESI
        JGE     @@lp
        JMP     @@qt
@@ee:   MOV     EDI,EBX
        LEA     ECX,[EBX-1]
        CMP     ESI,ECX
        JLE     @@lp
@@qt:   POP     EBX
        MOV     EAX,EDI
        POP     ESI
        POP     EDI
end;

function Q_SearchFirstGE_Integer(N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,$FFFFFFFF
        XOR     ESI,ESI
        PUSH    EBX
        DEC     ECX
        JS      @@qt
@@lp:   LEA     EBX,[ECX+ESI]
        SHR     EBX,1
        CMP     EAX,[EDX+EBX*4]
        JG      @@aa
        MOV     EDI,EBX
        LEA     ECX,[EBX-1]
        CMP     ESI,ECX
        JLE     @@lp
        JMP     @@qt
@@aa:   LEA     ESI,[EBX+1]
        CMP     ECX,ESI
        JGE     @@lp
@@qt:   POP     EBX
        MOV     EAX,EDI
        POP     ESI
        POP     EDI
end;

function Q_SearchFirstGE_LongWord(N: LongWord; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,$FFFFFFFF
        XOR     ESI,ESI
        PUSH    EBX
        DEC     ECX
        JS      @@qt
@@lp:   LEA     EBX,[ECX+ESI]
        SHR     EBX,1
        CMP     EAX,[EDX+EBX*4]
        JA      @@aa
        MOV     EDI,EBX
        LEA     ECX,[EBX-1]
        CMP     ESI,ECX
        JLE     @@lp
        JMP     @@qt
@@aa:   LEA     ESI,[EBX+1]
        CMP     ECX,ESI
        JGE     @@lp
@@qt:   POP     EBX
        MOV     EAX,EDI
        POP     ESI
        POP     EDI
end;

function Q_SearchFirstGE_Word(N: Integer; ArrPtr: Pointer; Count: Cardinal): Integer;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,$FFFFFFFF
        XOR     ESI,ESI
        PUSH    EBX
        DEC     ECX
        JS      @@qt
@@lp:   LEA     EBX,[ECX+ESI]
        SHR     EBX,1
        CMP     AX,[EDX+EBX*2]
        JA      @@aa
        MOV     EDI,EBX
        LEA     ECX,[EBX-1]
        CMP     ESI,ECX
        JLE     @@lp
        JMP     @@qt
@@aa:   LEA     ESI,[EBX+1]
        CMP     ECX,ESI
        JGE     @@lp
@@qt:   POP     EBX
        MOV     EAX,EDI
        POP     ESI
        POP     EDI
end;

function Q_ANDSet_Integer(P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer): Cardinal;
asm
        PUSH    ESI
        PUSH    EBX
        PUSH    EDI
        PUSH    EBP
        MOV     EDI,[ESP+24]
        MOV     EBX,[ESP+28]
        TEST    EDI,EDI
        JE      @@lp1
@@lp:   DEC     EDX
        JS      @@qt
        DEC     EBX
        JS      @@qt
        MOV     ESI,[EAX]
        MOV     EBP,[ECX]
        ADD     EAX,4
        ADD     ECX,4
@@nx:   CMP     ESI,EBP
        JL      @@bb
        JG      @@aa
        MOV     [EDI],ESI
        ADD     EDI,4
        JMP     @@lp
@@bb:   DEC     EDX
        JS      @@qt
        MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@nx
@@aa:   DEC     EBX
        JS      @@qt
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@nx
@@qt:   SUB     EDI,[ESP+24]
        SHR     EDI,2
        JMP     @@qt1
@@lp1:  DEC     EDX
        JS      @@qt1
        DEC     EBX
        JS      @@qt1
        MOV     ESI,[EAX]
        MOV     EBP,[ECX]
        ADD     EAX,4
        ADD     ECX,4
@@nx1:  CMP     ESI,EBP
        JL      @@bb1
        JG      @@aa1
        INC     EDI
        JMP     @@lp1
@@bb1:  DEC     EDX
        JS      @@qt1
        MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@nx1
@@aa1:  DEC     EBX
        JS      @@qt1
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@nx1
@@qt1:  MOV     EAX,EDI
        POP     EBP
        POP     EDI
        POP     EBX
        POP     ESI
end;

function Q_ANDSet_LongWord(P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer): Cardinal;
asm
        PUSH    ESI
        PUSH    EBX
        PUSH    EDI
        PUSH    EBP
        MOV     EDI,[ESP+24]
        MOV     EBX,[ESP+28]
        TEST    EDI,EDI
        JE      @@lp1
@@lp:   DEC     EDX
        JS      @@qt
        DEC     EBX
        JS      @@qt
        MOV     ESI,[EAX]
        MOV     EBP,[ECX]
        ADD     EAX,4
        ADD     ECX,4
@@nx:   CMP     ESI,EBP
        JB      @@bb
        JA      @@aa
        MOV     [EDI],ESI
        ADD     EDI,4
        JMP     @@lp
@@bb:   DEC     EDX
        JS      @@qt
        MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@nx
@@aa:   DEC     EBX
        JS      @@qt
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@nx
@@qt:   SUB     EDI,[ESP+24]
        SHR     EDI,2
        JMP     @@qt1
@@lp1:  DEC     EDX
        JS      @@qt1
        DEC     EBX
        JS      @@qt1
        MOV     ESI,[EAX]
        MOV     EBP,[ECX]
        ADD     EAX,4
        ADD     ECX,4
@@nx1:  CMP     ESI,EBP
        JB      @@bb1
        JA      @@aa1
        INC     EDI
        JMP     @@lp1
@@bb1:  DEC     EDX
        JS      @@qt1
        MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@nx1
@@aa1:  DEC     EBX
        JS      @@qt1
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@nx1
@@qt1:  MOV     EAX,EDI
        POP     EBP
        POP     EDI
        POP     EBX
        POP     ESI
end;

function Q_ANDSet_Word(P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer): Cardinal;
asm
        PUSH    ESI
        PUSH    EBX
        PUSH    EDI
        PUSH    EBP
        MOV     EDI,[ESP+24]
        MOV     EBX,[ESP+28]
        TEST    EDI,EDI
        JE      @@lp1
@@lp:   DEC     EDX
        JS      @@qt
        DEC     EBX
        JS      @@qt
        MOV     SI,[EAX]
        MOV     BP,[ECX]
        ADD     EAX,2
        ADD     ECX,2
@@nx:   CMP     SI,BP
        JB      @@bb
        JA      @@aa
        MOV     [EDI],SI
        ADD     EDI,2
        JMP     @@lp
@@bb:   DEC     EDX
        JS      @@qt
        MOV     SI,[EAX]
        ADD     EAX,2
        JMP     @@nx
@@aa:   DEC     EBX
        JS      @@qt
        MOV     BP,[ECX]
        ADD     ECX,2
        JMP     @@nx
@@qt:   SUB     EDI,[ESP+24]
        SHR     EDI,1
        JMP     @@qt1
@@lp1:  DEC     EDX
        JS      @@qt1
        DEC     EBX
        JS      @@qt1
        MOV     SI,[EAX]
        MOV     BP,[ECX]
        ADD     EAX,2
        ADD     ECX,2
@@nx1:  CMP     SI,BP
        JB      @@bb1
        JA      @@aa1
        INC     EDI
        JMP     @@lp1
@@bb1:  DEC     EDX
        JS      @@qt1
        MOV     SI,[EAX]
        ADD     EAX,2
        JMP     @@nx1
@@aa1:  DEC     EBX
        JS      @@qt1
        MOV     BP,[ECX]
        ADD     ECX,2
        JMP     @@nx1
@@qt1:  MOV     EAX,EDI
        POP     EBP
        POP     EDI
        POP     EBX
        POP     ESI
end;

function Q_ORSet_Integer(P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer): Cardinal;
asm
        PUSH    ESI
        PUSH    EBX
        PUSH    EDI
        PUSH    EBP
        MOV     EDI,[ESP+24]
        MOV     EBX,[ESP+28]
        TEST    EDI,EDI
        JE      @@lp1
@@lp:   DEC     EDX
        JS      @@mm
        DEC     EBX
        JS      @@nn
        MOV     ESI,[EAX]
        MOV     EBP,[ECX]
        ADD     EAX,4
        ADD     ECX,4
@@nx:   CMP     ESI,EBP
        JL      @@bb
        JG      @@aa
        MOV     [EDI],ESI
        ADD     EDI,4
        JMP     @@lp
@@bb:   MOV     [EDI],ESI
        ADD     EDI,4
        DEC     EDX
        JS      @@bx
        MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@nx
@@bx:   MOV     [EDI],EBP
        ADD     EDI,4
@@mm:   DEC     EBX
        JS      @@qt
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@bx
@@aa:   MOV     [EDI],EBP
        ADD     EDI,4
        DEC     EBX
        JS      @@ax
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@nx
@@ax:   MOV     [EDI],ESI
        ADD     EDI,4
        DEC     EDX
        JS      @@qt
@@nn:   MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@ax
@@qt:   SUB     EDI,[ESP+24]
        SHR     EDI,2
        JMP     @@qt1
@@lp1:  DEC     EDX
        JS      @@mm1
        DEC     EBX
        JS      @@nn1
        MOV     ESI,[EAX]
        MOV     EBP,[ECX]
        ADD     EAX,4
        ADD     ECX,4
@@nx1:  CMP     ESI,EBP
        JL      @@bb1
        JG      @@aa1
        INC     EDI
        JMP     @@lp1
@@bb1:  INC     EDI
        DEC     EDX
        JS      @@bx1
        MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@nx1
@@bx1:  INC     EDI
@@mm1:  DEC     EBX
        JS      @@qt1
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@bx1
@@aa1:  INC     EDI
        DEC     EBX
        JS      @@ax1
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@nx1
@@ax1:  INC     EDI
        DEC     EDX
        JS      @@qt1
@@nn1:  MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@ax1
@@qt1:  MOV     EAX,EDI
        POP     EBP
        POP     EDI
        POP     EBX
        POP     ESI
end;

function Q_ORSet_LongWord(P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer): Cardinal;
asm
        PUSH    ESI
        PUSH    EBX
        PUSH    EDI
        PUSH    EBP
        MOV     EDI,[ESP+24]
        MOV     EBX,[ESP+28]
        TEST    EDI,EDI
        JE      @@lp1
@@lp:   DEC     EDX
        JS      @@mm
        DEC     EBX
        JS      @@nn
        MOV     ESI,[EAX]
        MOV     EBP,[ECX]
        ADD     EAX,4
        ADD     ECX,4
@@nx:   CMP     ESI,EBP
        JB      @@bb
        JA      @@aa
        MOV     [EDI],ESI
        ADD     EDI,4
        JMP     @@lp
@@bb:   MOV     [EDI],ESI
        ADD     EDI,4
        DEC     EDX
        JS      @@bx
        MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@nx
@@bx:   MOV     [EDI],EBP
        ADD     EDI,4
@@mm:   DEC     EBX
        JS      @@qt
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@bx
@@aa:   MOV     [EDI],EBP
        ADD     EDI,4
        DEC     EBX
        JS      @@ax
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@nx
@@ax:   MOV     [EDI],ESI
        ADD     EDI,4
        DEC     EDX
        JS      @@qt
@@nn:   MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@ax
@@qt:   SUB     EDI,[ESP+24]
        SHR     EDI,2
        JMP     @@qt1
@@lp1:  DEC     EDX
        JS      @@mm1
        DEC     EBX
        JS      @@nn1
        MOV     ESI,[EAX]
        MOV     EBP,[ECX]
        ADD     EAX,4
        ADD     ECX,4
@@nx1:  CMP     ESI,EBP
        JB      @@bb1
        JA      @@aa1
        INC     EDI
        JMP     @@lp1
@@bb1:  INC     EDI
        DEC     EDX
        JS      @@bx1
        MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@nx1
@@bx1:  INC     EDI
@@mm1:  DEC     EBX
        JS      @@qt1
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@bx1
@@aa1:  INC     EDI
        DEC     EBX
        JS      @@ax1
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@nx1
@@ax1:  INC     EDI
        DEC     EDX
        JS      @@qt1
@@nn1:  MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@ax1
@@qt1:  MOV     EAX,EDI
        POP     EBP
        POP     EDI
        POP     EBX
        POP     ESI
end;

function Q_ORSet_Word(P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer): Cardinal;
asm
        PUSH    ESI
        PUSH    EBX
        PUSH    EDI
        PUSH    EBP
        MOV     EDI,[ESP+24]
        MOV     EBX,[ESP+28]
        TEST    EDI,EDI
        JE      @@lp1
@@lp:   DEC     EDX
        JS      @@mm
        DEC     EBX
        JS      @@nn
        MOV     SI,[EAX]
        MOV     BP,[ECX]
        ADD     EAX,2
        ADD     ECX,2
@@nx:   CMP     SI,BP
        JB      @@bb
        JA      @@aa
        MOV     [EDI],SI
        ADD     EDI,2
        JMP     @@lp
@@bb:   MOV     [EDI],SI
        ADD     EDI,2
        DEC     EDX
        JS      @@bx
        MOV     SI,[EAX]
        ADD     EAX,2
        JMP     @@nx
@@bx:   MOV     [EDI],BP
        ADD     EDI,2
@@mm:   DEC     EBX
        JS      @@qt
        MOV     BP,[ECX]
        ADD     ECX,2
        JMP     @@bx
@@aa:   MOV     [EDI],BP
        ADD     EDI,2
        DEC     EBX
        JS      @@ax
        MOV     BP,[ECX]
        ADD     ECX,2
        JMP     @@nx
@@ax:   MOV     [EDI],SI
        ADD     EDI,2
        DEC     EDX
        JS      @@qt
@@nn:   MOV     SI,[EAX]
        ADD     EAX,2
        JMP     @@ax
@@qt:   SUB     EDI,[ESP+24]
        SHR     EDI,1
        JMP     @@qt1
@@lp1:  DEC     EDX
        JS      @@mm1
        DEC     EBX
        JS      @@nn1
        MOV     SI,[EAX]
        MOV     BP,[ECX]
        ADD     EAX,2
        ADD     ECX,2
@@nx1:  CMP     SI,BP
        JB      @@bb1
        JA      @@aa1
        INC     EDI
        JMP     @@lp1
@@bb1:  INC     EDI
        DEC     EDX
        JS      @@bx1
        MOV     SI,[EAX]
        ADD     EAX,2
        JMP     @@nx1
@@bx1:  INC     EDI
@@mm1:  DEC     EBX
        JS      @@qt1
        MOV     BP,[ECX]
        ADD     ECX,2
        JMP     @@bx1
@@aa1:  INC     EDI
        DEC     EBX
        JS      @@ax1
        MOV     BP,[ECX]
        ADD     ECX,2
        JMP     @@nx1
@@ax1:  INC     EDI
        DEC     EDX
        JS      @@qt1
@@nn1:  MOV     SI,[EAX]
        ADD     EAX,2
        JMP     @@ax1
@@qt1:  MOV     EAX,EDI
        POP     EBP
        POP     EDI
        POP     EBX
        POP     ESI
end;

function Q_XORSet_Integer(P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer): Cardinal;
asm
        PUSH    ESI
        PUSH    EBX
        PUSH    EDI
        PUSH    EBP
        MOV     EDI,[ESP+24]
        MOV     EBX,[ESP+28]
        TEST    EDI,EDI
        JE      @@lp1
@@lp:   DEC     EDX
        JS      @@mm
        DEC     EBX
        JS      @@nn
        MOV     ESI,[EAX]
        MOV     EBP,[ECX]
        ADD     EAX,4
        ADD     ECX,4
@@nx:   CMP     ESI,EBP
        JL      @@bb
        JG      @@aa
        JMP     @@lp
@@bb:   MOV     [EDI],ESI
        ADD     EDI,4
        DEC     EDX
        JS      @@bx
        MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@nx
@@bx:   MOV     [EDI],EBP
        ADD     EDI,4
@@mm:   DEC     EBX
        JS      @@qt
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@bx
@@aa:   MOV     [EDI],EBP
        ADD     EDI,4
        DEC     EBX
        JS      @@ax
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@nx
@@ax:   MOV     [EDI],ESI
        ADD     EDI,4
        DEC     EDX
        JS      @@qt
@@nn:   MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@ax
@@qt:   SUB     EDI,[ESP+24]
        SHR     EDI,2
        JMP     @@qt1
@@lp1:  DEC     EDX
        JS      @@mm1
        DEC     EBX
        JS      @@nn1
        MOV     ESI,[EAX]
        MOV     EBP,[ECX]
        ADD     EAX,4
        ADD     ECX,4
@@nx1:  CMP     ESI,EBP
        JL      @@bb1
        JG      @@aa1
        JMP     @@lp1
@@bb1:  INC     EDI
        DEC     EDX
        JS      @@bx1
        MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@nx1
@@bx1:  INC     EDI
@@mm1:  DEC     EBX
        JS      @@qt1
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@bx1
@@aa1:  INC     EDI
        DEC     EBX
        JS      @@ax1
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@nx1
@@ax1:  INC     EDI
        DEC     EDX
        JS      @@qt1
@@nn1:  MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@ax1
@@qt1:  MOV     EAX,EDI
        POP     EBP
        POP     EDI
        POP     EBX
        POP     ESI
end;

function Q_XORSet_LongWord(P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer): Cardinal;
asm
        PUSH    ESI
        PUSH    EBX
        PUSH    EDI
        PUSH    EBP
        MOV     EDI,[ESP+24]
        MOV     EBX,[ESP+28]
        TEST    EDI,EDI
        JE      @@lp1
@@lp:   DEC     EDX
        JS      @@mm
        DEC     EBX
        JS      @@nn
        MOV     ESI,[EAX]
        MOV     EBP,[ECX]
        ADD     EAX,4
        ADD     ECX,4
@@nx:   CMP     ESI,EBP
        JB      @@bb
        JA      @@aa
        JMP     @@lp
@@bb:   MOV     [EDI],ESI
        ADD     EDI,4
        DEC     EDX
        JS      @@bx
        MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@nx
@@bx:   MOV     [EDI],EBP
        ADD     EDI,4
@@mm:   DEC     EBX
        JS      @@qt
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@bx
@@aa:   MOV     [EDI],EBP
        ADD     EDI,4
        DEC     EBX
        JS      @@ax
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@nx
@@ax:   MOV     [EDI],ESI
        ADD     EDI,4
        DEC     EDX
        JS      @@qt
@@nn:   MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@ax
@@qt:   SUB     EDI,[ESP+24]
        SHR     EDI,2
        JMP     @@qt1
@@lp1:  DEC     EDX
        JS      @@mm1
        DEC     EBX
        JS      @@nn1
        MOV     ESI,[EAX]
        MOV     EBP,[ECX]
        ADD     EAX,4
        ADD     ECX,4
@@nx1:  CMP     ESI,EBP
        JB      @@bb1
        JA      @@aa1
        JMP     @@lp1
@@bb1:  INC     EDI
        DEC     EDX
        JS      @@bx1
        MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@nx1
@@bx1:  INC     EDI
@@mm1:  DEC     EBX
        JS      @@qt1
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@bx1
@@aa1:  INC     EDI
        DEC     EBX
        JS      @@ax1
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@nx1
@@ax1:  INC     EDI
        DEC     EDX
        JS      @@qt1
@@nn1:  MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@ax1
@@qt1:  MOV     EAX,EDI
        POP     EBP
        POP     EDI
        POP     EBX
        POP     ESI
end;

function Q_XORSet_Word(P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer): Cardinal;
asm
        PUSH    ESI
        PUSH    EBX
        PUSH    EDI
        PUSH    EBP
        MOV     EDI,[ESP+24]
        MOV     EBX,[ESP+28]
        TEST    EDI,EDI
        JE      @@lp1
@@lp:   DEC     EDX
        JS      @@mm
        DEC     EBX
        JS      @@nn
        MOV     SI,[EAX]
        MOV     BP,[ECX]
        ADD     EAX,2
        ADD     ECX,2
@@nx:   CMP     SI,BP
        JB      @@bb
        JA      @@aa
        JMP     @@lp
@@bb:   MOV     [EDI],SI
        ADD     EDI,2
        DEC     EDX
        JS      @@bx
        MOV     SI,[EAX]
        ADD     EAX,2
        JMP     @@nx
@@bx:   MOV     [EDI],BP
        ADD     EDI,2
@@mm:   DEC     EBX
        JS      @@qt
        MOV     BP,[ECX]
        ADD     ECX,2
        JMP     @@bx
@@aa:   MOV     [EDI],BP
        ADD     EDI,2
        DEC     EBX
        JS      @@ax
        MOV     BP,[ECX]
        ADD     ECX,2
        JMP     @@nx
@@ax:   MOV     [EDI],SI
        ADD     EDI,2
        DEC     EDX
        JS      @@qt
@@nn:   MOV     SI,[EAX]
        ADD     EAX,2
        JMP     @@ax
@@qt:   SUB     EDI,[ESP+24]
        SHR     EDI,1
        JMP     @@qt1
@@lp1:  DEC     EDX
        JS      @@mm1
        DEC     EBX
        JS      @@nn1
        MOV     SI,[EAX]
        MOV     BP,[ECX]
        ADD     EAX,2
        ADD     ECX,2
@@nx1:  CMP     SI,BP
        JB      @@bb1
        JA      @@aa1
        JMP     @@lp1
@@bb1:  INC     EDI
        DEC     EDX
        JS      @@bx1
        MOV     SI,[EAX]
        ADD     EAX,2
        JMP     @@nx1
@@bx1:  INC     EDI
@@mm1:  DEC     EBX
        JS      @@qt1
        MOV     BP,[ECX]
        ADD     ECX,2
        JMP     @@bx1
@@aa1:  INC     EDI
        DEC     EBX
        JS      @@ax1
        MOV     BP,[ECX]
        ADD     ECX,2
        JMP     @@nx1
@@ax1:  INC     EDI
        DEC     EDX
        JS      @@qt1
@@nn1:  MOV     SI,[EAX]
        ADD     EAX,2
        JMP     @@ax1
@@qt1:  MOV     EAX,EDI
        POP     EBP
        POP     EDI
        POP     EBX
        POP     ESI
end;

function Q_ANDNOTSet_Integer(P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer): Cardinal;
asm
        PUSH    ESI
        PUSH    EBX
        PUSH    EDI
        PUSH    EBP
        MOV     EDI,[ESP+24]
        MOV     EBX,[ESP+28]
        TEST    EDI,EDI
        JE      @@lp1
@@lp:   DEC     EDX
        JS      @@qt
        DEC     EBX
        JS      @@nn
        MOV     ESI,[EAX]
        MOV     EBP,[ECX]
        ADD     EAX,4
        ADD     ECX,4
@@nx:   CMP     ESI,EBP
        JL      @@bb
        JG      @@aa
        JMP     @@lp
@@bb:   MOV     [EDI],ESI
        ADD     EDI,4
        DEC     EDX
        JS      @@qt
        MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@nx
@@aa:   DEC     EBX
        JS      @@ax
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@nx
@@ax:   MOV     [EDI],ESI
        ADD     EDI,4
        DEC     EDX
        JS      @@qt
@@nn:   MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@ax
@@qt:   SUB     EDI,[ESP+24]
        SHR     EDI,2
        JMP     @@qt1
@@lp1:  DEC     EDX
        JS      @@qt1
        DEC     EBX
        JS      @@nn1
        MOV     ESI,[EAX]
        MOV     EBP,[ECX]
        ADD     EAX,4
        ADD     ECX,4
@@nx1:  CMP     ESI,EBP
        JL      @@bb1
        JG      @@aa1
        JMP     @@lp1
@@bb1:  INC     EDI
        DEC     EDX
        JS      @@qt1
        MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@nx1
@@aa1:  DEC     EBX
        JS      @@ax1
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@nx1
@@ax1:  INC     EDI
        DEC     EDX
        JS      @@qt1
@@nn1:  MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@ax1
@@qt1:  MOV     EAX,EDI
        POP     EBP
        POP     EDI
        POP     EBX
        POP     ESI
end;

function Q_ANDNOTSet_LongWord(P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer): Cardinal;
asm
        PUSH    ESI
        PUSH    EBX
        PUSH    EDI
        PUSH    EBP
        MOV     EDI,[ESP+24]
        MOV     EBX,[ESP+28]
        TEST    EDI,EDI
        JE      @@lp1
@@lp:   DEC     EDX
        JS      @@qt
        DEC     EBX
        JS      @@nn
        MOV     ESI,[EAX]
        MOV     EBP,[ECX]
        ADD     EAX,4
        ADD     ECX,4
@@nx:   CMP     ESI,EBP
        JB      @@bb
        JA      @@aa
        JMP     @@lp
@@bb:   MOV     [EDI],ESI
        ADD     EDI,4
        DEC     EDX
        JS      @@qt
        MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@nx
@@aa:   DEC     EBX
        JS      @@ax
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@nx
@@ax:   MOV     [EDI],ESI
        ADD     EDI,4
        DEC     EDX
        JS      @@qt
@@nn:   MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@ax
@@qt:   SUB     EDI,[ESP+24]
        SHR     EDI,2
        JMP     @@qt1
@@lp1:  DEC     EDX
        JS      @@qt1
        DEC     EBX
        JS      @@nn1
        MOV     ESI,[EAX]
        MOV     EBP,[ECX]
        ADD     EAX,4
        ADD     ECX,4
@@nx1:  CMP     ESI,EBP
        JB      @@bb1
        JA      @@aa1
        JMP     @@lp1
@@bb1:  INC     EDI
        DEC     EDX
        JS      @@qt1
        MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@nx1
@@aa1:  DEC     EBX
        JS      @@ax1
        MOV     EBP,[ECX]
        ADD     ECX,4
        JMP     @@nx1
@@ax1:  INC     EDI
        DEC     EDX
        JS      @@qt1
@@nn1:  MOV     ESI,[EAX]
        ADD     EAX,4
        JMP     @@ax1
@@qt1:  MOV     EAX,EDI
        POP     EBP
        POP     EDI
        POP     EBX
        POP     ESI
end;

function Q_ANDNOTSet_Word(P1: Pointer; Count1: Cardinal;
  P2: Pointer; Count2: Cardinal; OutPlace: Pointer): Cardinal;
asm
        PUSH    ESI
        PUSH    EBX
        PUSH    EDI
        PUSH    EBP
        MOV     EDI,[ESP+24]
        MOV     EBX,[ESP+28]
        TEST    EDI,EDI
        JE      @@lp1
@@lp:   DEC     EDX
        JS      @@qt
        DEC     EBX
        JS      @@nn
        MOV     SI,[EAX]
        MOV     BP,[ECX]
        ADD     EAX,2
        ADD     ECX,2
@@nx:   CMP     SI,BP
        JB      @@bb
        JA      @@aa
        JMP     @@lp
@@bb:   MOV     [EDI],SI
        ADD     EDI,2
        DEC     EDX
        JS      @@qt
        MOV     SI,[EAX]
        ADD     EAX,2
        JMP     @@nx
@@aa:   DEC     EBX
        JS      @@ax
        MOV     BP,[ECX]
        ADD     ECX,2
        JMP     @@nx
@@ax:   MOV     [EDI],SI
        ADD     EDI,2
        DEC     EDX
        JS      @@qt
@@nn:   MOV     SI,[EAX]
        ADD     EAX,2
        JMP     @@ax
@@qt:   SUB     EDI,[ESP+24]
        SHR     EDI,1
        JMP     @@qt1
@@lp1:  DEC     EDX
        JS      @@qt1
        DEC     EBX
        JS      @@nn1
        MOV     SI,[EAX]
        MOV     BP,[ECX]
        ADD     EAX,2
        ADD     ECX,2
@@nx1:  CMP     SI,BP
        JB      @@bb1
        JA      @@aa1
        JMP     @@lp1
@@bb1:  INC     EDI
        DEC     EDX
        JS      @@qt1
        MOV     SI,[EAX]
        ADD     EAX,2
        JMP     @@nx1
@@aa1:  DEC     EBX
        JS      @@ax1
        MOV     BP,[ECX]
        ADD     ECX,2
        JMP     @@nx1
@@ax1:  INC     EDI
        DEC     EDX
        JS      @@qt1
@@nn1:  MOV     SI,[EAX]
        ADD     EAX,2
        JMP     @@ax1
@@qt1:  MOV     EAX,EDI
        POP     EBP
        POP     EDI
        POP     EBX
        POP     ESI
end;

function Q_BitTest32(D: LongWord; Index: Cardinal): Boolean;
asm
        MOV     CL,DL
        SHR     EAX,CL
        AND     EAX,1
end;

const
  BitMasks: array[0..31] of LongWord =
    ($00000001,$00000002,$00000004,$00000008,$00000010,$00000020,$00000040,$00000080,
     $00000100,$00000200,$00000400,$00000800,$00001000,$00002000,$00004000,$00008000,
     $00010000,$00020000,$00040000,$00080000,$00100000,$00200000,$00400000,$00800000,
     $01000000,$02000000,$04000000,$08000000,$10000000,$20000000,$40000000,$80000000);

function Q_BitSet32(D: LongWord; Index: Cardinal): LongWord;
asm
        OR      EAX,DWORD PTR [EDX*4+BitMasks]
end;

function Q_BitReset32(D: LongWord; Index: Cardinal): LongWord;
asm
        MOV     ECX,DWORD PTR [EDX*4+BitMasks]
        NOT     ECX
        AND     EAX,ECX
end;

function Q_BitToggle32(D: LongWord; Index: Cardinal): LongWord;
asm
        XOR     EAX,DWORD PTR [EDX*4+BitMasks]
end;

function Q_CountOfSetBits32(D: LongWord): Cardinal;
asm
        PUSH    EBX
        MOVZX   EDX,AL
        MOVZX   ECX,AH
        SHR     EAX,16
        MOVZX   EBX,AH
        AND     EAX,$FF
        MOVZX   EAX,BYTE PTR [EAX+BitTable]
        ADD     AL,BYTE PTR [EBX+BitTable]
        ADD     AL,BYTE PTR [ECX+BitTable]
        ADD     AL,BYTE PTR [EDX+BitTable]
        POP     EBX
end;

function Q_CountOfFreeBits32(D: LongWord): Cardinal;
asm
        PUSH    EBX
        MOVZX   EDX,AL
        MOVZX   ECX,AH
        SHR     EAX,16
        MOVZX   EBX,AH
        AND     EAX,$FF
        MOV     BL,BYTE PTR [EBX+BitTable]
        ADD     BL,BYTE PTR [EAX+BitTable]
        MOV     EAX,32
        ADD     BL,BYTE PTR [ECX+BitTable]
        ADD     BL,BYTE PTR [EDX+BitTable]
        SUB     AL,BL
        POP     EBX
end;

function Q_SetBitScanForward32(D: LongWord; FirstBit: Integer): Integer;
asm
        MOV     ECX,EDX
        MOV     EDX,$FFFFFFFF
        SHL     EDX,CL
        AND     EDX,EAX
        JE      @@zq
        BSF     EAX,EDX
        RET
@@zq:   MOV     EAX,$FFFFFFFF
end;

function Q_FreeBitScanForward32(D: LongWord; FirstBit: Integer): Integer;
asm
        MOV     ECX,EDX
        MOV     EDX,$FFFFFFFF
        NOT     EAX
        SHL     EDX,CL
        AND     EDX,EAX
        JE      @@zq
        BSF     EAX,EDX
        RET
@@zq:   MOV     EAX,$FFFFFFFF
end;

function Q_SetBitScanReverse32(D: LongWord; LastBit: Integer): Integer;
asm
        LEA     ECX,[EDX-31]
        MOV     EDX,$FFFFFFFF
        NEG     ECX
        SHR     EDX,CL
        AND     EDX,EAX
        JE      @@zq
        BSR     EAX,EDX
        RET
@@zq:   MOV     EAX,$FFFFFFFF
end;

function Q_FreeBitScanReverse32(D: LongWord; LastBit: Integer): Integer;
asm
        LEA     ECX,[EDX-31]
        MOV     EDX,$FFFFFFFF
        NEG     ECX
        NOT     EAX
        SHR     EDX,CL
        AND     EDX,EAX
        JE      @@zq
        BSR     EAX,EDX
        RET
@@zq:   MOV     EAX,$FFFFFFFF
end;

function Q_BitTest(P: Pointer; Index: Integer): Boolean;
asm
        BT      [EAX],EDX
        SETC    AL
end;

function Q_BitSet(P: Pointer; Index: Integer): Boolean;
asm
        BTS     [EAX],EDX
        SETC    AL
end;

function Q_BitReset(P: Pointer; Index: Integer): Boolean;
asm
        BTR     [EAX],EDX
        SETC    AL
end;

function Q_BitToggle(P: Pointer; Index: Integer): Boolean;
asm
        BTC     [EAX],EDX
        SETC    AL
end;

procedure Q_SetBits(P: Pointer; FirstBit, LastBit: Integer);
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        LEA     ESI,[EDX+8]
        CMP     ECX,ESI
        JL      @@ut
        MOV     EBX,$FFFFFFFF
        MOV     ESI,ECX
        MOV     EDI,$0000001F
        AND     ECX,EDI
        AND     ESI,$FFFFFFE0
        SUB     EDI,ECX
        SHR     ESI,5
        MOV     ECX,EDI
        MOV     EDI,EBX
        SHR     EDI,CL
        MOV     ECX,EDX
        AND     EDX,$FFFFFFE0
        AND     ECX,$0000001F
        SHR     EDX,5
        SHL     EBX,CL
        SUB     ESI,EDX
        JE      @@xx
        OR      [EAX+EDX*4],EBX
        INC     EDX
        DEC     ESI
        JE      @@ne
        MOV     EBX,$FFFFFFFF
@@lp:   MOV     [EAX+EDX*4],EBX
        INC     EDX
        DEC     ESI
        JNE     @@lp
@@xx:   AND     EDI,EBX
@@ne:   OR      [EAX+EDX*4],EDI
        POP     EBX
        POP     ESI
        POP     EDI
        RET
@@ut:   SUB     ECX,EDX
        JS      @@qt
@@uk:   BTS     [EAX],EDX
        INC     EDX
        DEC     ECX
        JNS     @@uk
@@qt:   POP     EBX
        POP     ESI
        POP     EDI
end;

procedure Q_ResetBits(P: Pointer; FirstBit, LastBit: Integer);
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        LEA     ESI,[EDX+8]
        CMP     ECX,ESI
        JL      @@ut
        MOV     EBX,$FFFFFFFF
        MOV     ESI,ECX
        MOV     EDI,$0000001F
        AND     ECX,EDI
        AND     ESI,$FFFFFFE0
        SUB     EDI,ECX
        SHR     ESI,5
        MOV     ECX,EDI
        MOV     EDI,EBX
        SHR     EDI,CL
        MOV     ECX,EDX
        AND     EDX,$FFFFFFE0
        AND     ECX,$0000001F
        SHR     EDX,5
        SHL     EBX,CL
        NOT     EDI
        NOT     EBX
        SUB     ESI,EDX
        JE      @@xx
        AND     [EAX+EDX*4],EBX
        INC     EDX
        DEC     ESI
        JE      @@ne
        XOR     EBX,EBX
@@lp:   MOV     [EAX+EDX*4],EBX
        INC     EDX
        DEC     ESI
        JNE     @@lp
@@xx:   OR      EDI,EBX
@@ne:   AND     [EAX+EDX*4],EDI
        POP     EBX
        POP     ESI
        POP     EDI
        RET
@@ut:   SUB     ECX,EDX
        JS      @@qt
@@uk:   BTR     [EAX],EDX
        INC     EDX
        DEC     ECX
        JNS     @@uk
@@qt:   POP     EBX
        POP     ESI
        POP     EDI
end;

procedure Q_ToggleBits(P: Pointer; FirstBit, LastBit: Integer);
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        LEA     ESI,[EDX+8]
        CMP     ECX,ESI
        JL      @@ut
        MOV     EBX,$FFFFFFFF
        MOV     ESI,ECX
        MOV     EDI,$0000001F
        AND     ECX,EDI
        AND     ESI,$FFFFFFE0
        SUB     EDI,ECX
        SHR     ESI,5
        MOV     ECX,EDI
        MOV     EDI,EBX
        SHR     EDI,CL
        MOV     ECX,EDX
        AND     EDX,$FFFFFFE0
        AND     ECX,$0000001F
        SHR     EDX,5
        SHL     EBX,CL
        SUB     ESI,EDX
        JE      @@xx
        XOR     [EAX+EDX*4],EBX
        INC     EDX
        DEC     ESI
        JE      @@ne
        MOV     EBX,$FFFFFFFF
@@lp:   NOT     DWORD PTR [EAX+EDX*4]
        INC     EDX
        DEC     ESI
        JNE     @@lp
@@xx:   AND     EDI,EBX
@@ne:   XOR     [EAX+EDX*4],EDI
        POP     EBX
        POP     ESI
        POP     EDI
        RET
@@ut:   SUB     ECX,EDX
        JS      @@qt
@@uk:   BTC     [EAX],EDX
        INC     EDX
        DEC     ECX
        JNS     @@uk
@@qt:   POP     EBX
        POP     ESI
        POP     EDI
end;

function Q_CountOfSetBits(P: Pointer; L: Cardinal): Cardinal;
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        XOR     EAX,EAX
        SUB     EDX,2
        JS      @@nx
@@lp:   MOVZX   ECX,BYTE PTR [EBX+EDX]
        MOVZX   ESI,BYTE PTR [EBX+EDX+1]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   ESI,BYTE PTR [ESI+BitTable]
        ADD     EAX,ESI
        SUB     EDX,2
        JNS     @@lp
@@nx:   INC     EDX
        JZ      @@qt2
        POP     ESI
        POP     EBX
        RET
@@qt2:  MOVZX   ECX,BYTE PTR [EBX]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        POP     ESI
        POP     EBX
end;

function Q_CountOfFreeBits(P: Pointer; L: Cardinal): Cardinal;
asm
        PUSH    EDX
        CALL    Q_CountOfSetBits
        NEG     EAX
        POP     EDX
        LEA     EAX,[EAX+EDX*8]
end;

function Q_SetBitScanForward(P: Pointer; FirstBit, LastBit: Integer): Integer;
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        LEA     ESI,[EDX+8]
        CMP     ECX,ESI
        JL      @@ut
        MOV     EBX,$FFFFFFFF
        MOV     ESI,ECX
        MOV     EDI,$0000001F
        AND     ECX,EDI
        AND     ESI,$FFFFFFE0
        SUB     EDI,ECX
        SHR     ESI,5
        MOV     ECX,EDI
        MOV     EDI,EBX
        SHR     EDI,CL
        MOV     ECX,EDX
        AND     EDX,$FFFFFFE0
        AND     ECX,$0000001F
        SHR     EDX,5
        SHL     EBX,CL
        AND     EBX,[EAX+EDX*4]
        SUB     ESI,EDX
        JE      @@nq
        TEST    EBX,EBX
        JNE     @@ne
        INC     EDX
        DEC     ESI
        JE      @@xx
@@lp:   OR      EBX,[EAX+EDX*4]
        JNE     @@ne
        INC     EDX
        DEC     ESI
        JNE     @@lp
@@xx:   MOV     EBX,[EAX+EDX*4]
@@nq:   AND     EBX,EDI
        JE      @@zq
@@ne:   BSF     ECX,EBX
@@qt:   SHL     EDX,5
        LEA     EAX,[ECX+EDX]
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@ut:   SUB     ECX,EDX
        JS      @@zq
@@uk:   BT      [EAX],EDX
        JC      @@iq
        INC     EDX
        DEC     ECX
        JNS     @@uk
@@zq:   MOV     EAX,$FFFFFFFF
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@iq:   MOV     EAX,EDX
        POP     EDI
        POP     ESI
        POP     EBX
end;

function Q_FreeBitScanForward(P: Pointer; FirstBit, LastBit: Integer): Integer;
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        LEA     ESI,[EDX+8]
        CMP     ECX,ESI
        JL      @@ut
        MOV     EBX,$FFFFFFFF
        MOV     ESI,ECX
        MOV     EDI,$0000001F
        AND     ECX,EDI
        AND     ESI,$FFFFFFE0
        SUB     EDI,ECX
        SHR     ESI,5
        MOV     ECX,EDI
        MOV     EDI,EBX
        SHR     EDI,CL
        MOV     ECX,EDX
        AND     EDX,$FFFFFFE0
        AND     ECX,$0000001F
        SHR     EDX,5
        SHL     EBX,CL
        MOV     ECX,[EAX+EDX*4]
        NOT     ECX
        AND     EBX,ECX
        SUB     ESI,EDX
        JE      @@nq
        TEST    EBX,EBX
        JNE     @@ne
        INC     EDX
        DEC     ESI
        JE      @@xx
@@lp:   MOV     EBX,[EAX+EDX*4]
        NOT     EBX
        TEST    EBX,EBX
        JNE     @@ne
        INC     EDX
        DEC     ESI
        JNE     @@lp
@@xx:   MOV     EBX,[EAX+EDX*4]
        NOT     EBX
@@nq:   AND     EBX,EDI
        JE      @@zq
@@ne:   BSF     ECX,EBX
@@qt:   SHL     EDX,5
        LEA     EAX,[ECX+EDX]
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@ut:   SUB     ECX,EDX
        JS      @@zq
@@uk:   BT      [EAX],EDX
        JNC     @@iq
        INC     EDX
        DEC     ECX
        JNS     @@uk
@@zq:   MOV     EAX,$FFFFFFFF
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@iq:   MOV     EAX,EDX
        POP     EDI
        POP     ESI
        POP     EBX
end;

function Q_SetBitScanReverse(P: Pointer; FirstBit, LastBit: Integer): Integer;
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        LEA     ESI,[EDX+8]
        CMP     ECX,ESI
        JL      @@ut
        MOV     EBX,$FFFFFFFF
        MOV     ESI,ECX
        MOV     EDI,$0000001F
        AND     ECX,EDI
        AND     ESI,$FFFFFFE0
        SUB     EDI,ECX
        SHR     ESI,5
        MOV     ECX,EDI
        MOV     EDI,EBX
        SHR     EDI,CL
        MOV     ECX,EDX
        AND     EDX,$FFFFFFE0
        AND     ECX,$0000001F
        SHR     EDX,5
        SHL     EBX,CL
        AND     EDI,[EAX+ESI*4]
        SUB     EDX,ESI
        JE      @@nq
        TEST    EDI,EDI
        JNE     @@ne
        NEG     EDX
        DEC     ESI
        DEC     EDX
        JE      @@xx
@@lp:   OR      EDI,[EAX+ESI*4]
        JNE     @@ne
        DEC     ESI
        DEC     EDX
        JNE     @@lp
@@xx:   MOV     EDI,[EAX+ESI*4]
@@nq:   AND     EDI,EBX
        JE      @@zq
@@ne:   BSR     ECX,EDI
@@qt:   SHL     ESI,5
        LEA     EAX,[ECX+ESI]
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@ut:   SUB     EDX,ECX
        JG      @@zq
@@uk:   BT      [EAX],ECX
        JC      @@iq
        DEC     ECX
        INC     EDX
        JNG     @@uk
@@zq:   MOV     EAX,$FFFFFFFF
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@iq:   MOV     EAX,ECX
        POP     EDI
        POP     ESI
        POP     EBX
end;

function Q_FreeBitScanReverse(P: Pointer; FirstBit, LastBit: Integer): Integer;
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        LEA     ESI,[EDX+8]
        CMP     ECX,ESI
        JL      @@ut
        MOV     EBX,$FFFFFFFF
        MOV     ESI,ECX
        MOV     EDI,$0000001F
        AND     ECX,EDI
        AND     ESI,$FFFFFFE0
        SUB     EDI,ECX
        SHR     ESI,5
        MOV     ECX,EDI
        MOV     EDI,EBX
        SHR     EDI,CL
        MOV     ECX,EDX
        AND     EDX,$FFFFFFE0
        AND     ECX,$0000001F
        SHR     EDX,5
        SHL     EBX,CL
        MOV     ECX,[EAX+ESI*4]
        NOT     ECX
        AND     EDI,ECX
        SUB     EDX,ESI
        JE      @@nq
        TEST    EDI,EDI
        JNE     @@ne
        NEG     EDX
        DEC     ESI
        DEC     EDX
        JE      @@xx
@@lp:   MOV     EDI,[EAX+ESI*4]
        NOT     EDI
        TEST    EDI,EDI
        JNE     @@ne
        DEC     ESI
        DEC     EDX
        JNE     @@lp
@@xx:   MOV     EDI,[EAX+ESI*4]
        NOT     EDI
@@nq:   AND     EDI,EBX
        JE      @@zq
@@ne:   BSR     ECX,EDI
@@qt:   SHL     ESI,5
        LEA     EAX,[ECX+ESI]
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@ut:   SUB     EDX,ECX
        JG      @@zq
@@uk:   BT      [EAX],ECX
        JNC     @@iq
        DEC     ECX
        INC     EDX
        JNG     @@uk
@@zq:   MOV     EAX,$FFFFFFFF
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@iq:   MOV     EAX,ECX
        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure Q_NOTByteArr(P: Pointer; L: Cardinal);
asm
        MOV     ECX,EDX
        AND     EDX,15
        SHR     ECX,4
        JE      @@nx
@@lp:   NOT     DWORD PTR [EAX]
        NOT     DWORD PTR [EAX+4]
        NOT     DWORD PTR [EAX+8]
        NOT     DWORD PTR [EAX+12]
        ADD     EAX,16
        DEC     ECX
        JNE     @@lp
@@nx:   JMP     DWORD PTR @@kV[EDX*4]
@@kV:   DD      @@ku00, @@ku01, @@ku02, @@ku03
        DD      @@ku04, @@ku05, @@ku06, @@ku07
        DD      @@ku08, @@ku09, @@ku10, @@ku11
        DD      @@ku12, @@ku13, @@ku14, @@ku15
@@ku15: NOT     BYTE PTR [EAX+14]
@@ku14: NOT     BYTE PTR [EAX+13]
@@ku13: NOT     BYTE PTR [EAX+12]
@@ku12: NOT     DWORD PTR [EAX+8]
        NOT     DWORD PTR [EAX+4]
        NOT     DWORD PTR [EAX]
        RET
@@ku11: NOT     BYTE PTR [EAX+10]
@@ku10: NOT     BYTE PTR [EAX+9]
@@ku09: NOT     BYTE PTR [EAX+8]
@@ku08: NOT     DWORD PTR [EAX+4]
        NOT     DWORD PTR [EAX]
        RET
@@ku07: NOT     BYTE PTR [EAX+6]
@@ku06: NOT     BYTE PTR [EAX+5]
@@ku05: NOT     BYTE PTR [EAX+4]
@@ku04: NOT     DWORD PTR [EAX]
        RET
@@ku03: NOT     BYTE PTR [EAX+2]
@@ku02: NOT     BYTE PTR [EAX+1]
@@ku01: NOT     BYTE PTR [EAX]
@@ku00:
end;

procedure Q_XORByData(Dest, Source: Pointer; L: Cardinal);
asm
        TEST    ECX,ECX
        JE      @@qt
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EDX
        MOV     EDI,EAX
        MOV     EDX,ECX
        AND     ECX,15
        SHR     EDX,4
        JE      @@nx
@@lp:   MOV     EAX,[ESI]
        XOR     [EDI],EAX
        MOV     EAX,[ESI+4]
        XOR     [EDI+4],EAX
        MOV     EAX,[ESI+8]
        XOR     [EDI+8],EAX
        MOV     EAX,[ESI+12]
        XOR     [EDI+12],EAX
        ADD     ESI,16
        ADD     EDI,16
        DEC     EDX
        JNE     @@lp
@@nx:   JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@t00, @@t01, @@t02, @@t03
        DD      @@t04, @@t05, @@t06, @@t07
        DD      @@t08, @@t09, @@t10, @@t11
        DD      @@t12, @@t13, @@t14, @@t15
@@t15:  MOV     AL,BYTE PTR [ESI+14]
        XOR     BYTE PTR [EDI+14],AL
@@t14:  MOV     AL,BYTE PTR [ESI+13]
        XOR     BYTE PTR [EDI+13],AL
@@t13:  MOV     AL,BYTE PTR [ESI+12]
        XOR     BYTE PTR [EDI+12],AL
@@t12:  MOV     EAX,[ESI+8]
        XOR     [EDI+8],EAX
        MOV     EAX,[ESI+4]
        XOR     [EDI+4],EAX
        MOV     EAX,[ESI]
        XOR     [EDI],EAX
@@t00:  POP     EDI
        POP     ESI
@@qt:   RET
@@t11:  MOV     AL,BYTE PTR [ESI+10]
        XOR     BYTE PTR [EDI+10],AL
@@t10:  MOV     AL,BYTE PTR [ESI+9]
        XOR     BYTE PTR [EDI+9],AL
@@t09:  MOV     AL,BYTE PTR [ESI+8]
        XOR     BYTE PTR [EDI+8],AL
@@t08:  MOV     EAX,[ESI+4]
        XOR     [EDI+4],EAX
        MOV     EAX,[ESI]
        XOR     [EDI],EAX
        POP     EDI
        POP     ESI
        RET
@@t07:  MOV     AL,BYTE PTR [ESI+6]
        XOR     BYTE PTR [EDI+6],AL
@@t06:  MOV     AL,BYTE PTR [ESI+5]
        XOR     BYTE PTR [EDI+5],AL
@@t05:  MOV     AL,BYTE PTR [ESI+4]
        XOR     BYTE PTR [EDI+4],AL
@@t04:  MOV     EAX,[ESI]
        XOR     [EDI],EAX
        POP     EDI
        POP     ESI
        RET
@@t03:  MOV     AL,BYTE PTR [ESI+2]
        XOR     BYTE PTR [EDI+2],AL
@@t02:  MOV     AL,BYTE PTR [ESI+1]
        XOR     BYTE PTR [EDI+1],AL
@@t01:  MOV     AL,BYTE PTR [ESI]
        XOR     BYTE PTR [EDI],AL
        POP     EDI
        POP     ESI
end;

procedure Q_ANDLongs(Dest, Source: Pointer; Count: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,ECX
        AND     EBX,7
        SHR     ECX,3
        JE      @@nx
@@lp:   MOV     ESI,[EDX]
        AND     [EAX],ESI
        MOV     ESI,[EDX+4]
        AND     [EAX+4],ESI
        MOV     ESI,[EDX+8]
        AND     [EAX+8],ESI
        MOV     ESI,[EDX+12]
        AND     [EAX+12],ESI
        MOV     ESI,[EDX+16]
        AND     [EAX+16],ESI
        MOV     ESI,[EDX+20]
        AND     [EAX+20],ESI
        MOV     ESI,[EDX+24]
        AND     [EAX+24],ESI
        MOV     ESI,[EDX+28]
        AND     [EAX+28],ESI
        ADD     EDX,32
        ADD     EAX,32
        DEC     ECX
        JNE     @@lp
@@nx:   JMP     DWORD PTR @@kV[EBX*4]
@@kV:   DD      @@ku00, @@ku01, @@ku02, @@ku03
        DD      @@ku04, @@ku05, @@ku06, @@ku07
@@ku07: MOV     ESI,[EDX+24]
        AND     [EAX+24],ESI
@@ku06: MOV     ESI,[EDX+20]
        AND     [EAX+20],ESI
@@ku05: MOV     ESI,[EDX+16]
        AND     [EAX+16],ESI
@@ku04: MOV     ESI,[EDX+12]
        AND     [EAX+12],ESI
@@ku03: MOV     ESI,[EDX+8]
        AND     [EAX+8],ESI
@@ku02: MOV     ESI,[EDX+4]
        AND     [EAX+4],ESI
@@ku01: MOV     ESI,[EDX]
        AND     [EAX],ESI
@@ku00: POP     ESI
        POP     EBX
end;

procedure Q_ORLongs(Dest, Source: Pointer; Count: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,ECX
        AND     EBX,7
        SHR     ECX,3
        JE      @@nx
@@lp:   MOV     ESI,[EDX]
        OR      [EAX],ESI
        MOV     ESI,[EDX+4]
        OR      [EAX+4],ESI
        MOV     ESI,[EDX+8]
        OR      [EAX+8],ESI
        MOV     ESI,[EDX+12]
        OR      [EAX+12],ESI
        MOV     ESI,[EDX+16]
        OR      [EAX+16],ESI
        MOV     ESI,[EDX+20]
        OR      [EAX+20],ESI
        MOV     ESI,[EDX+24]
        OR      [EAX+24],ESI
        MOV     ESI,[EDX+28]
        OR      [EAX+28],ESI
        ADD     EDX,32
        ADD     EAX,32
        DEC     ECX
        JNE     @@lp
@@nx:   JMP     DWORD PTR @@kV[EBX*4]
@@kV:   DD      @@ku00, @@ku01, @@ku02, @@ku03
        DD      @@ku04, @@ku05, @@ku06, @@ku07
@@ku07: MOV     ESI,[EDX+24]
        OR      [EAX+24],ESI
@@ku06: MOV     ESI,[EDX+20]
        OR      [EAX+20],ESI
@@ku05: MOV     ESI,[EDX+16]
        OR      [EAX+16],ESI
@@ku04: MOV     ESI,[EDX+12]
        OR      [EAX+12],ESI
@@ku03: MOV     ESI,[EDX+8]
        OR      [EAX+8],ESI
@@ku02: MOV     ESI,[EDX+4]
        OR      [EAX+4],ESI
@@ku01: MOV     ESI,[EDX]
        OR      [EAX],ESI
@@ku00: POP     ESI
        POP     EBX
end;

procedure Q_XORLongs(Dest, Source: Pointer; Count: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,ECX
        AND     EBX,7
        SHR     ECX,3
        JE      @@nx
@@lp:   MOV     ESI,[EDX]
        XOR     [EAX],ESI
        MOV     ESI,[EDX+4]
        XOR     [EAX+4],ESI
        MOV     ESI,[EDX+8]
        XOR     [EAX+8],ESI
        MOV     ESI,[EDX+12]
        XOR     [EAX+12],ESI
        MOV     ESI,[EDX+16]
        XOR     [EAX+16],ESI
        MOV     ESI,[EDX+20]
        XOR     [EAX+20],ESI
        MOV     ESI,[EDX+24]
        XOR     [EAX+24],ESI
        MOV     ESI,[EDX+28]
        XOR     [EAX+28],ESI
        ADD     EDX,32
        ADD     EAX,32
        DEC     ECX
        JNE     @@lp
@@nx:   JMP     DWORD PTR @@kV[EBX*4]
@@kV:   DD      @@ku00, @@ku01, @@ku02, @@ku03
        DD      @@ku04, @@ku05, @@ku06, @@ku07
@@ku07: MOV     ESI,[EDX+24]
        XOR     [EAX+24],ESI
@@ku06: MOV     ESI,[EDX+20]
        XOR     [EAX+20],ESI
@@ku05: MOV     ESI,[EDX+16]
        XOR     [EAX+16],ESI
@@ku04: MOV     ESI,[EDX+12]
        XOR     [EAX+12],ESI
@@ku03: MOV     ESI,[EDX+8]
        XOR     [EAX+8],ESI
@@ku02: MOV     ESI,[EDX+4]
        XOR     [EAX+4],ESI
@@ku01: MOV     ESI,[EDX]
        XOR     [EAX],ESI
@@ku00: POP     ESI
        POP     EBX
end;

procedure Q_NOTLongArr(P: Pointer; Count: Cardinal);
asm
        MOV     ECX,EDX
        AND     EDX,15
        SHR     ECX,4
        JE      @@nx
@@lp:   NOT     DWORD PTR [EAX]
        NOT     DWORD PTR [EAX+4]
        NOT     DWORD PTR [EAX+8]
        NOT     DWORD PTR [EAX+12]
        NOT     DWORD PTR [EAX+16]
        NOT     DWORD PTR [EAX+20]
        NOT     DWORD PTR [EAX+24]
        NOT     DWORD PTR [EAX+28]
        NOT     DWORD PTR [EAX+32]
        NOT     DWORD PTR [EAX+36]
        NOT     DWORD PTR [EAX+40]
        NOT     DWORD PTR [EAX+44]
        NOT     DWORD PTR [EAX+48]
        NOT     DWORD PTR [EAX+52]
        NOT     DWORD PTR [EAX+56]
        NOT     DWORD PTR [EAX+60]
        ADD     EAX,64
        DEC     ECX
        JNE     @@lp
@@nx:   JMP     DWORD PTR @@kV[EDX*4]
@@kV:   DD      @@ku00, @@ku01, @@ku02, @@ku03
        DD      @@ku04, @@ku05, @@ku06, @@ku07
        DD      @@ku08, @@ku09, @@ku10, @@ku11
        DD      @@ku12, @@ku13, @@ku14, @@ku15
@@ku15: NOT     DWORD PTR [EAX+56]
@@ku14: NOT     DWORD PTR [EAX+52]
@@ku13: NOT     DWORD PTR [EAX+48]
@@ku12: NOT     DWORD PTR [EAX+44]
@@ku11: NOT     DWORD PTR [EAX+40]
@@ku10: NOT     DWORD PTR [EAX+36]
@@ku09: NOT     DWORD PTR [EAX+32]
@@ku08: NOT     DWORD PTR [EAX+28]
@@ku07: NOT     DWORD PTR [EAX+24]
@@ku06: NOT     DWORD PTR [EAX+20]
@@ku05: NOT     DWORD PTR [EAX+16]
@@ku04: NOT     DWORD PTR [EAX+12]
@@ku03: NOT     DWORD PTR [EAX+8]
@@ku02: NOT     DWORD PTR [EAX+4]
@@ku01: NOT     DWORD PTR [EAX]
@@ku00:
end;

procedure Q_ANDNOTLongs(Dest, Source: Pointer; Count: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,ECX
        AND     EBX,7
        SHR     ECX,3
        JE      @@nx
@@lp:   MOV     ESI,[EDX]
        NOT     ESI
        AND     [EAX],ESI
        MOV     ESI,[EDX+4]
        NOT     ESI
        AND     [EAX+4],ESI
        MOV     ESI,[EDX+8]
        NOT     ESI
        AND     [EAX+8],ESI
        MOV     ESI,[EDX+12]
        NOT     ESI
        AND     [EAX+12],ESI
        MOV     ESI,[EDX+16]
        NOT     ESI
        AND     [EAX+16],ESI
        MOV     ESI,[EDX+20]
        NOT     ESI
        AND     [EAX+20],ESI
        MOV     ESI,[EDX+24]
        NOT     ESI
        AND     [EAX+24],ESI
        MOV     ESI,[EDX+28]
        NOT     ESI
        AND     [EAX+28],ESI
        ADD     EDX,32
        ADD     EAX,32
        DEC     ECX
        JNE     @@lp
@@nx:   JMP     DWORD PTR @@kV[EBX*4]
@@kV:   DD      @@ku00, @@ku01, @@ku02, @@ku03
        DD      @@ku04, @@ku05, @@ku06, @@ku07
@@ku07: MOV     ESI,[EDX+24]
        NOT     ESI
        AND     [EAX+24],ESI
@@ku06: MOV     ESI,[EDX+20]
        NOT     ESI
        AND     [EAX+20],ESI
@@ku05: MOV     ESI,[EDX+16]
        NOT     ESI
        AND     [EAX+16],ESI
@@ku04: MOV     ESI,[EDX+12]
        NOT     ESI
        AND     [EAX+12],ESI
@@ku03: MOV     ESI,[EDX+8]
        NOT     ESI
        AND     [EAX+8],ESI
@@ku02: MOV     ESI,[EDX+4]
        NOT     ESI
        AND     [EAX+4],ESI
@@ku01: MOV     ESI,[EDX]
        NOT     ESI
        AND     [EAX],ESI
@@ku00: POP     ESI
        POP     EBX
end;

procedure Q_LShift1Longs(P: Pointer; Count: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        LEA     EBX,[EAX+EDX*4-8]
        DEC     EDX
        JS      @@qt
        MOV     ESI,[EBX+4]
        MOV     ECX,EDX
        AND     EDX,3
        SHL     ESI,1
        SHR     ECX,2
        JE      @@nx
@@lp:   MOV     EAX,[EBX]
        SHL     EAX,1
        ADC     ESI,0
        MOV     [EBX+4],ESI
        MOV     ESI,[EBX-4]
        SHL     ESI,1
        ADC     EAX,0
        MOV     [EBX],EAX
        MOV     EAX,[EBX-8]
        SHL     EAX,1
        ADC     ESI,0
        MOV     [EBX-4],ESI
        MOV     ESI,[EBX-12]
        SHL     ESI,1
        ADC     EAX,0
        MOV     [EBX-8],EAX
        SUB     EBX,16
        DEC     ECX
        JNE     @@lp
@@nx:   JMP     DWORD PTR @@kV[EDX*4]
@@kV:   DD      @@mv0, @@mv1, @@mv2, @@mv3
@@mv3:  MOV     EAX,[EBX]
        SHL     EAX,1
        ADC     ESI,0
        MOV     [EBX+4],ESI
        MOV     ESI,[EBX-4]
        SHL     ESI,1
        ADC     EAX,0
        MOV     [EBX],EAX
        SHL     DWORD PTR [EBX-8],1
        ADC     ESI,0
        MOV     [EBX-4],ESI
@@qt:   POP     ESI
        POP     EBX
        RET
@@mv2:  MOV     EAX,[EBX]
        SHL     EAX,1
        ADC     ESI,0
        MOV     [EBX+4],ESI
        SHL     DWORD PTR [EBX-4],1
        ADC     EAX,0
        MOV     [EBX],EAX
        POP     ESI
        POP     EBX
        RET
@@mv1:  SHL     DWORD PTR [EBX],1
        ADC     ESI,0
@@mv0:  MOV     [EBX+4],ESI
        POP     ESI
        POP     EBX
end;

procedure Q_RShift1Longs(P: Pointer; Count: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        LEA     EAX,[EAX+EDX*4-4]
        XOR     EBX,EBX
        MOV     ECX,EDX
        AND     EDX,3
        SHR     ECX,2
        JE      @@nx
@@lp:   MOV     ESI,[EAX]
        SHR     EBX,1
        RCR     ESI,1
        MOV     [EAX],ESI
        MOV     ESI,[EAX-4]
        RCR     ESI,1
        MOV     [EAX-4],ESI
        MOV     ESI,[EAX-8]
        RCR     ESI,1
        MOV     [EAX-8],ESI
        MOV     ESI,[EAX-12]
        RCR     ESI,1
        MOV     [EAX-12],ESI
        ADC     EBX,0
        SUB     EAX,16
        DEC     ECX
        JNE     @@lp
@@nx:   JMP     DWORD PTR @@kV[EDX*4]
@@kV:   DD      @@mv0, @@mv1, @@mv2, @@mv3
@@mv0:  POP     ESI
        POP     EBX
        RET
@@mv1:  MOV     ESI,[EAX]
        SHR     EBX,1
        RCR     ESI,1
        MOV     [EAX],ESI
        POP     ESI
        POP     EBX
        RET
@@mv2:  MOV     ESI,[EAX]
        SHR     EBX,1
        RCR     ESI,1
        MOV     [EAX],ESI
        MOV     ESI,[EAX-4]
        RCR     ESI,1
        MOV     [EAX-4],ESI
        POP     ESI
        POP     EBX
        RET
@@mv3:  MOV     ESI,[EAX]
        SHR     EBX,1
        RCR     ESI,1
        MOV     [EAX],ESI
        MOV     ESI,[EAX-4]
        RCR     ESI,1
        MOV     [EAX-4],ESI
        MOV     ESI,[EAX-8]
        RCR     ESI,1
        MOV     [EAX-8],ESI
        POP     ESI
        POP     EBX
end;

const
  RevBits: array[0..255] of Byte =
    ($00,$80,$40,$C0,$20,$A0,$60,$E0,$10,$90,$50,$D0,$30,$B0,$70,$F0,
     $08,$88,$48,$C8,$28,$A8,$68,$E8,$18,$98,$58,$D8,$38,$B8,$78,$F8,
     $04,$84,$44,$C4,$24,$A4,$64,$E4,$14,$94,$54,$D4,$34,$B4,$74,$F4,
     $0C,$8C,$4C,$CC,$2C,$AC,$6C,$EC,$1C,$9C,$5C,$DC,$3C,$BC,$7C,$FC,
     $02,$82,$42,$C2,$22,$A2,$62,$E2,$12,$92,$52,$D2,$32,$B2,$72,$F2,
     $0A,$8A,$4A,$CA,$2A,$AA,$6A,$EA,$1A,$9A,$5A,$DA,$3A,$BA,$7A,$FA,
     $06,$86,$46,$C6,$26,$A6,$66,$E6,$16,$96,$56,$D6,$36,$B6,$76,$F6,
     $0E,$8E,$4E,$CE,$2E,$AE,$6E,$EE,$1E,$9E,$5E,$DE,$3E,$BE,$7E,$FE,
     $01,$81,$41,$C1,$21,$A1,$61,$E1,$11,$91,$51,$D1,$31,$B1,$71,$F1,
     $09,$89,$49,$C9,$29,$A9,$69,$E9,$19,$99,$59,$D9,$39,$B9,$79,$F9,
     $05,$85,$45,$C5,$25,$A5,$65,$E5,$15,$95,$55,$D5,$35,$B5,$75,$F5,
     $0D,$8D,$4D,$CD,$2D,$AD,$6D,$ED,$1D,$9D,$5D,$DD,$3D,$BD,$7D,$FD,
     $03,$83,$43,$C3,$23,$A3,$63,$E3,$13,$93,$53,$D3,$33,$B3,$73,$F3,
     $0B,$8B,$4B,$CB,$2B,$AB,$6B,$EB,$1B,$9B,$5B,$DB,$3B,$BB,$7B,$FB,
     $07,$87,$47,$C7,$27,$A7,$67,$E7,$17,$97,$57,$D7,$37,$B7,$77,$F7,
     $0F,$8F,$4F,$CF,$2F,$AF,$6F,$EF,$1F,$9F,$5F,$DF,$3F,$BF,$7F,$FF);

procedure Q_ReverseBits(P: Pointer; BitCount: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EDX
        PUSH    EDI
        SHR     EBX,3
        AND     EDX,$7
        JE      @@nx1
        INC     EBX
        MOV     ECX,$8
        MOV     EDI,EAX
        PUSH    EAX
        SUB     ECX,EDX
        XOR     EDX,EDX
        PUSH    EBX
@@lp1:  MOVZX   EAX,BYTE PTR [EDI]
        SHL     EAX,CL
        OR      EAX,EDX
        MOV     BYTE PTR [EDI],AL
        MOVZX   EDX,AH
        INC     EDI
        DEC     EBX
        JNE     @@lp1
        POP     EBX
        POP     EAX
@@nx1:  LEA     ECX,[EAX+EBX-1]
@@lp2:  CMP     EAX,ECX
        JGE     @@qt
        MOVZX   ESI,BYTE PTR [EAX]
        MOVZX   EDI,BYTE PTR [ECX]
        MOV     DH,BYTE PTR [ESI+RevBits]
        MOV     BYTE PTR [ECX],DH
        MOV     DL,BYTE PTR [EDI+RevBits]
        MOV     BYTE PTR [EAX],DL
        INC     EAX
        DEC     ECX
        JMP     @@lp2
@@qt:   POP     EDI
        POP     ESI
        POP     EBX
end;

function Q_Lrot32(D: LongWord; Shift: Byte): LongWord;
asm
        MOV     CL,DL
        ROL     EAX,CL
end;

function Q_Rrot32(D: LongWord; Shift: Byte): LongWord;
asm
        MOV     CL,DL
        ROR     EAX,CL
end;

function Q_Lrot16(W: Word; Shift: Byte): Word;
asm
        MOV     CL,DL
        ROL     AX,CL
end;

function Q_Rrot16(W: Word; Shift: Byte): Word;
asm
        MOV     CL,DL
        ROR     AX,CL
end;

function Q_Lrot8(B, Shift: Byte): Byte;
asm
        MOV     CL,DL
        ROL     AL,CL
end;

function Q_Rrot8(B, Shift: Byte): Byte;
asm
        MOV     CL,DL
        ROR     AL,CL
end;

procedure Q_RotateLongsLeft(P: Pointer; Count: Cardinal; Shift: Byte);
asm
        DEC     EDX
        JS      @@qt
@@lp:   ROL     DWORD PTR [EAX+EDX*4],CL
        DEC     EDX
        JNS     @@lp
@@qt:
end;

procedure Q_RotateLongsRight(P: Pointer; Count: Cardinal; Shift: Byte);
asm
        DEC     EDX
        JS      @@qt
@@lp:   ROR     DWORD PTR [EAX+EDX*4],CL
        DEC     EDX
        JNS     @@lp
@@qt:
end;

procedure Q_RotateWordsLeft(P: Pointer; Count: Cardinal; Shift: Byte);
asm
        DEC     EDX
        JS      @@qt
@@lp:   ROL     WORD PTR [EAX+EDX*2],CL
        DEC     EDX
        JNS     @@lp
@@qt:
end;

procedure Q_RotateWordsRight(P: Pointer; Count: Cardinal; Shift: Byte);
asm
        DEC     EDX
        JS      @@qt
@@lp:   ROR     WORD PTR [EAX+EDX*2],CL
        DEC     EDX
        JNS     @@lp
@@qt:
end;

procedure Q_RotateBytesLeft(P: Pointer; L: Cardinal; Shift: Byte);
asm
        DEC     EDX
        JS      @@qt
@@lp:   ROL     BYTE PTR [EAX+EDX],CL
        DEC     EDX
        JNS     @@lp
@@qt:
end;

procedure Q_RotateBytesRight(P: Pointer; L: Cardinal; Shift: Byte);
asm
        DEC     EDX
        JS      @@qt
@@lp:   ROR     BYTE PTR [EAX+EDX],CL
        DEC     EDX
        JNS     @@lp
@@qt:
end;

procedure IntRotateBitsLeft(P: Pointer; L: Cardinal; Shift: Byte);
asm
        TEST    EDX,EDX
        JE      @@qt
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EBX
        PUSH    EDI
        MOV     EBX,EDX
        MOV     EDI,EAX
        MOVZX   EDX,BYTE PTR [EDI+EBX-1]
        SHL     EDX,CL
        MOV     AL,DH
@@lp:   MOVZX   EDX,BYTE PTR [EDI]
        SHL     EDX,CL
        OR      DL,AL
        MOV     AL,DH
        MOV     BYTE PTR [EDI],DL
        INC     EDI
        DEC     EBX
        JNE     @@lp
        POP     EDI
        POP     EBX
@@qt:
end;

procedure IntRotateBitsRight(P: Pointer; L: Cardinal; Shift: Byte);
asm
        TEST    EDX,EDX
        JE      @@qt
        TEST    ECX,ECX
        JE      @@qt
        DEC     EDX
        JE      @@ob
        PUSH    EDI
        PUSH    EBX
        MOV     EDI,EAX
        MOV     EBX,EDX
        XOR     EAX,EAX
        MOV     AH,BYTE PTR [EDI]
        SHR     EAX,CL
        MOV     BYTE PTR [EDI],AH
        MOV     DL,AL
@@lp:   XOR     EAX,EAX
        MOV     AH,BYTE PTR [EDI+EBX]
        SHR     EAX,CL
        OR      AH,DL
        MOV     DL,AL
        MOV     BYTE PTR [EDI+EBX],AH
        DEC     EBX
        JNE     @@lp
        OR      DL,BYTE PTR [EDI]
        MOV     BYTE PTR [EDI],DL
        POP     EBX
        POP     EDI
        RET
@@ob:   ROR     BYTE PTR [EAX],CL
@@qt:
end;

procedure IntBtRtLeft(P: Pointer; L, Shift: Integer);
begin
  if Shift <= 4 then
    IntShortRtRight(P,Shift,L)
  else if Shift <= 256 then
    IntMediumRtRight(P,Shift,L)
  else
    IntLongRotateStr(P,L-Shift,L);
end;

procedure IntBtRtRight(P: Pointer; L, Shift: Integer);
begin
  if Shift <= 4 then
    IntShortRtLeft(P,Shift,L)
  else if Shift <= 256 then
    IntMediumRtLeft(P,Shift,L)
  else
    IntLongRotateStr(P,Shift,L);
end;

procedure Q_RotateBitsLeft(P: Pointer; L: Cardinal; Shift: Integer);
var
  BL,SS: Integer;
begin
  if Integer(L) > 0 then
  begin
    BL := L shl 3;
    Shift := Shift mod BL;
    if Shift < 0 then
      Inc(Shift,BL);
    SS := Shift shr 3;
    if SS > 0 then
    begin
      if SS <= Integer(L) shr 1 then
        IntBtRtLeft(P,L,SS)
      else
        IntBtRtRight(P,L,Integer(L)-SS);
    end;
    IntRotateBitsLeft(P,L,Shift and 7);
  end;
end;

procedure Q_RotateBitsRight(P: Pointer; L: Cardinal; Shift: Integer);
var
  BL,SS: Integer;
begin
  if Integer(L) <> 0 then
  begin
    BL := L shl 3;
    Shift := Shift mod BL;
    if Shift < 0 then
      Inc(Shift,BL);
    SS := Shift shr 3;
    if SS > 0 then
    begin
      if SS <= Integer(L) shr 1 then
        IntBtRtRight(P,L,SS)
      else
        IntBtRtLeft(P,L,Integer(L)-SS);
    end;
    IntRotateBitsRight(P,L,Shift and 7);
  end;
end;

procedure Q_XORByChar(P: Pointer; L: Cardinal; Ch: Char); overload;
asm
        PUSH    EBX
        MOV     EBX,EAX
        MOVZX   EAX,CL
@@lp1:  TEST    EBX,3
        JE      @@nx1
        DEC     EDX
        JS      @@qt
        XOR     BYTE PTR [EBX],CL
        INC     EBX
        JMP     @@lp1
@@nx1:  MOV     ECX,EAX
        SHL     EAX,8
        ADD     EAX,ECX
        MOV     ECX,EAX
        SHL     EAX,16
        ADD     EAX,ECX
        MOV     ECX,EDX
        AND     EDX,7
        SHR     ECX,3
        JE      @@nx2
@@lp2:  XOR     [EBX],EAX
        XOR     [EBX+4],EAX
        ADD     EBX,8
        DEC     ECX
        JNE     @@lp2
@@nx2:  DEC     EDX
        JS      @@qt
        XOR     BYTE PTR [EBX+EDX],AL
        JMP     @@nx2
@@qt:   POP     EBX
end;

procedure Q_XORByChar(P: Pointer; L: Cardinal; Ch: Byte); overload;
asm
        PUSH    EBX
        MOV     EBX,EAX
        MOVZX   EAX,CL
@@lp1:  TEST    EBX,3
        JE      @@nx1
        DEC     EDX
        JS      @@qt
        XOR     BYTE PTR [EBX],CL
        INC     EBX
        JMP     @@lp1
@@nx1:  MOV     ECX,EAX
        SHL     EAX,8
        ADD     EAX,ECX
        MOV     ECX,EAX
        SHL     EAX,16
        ADD     EAX,ECX
        MOV     ECX,EDX
        AND     EDX,7
        SHR     ECX,3
        JE      @@nx2
@@lp2:  XOR     [EBX],EAX
        XOR     [EBX+4],EAX
        ADD     EBX,8
        DEC     ECX
        JNE     @@lp2
@@nx2:  DEC     EDX
        JS      @@qt
        XOR     BYTE PTR [EBX+EDX],AL
        JMP     @@nx2
@@qt:   POP     EBX
end;

procedure Q_XORByLong(P: Pointer; Count: Cardinal; D: LongWord);
asm
        DEC     EDX
        JS      @@qt
@@lp:   XOR     DWORD PTR [EAX+EDX*4],ECX
        DEC     EDX
        JNS     @@lp
@@qt:
end;

procedure Q_XORByWord(P: Pointer; Count: Cardinal; W: Word);
asm
        DEC     EDX
        JS      @@qt
@@lp:   XOR     WORD PTR [EAX+EDX*2],CX
        DEC     EDX
        JNS     @@lp
@@qt:
end;

procedure Q_XORByStr(P: Pointer; L: Cardinal; const Key: string);
asm
        TEST    EDX,EDX
        JE      @@qt
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,[ECX-4]
        TEST    EBX,EBX
        JE      @@qt1
        LEA     EDI,[EAX-1]
        LEA     ESI,[ECX-1]
@@lp1:  CMP     EDX,EBX
        JL      @@ls
        MOV     ECX,EBX
@@lp2:  MOV     AL,BYTE PTR [ESI+ECX]
        XOR     BYTE PTR [EDI+ECX],AL
        DEC     ECX
        JNE     @@lp2
        ADD     EDI,EBX
        SUB     EDX,EBX
        JNE     @@lp1
@@qt1:  POP     EDI
        POP     ESI
        POP     EBX
@@qt:   RET
@@ls:   MOV     ECX,EDX
        MOV     EBX,EDX
        JMP     @@lp2
end;

procedure Q_XORByRandom(P: Pointer; L: Cardinal; Seed: LongWord);
asm
        PUSH    ESI
        DEC     EDX
        JS      @@qt
        MOV     ESI,$8088405
        PUSH    EBX
@@lp:   IMUL    ECX,ESI
        INC     ECX
        MOV     EBX,ECX
        SHR     EBX,24
        XOR     BYTE PTR [EAX+EDX],BL
        DEC     EDX
        JNS     @@lp
        POP     EBX
@@qt:   POP     ESI
end;

type
  PRC4Data = ^TRC4Data;
  TRC4Data = record
    State: array[0..255] of Byte;
    X,Y: Byte;
  end;

procedure IntRC4Init(PSt, Key: Pointer; KeyLen: Cardinal);
asm
        PUSH    EBX
        PUSH    EDI
        MOV     EBX,$03020100
        PUSH    ESI
        MOV     EDI,EAX
        MOV     ESI,8
@@lp1:  MOV     [EAX],EBX
        ADD     EBX,$04040404
        MOV     [EAX+4],EBX
        ADD     EBX,$04040404
        MOV     [EAX+8],EBX
        ADD     EBX,$04040404
        MOV     [EAX+12],EBX
        ADD     EBX,$04040404
        MOV     [EAX+16],EBX
        ADD     EBX,$04040404
        MOV     [EAX+20],EBX
        ADD     EBX,$04040404
        MOV     [EAX+24],EBX
        ADD     EBX,$04040404
        MOV     [EAX+28],EBX
        ADD     EBX,$04040404
        ADD     EAX,32
        DEC     ESI
        JNE     @@lp1
        DEC     ECX
        JS      @@qt
        PUSH    EBP
        XOR     EAX,EAX
        MOV     EBP,$100
        PUSH    EDX
        PUSH    ECX
@@lp2:  MOV     BL,BYTE PTR [EDI+ESI]
        ADD     AL,BYTE PTR [EDX]
        ADD     AL,BL
        MOVZX   EAX,AL
        MOV     BH,BYTE PTR [EDI+EAX]
        MOV     BYTE PTR [EDI+EAX],BL
        MOV     BYTE PTR [EDI+ESI],BH
        INC     EDX
        DEC     ECX
        JS      @@me
        INC     ESI
        DEC     EBP
        JNE     @@lp2
@@nx:   MOV     [ESP],EBP
        POP     ECX
        POP     ECX
        POP     EBP
@@qt:   POP     ESI
        POP     EDI
        POP     EBX
        RET
@@me:   MOV     ECX,[ESP]
        MOV     EDX,[ESP+4]
        INC     ESI
        DEC     EBP
        JNE     @@lp2
        JMP     @@nx
end;

procedure Q_RC4Init(var ID: TRC4ID; Key: Pointer; KeyLen: Cardinal);
begin
  GetMem(PRC4Data(ID),SizeOf(TRC4Data));
  with PRC4Data(ID)^ do
  begin
    IntRC4Init(@State,Key,KeyLen);
    X := 0;
    Y := 0;
  end;
end;

procedure Q_RC4Apply(ID: TRC4ID; P: Pointer; L: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EDI
        LEA     ESI,[EDX-1]
        PUSH    EAX
        XOR     EBX,EBX
        MOV     BL,BYTE PTR [EAX+256]
        MOV     EDI,EAX
        XOR     EDX,EDX
        MOV     DL,BYTE PTR [EAX+257]
@@lp:   INC     EBX
        AND     EBX,$FF
        MOVZX   EAX,BYTE PTR [EDI+EBX]
        ADD     EDX,EAX
        AND     EDX,$FF
        MOV     AH,BYTE PTR [EDI+EDX]
        MOV     BYTE PTR [EDI+EDX],AL
        ADD     AL,AH
        INC     ESI
        MOV     BYTE PTR [EDI+EBX],AH
        MOVZX   EAX,AL
        MOV     AL,BYTE PTR [EDI+EAX]
        XOR     BYTE PTR [ESI],AL
        DEC     ECX
        JNE     @@lp
        POP     EAX
        MOV     [EAX+256],BL
        MOV     [EAX+257],DL
        POP     EDI
@@qt:   POP     ESI
        POP     EBX
end;

procedure IntRC4Clear(ID: TRC4ID);
asm
        XOR     EDX,EDX
        CALL    IntFill32
        ADD     EAX,128
        CALL    IntFill32
        MOV     WORD PTR [EAX+128],DX
end;

procedure Q_RC4Done(ID: TRC4ID);
begin
  IntRC4Clear(ID);
  FreeMem(PRC4Data(ID));
end;

function Q_RC4SelfTest: Boolean;
const
  PlainText: string = 'DCEE4CF92C';
  UserKey: string = '618A63D2FB';
  CipherText: string = 'F13829C9DE';
var
  ID: TRC4ID;
  S,K: string;
begin
  S := Q_CodesToStr(PlainText);
  K := Q_CodesToStr(UserKey);
  Q_RC4Init(ID,Pointer(K),Length(K));
  Q_RC4Apply(ID,Pointer(S),Length(S));
  Q_RC4Done(ID);
  if not Q_SameStr(Q_StrToCodes(S),CipherText) then
  begin
    Result := False;
    Exit;
  end;
  Q_RC4Init(ID,Pointer(K),Length(K));
  Q_RC4Apply(ID,Pointer(S),Length(S));
  Q_RC4Done(ID);
  Result := Q_SameStr(Q_StrToCodes(S),PlainText);
end;

type
  PRC6Data = ^TRC6Data;
  TRC6Data = record
    Vec: TRC6InitVector;
    KeyData: array[0..43] of LongWord;
  end;

const
  RC6_IndShift: array[0..43] of LongWord =
    (4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,
     104,108,112,116,120,124,128,132,136,140,144,148,152,156,160,164,168,172,0);

procedure IntRC6Init(ID: TRC6ID; Key: Pointer; KeyLen: Cardinal);
asm
        PUSH    EBX
        PUSH    EBP
        PUSH    ESI
        XOR     EBP,EBP
        PUSH    EDI
        MOV     [EAX],EBP
        MOV     [EAX+4],EBP
        MOV     [EAX+8],EBP
        MOV     [EAX+12],EBP
        LEA     EBX,[EAX+16]
        TEST    ECX,ECX
        JE      @@mz
        SUB     ESP,$100
        MOV     ESI,ECX
        MOV     EBP,ECX
        MOV     EDI,ESP
        SHR     ESI,2
	MOV	EAX,ESI
        JE      @@nnx
@@ff:   MOV     ECX,[EDX+ESI*4-4]
        MOV     [EDI+ESI*4-4],ECX
        DEC     ESI
        JNE     @@ff
@@nnx:  AND     EBP,3
        XOR     ECX,ECX
        JMP     DWORD PTR @@tV[EBP*4]
@@tV:   DD      @@pe, @@t1, @@t2, @@t3
@@t3:   MOVZX   ECX,BYTE PTR [EDX+EAX*4+2]
@@t2:   SHL     ECX,8
        MOVZX   EBP,BYTE PTR [EDX+EAX*4+1]
        OR      ECX,EBP
@@t1:   SHL     ECX,8
        MOVZX   EBP,BYTE PTR [EDX+EAX*4]
        OR      ECX,EBP
        MOV     [EDI+EAX*4],ECX
        INC     EAX
@@pe:   MOV     ESI,EBX
        MOV     ECX,11
        MOV     EDX,$B7E15163
@@lp1:  MOV     [ESI],EDX
        ADD     EDX,$9E3779B9
        MOV     [ESI+4],EDX
        ADD     EDX,$9E3779B9
        MOV     [ESI+8],EDX
        ADD     EDX,$9E3779B9
        MOV     [ESI+12],EDX
        ADD     EDX,$9E3779B9
        ADD     ESI,16
        DEC     ECX
        JNE     @@lp1
        CMP     EAX,44
        JA      @@me
        MOV     EBP,132
        SHL     EAX,2
@@nx:   ADD     EAX,EDI
        PUSH    EAX
        XOR     EAX,EAX
        PUSH    EAX
        PUSH    EAX
        MOV     ESI,ESP
@@lp2:  MOV     ECX,[EBX+EAX]
        ADD     ECX,[ESI]
        ADD     ECX,[ESI+4]
        ROL     ECX,3
        MOV     [ESI+4],ECX
        MOV     [EBX+EAX],ECX
        ADD     ECX,[ESI]
        MOV     EDX,ECX
        ADD     EDX,[EDI]
        ROL     EDX,CL
        MOV     [EDI],EDX
        MOV     [ESI],EDX
        MOV     EAX,DWORD PTR [EAX+RC6_IndShift]
        ADD     EDI,4
        CMP     EDI,[ESI+8]
        JE      @@jx
@@nj:   DEC     EBP
        JNE     @@lp2
        MOV     EAX,ESP
        XOR     EDX,EDX
        MOV     [EAX],EDX
        MOV     [EAX+4],EDX
        MOV     [EAX+8],EDX
        ADD     EAX,12
        CALL    IntFill32
        ADD     EAX,128
        CALL    IntFill32
        ADD     ESP,$10C
@@k2:   POP     EDI
        POP     ESI
        POP     EBP
        POP     EBX
        RET
@@me:   MOV     EBP,EAX
        ADD     EBP,EAX
        ADD     EBP,EAX
        JMP     @@nx
@@jx:   LEA     EDI,[ESP+12]
        JMP     @@nj
@@mz:   MOV     ECX,11
        MOV     EDX,$B7E15163
@@lz:   MOV     [EBX],EDX
        ADD     EDX,$9E3779B9
        MOV     [EBX+4],EDX
        ADD     EDX,$9E3779B9
        MOV     [EBX+8],EDX
        ADD     EDX,$9E3779B9
        MOV     [EBX+12],EDX
        ADD     EDX,$9E3779B9
        ADD     EBX,16
        DEC     ECX
        JNE     @@lz
        JMP     @@k2
end;

procedure Q_RC6Init(var ID: TRC6ID; Key: Pointer; KeyLen: Cardinal);
begin
  GetMem(PRC6Data(ID),SizeOf(TRC6Data));
  if KeyLen <= 255 then
    IntRC6Init(ID,Key,KeyLen)
  else
    IntRC6Init(ID,Key,255);
end;

procedure IntRC6EncryptECB(ID: TRC6ID; P: Pointer);
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        LEA     EBX,[EAX+24]
        PUSH    EBP
        MOV     ESI,[EDX]
        PUSH    EDX
        MOV     EDI,[EDX+4]
        MOV     EBP,[EDX+12]
        ADD     EDI,[EBX-8]
        ADD     EBP,[EBX-4]
        MOV     EDX,[EDX+8]
        LEA     EAX,[EDI+EDI+1]
        IMUL    EAX,EDI
        LEA     ECX,[EBP+EBP+1]
        ROL     EAX,5
        XOR     ESI,EAX
        IMUL    ECX,EBP
        ROL     ECX,5
        XOR     EDX,ECX
        ROL     ESI,CL
        MOV     ECX,EAX
        ROL     EDX,CL
        ADD     ESI,[EBX]
        ADD     EDX,[EBX+4]
        LEA     ECX,[ESI+ESI+1]
        LEA     EAX,[EDX+EDX+1]
        IMUL    EAX,EDX
        ROL     EAX,5
        XOR     EDI,EAX
        IMUL    ECX,ESI
        ROL     ECX,5
        XOR     EBP,ECX
        ROL     EDI,CL
        MOV     ECX,EAX
        ROL     EBP,CL
        ADD     EDI,[EBX+8]
        ADD     EBP,[EBX+12]
        LEA     ECX,[EDI+EDI+1]
        LEA     EAX,[EBP+EBP+1]
        IMUL    EAX,EBP
        ROL     EAX,5
        XOR     EDX,EAX
        IMUL    ECX,EDI
        ROL     ECX,5
        XOR     ESI,ECX
        ROL     EDX,CL
        MOV     ECX,EAX
        ROL     ESI,CL
        ADD     EDX,[EBX+16]
        ADD     ESI,[EBX+20]
        LEA     ECX,[EDX+EDX+1]
        LEA     EAX,[ESI+ESI+1]
        IMUL    EAX,ESI
        ROL     EAX,5
        XOR     EBP,EAX
        IMUL    ECX,EDX
        ROL     ECX,5
        XOR     EDI,ECX
        ROL     EBP,CL
        MOV     ECX,EAX
        ROL     EDI,CL
        ADD     EBP,[EBX+24]
        ADD     EDI,[EBX+28]
        LEA     ECX,[EBP+EBP+1]
        LEA     EAX,[EDI+EDI+1]
        IMUL    EAX,EDI
        ROL     EAX,5
        XOR     ESI,EAX
        IMUL    ECX,EBP
        ROL     ECX,5
        XOR     EDX,ECX
        ROL     ESI,CL
        ADD     ESI,[EBX+32]
        MOV     ECX,EAX
        ROL     EDX,CL
        ADD     EDX,[EBX+36]
        LEA     ECX,[ESI+ESI+1]
        LEA     EAX,[EDX+EDX+1]
        IMUL    EAX,EDX
        ROL     EAX,5
        XOR     EDI,EAX
        IMUL    ECX,ESI
        ROL     ECX,5
        XOR     EBP,ECX
        ROL     EDI,CL
        MOV     ECX,EAX
        ROL     EBP,CL
        ADD     EDI,[EBX+40]
        ADD     EBP,[EBX+44]
        LEA     ECX,[EDI+EDI+1]
        LEA     EAX,[EBP+EBP+1]
        IMUL    EAX,EBP
        ROL     EAX,5
        XOR     EDX,EAX
        IMUL    ECX,EDI
        ROL     ECX,5
        XOR     ESI,ECX
        ROL     EDX,CL
        MOV     ECX,EAX
        ROL     ESI,CL
        ADD     EDX,[EBX+48]
        ADD     ESI,[EBX+52]
        LEA     ECX,[EDX+EDX+1]
        LEA     EAX,[ESI+ESI+1]
        IMUL    EAX,ESI
        ROL     EAX,5
        XOR     EBP,EAX
        IMUL    ECX,EDX
        ROL     ECX,5
        XOR     EDI,ECX
        ROL     EBP,CL
        MOV     ECX,EAX
        ROL     EDI,CL
        ADD     EBP,[EBX+56]
        ADD     EDI,[EBX+60]
        LEA     ECX,[EBP+EBP+1]
        LEA     EAX,[EDI+EDI+1]
        IMUL    EAX,EDI
        ROL     EAX,5
        XOR     ESI,EAX
        IMUL    ECX,EBP
        ROL     ECX,5
        XOR     EDX,ECX
        ROL     ESI,CL
        MOV     ECX,EAX
        ROL     EDX,CL
        ADD     ESI,[EBX+64]
        ADD     EDX,[EBX+68]
        LEA     ECX,[ESI+ESI+1]
        LEA     EAX,[EDX+EDX+1]
        IMUL    EAX,EDX
        ROL     EAX,5
        XOR     EDI,EAX
        IMUL    ECX,ESI
        ROL     ECX,5
        XOR     EBP,ECX
        ROL     EDI,CL
        MOV     ECX,EAX
        ROL     EBP,CL
        ADD     EDI,[EBX+72]
        ADD     EBP,[EBX+76]
        LEA     ECX,[EDI+EDI+1]
        LEA     EAX,[EBP+EBP+1]
        IMUL    EAX,EBP
        ROL     EAX,5
        XOR     EDX,EAX
        IMUL    ECX,EDI
        ROL     ECX,5
        XOR     ESI,ECX
        ROL     EDX,CL
        MOV     ECX,EAX
        ROL     ESI,CL
        ADD     EDX,[EBX+80]
        ADD     ESI,[EBX+84]
        LEA     ECX,[EDX+EDX+1]
        LEA     EAX,[ESI+ESI+1]
        IMUL    EAX,ESI
        ROL     EAX,5
        XOR     EBP,EAX
        IMUL    ECX,EDX
        ROL     ECX,5
        XOR     EDI,ECX
        ROL     EBP,CL
        MOV     ECX,EAX
        ROL     EDI,CL
        ADD     EBP,[EBX+88]
        ADD     EDI,[EBX+92]
        LEA     ECX,[EBP+EBP+1]
        LEA     EAX,[EDI+EDI+1]
        IMUL    EAX,EDI
        ROL     EAX,5
        XOR     ESI,EAX
        IMUL    ECX,EBP
        ROL     ECX,5
        XOR     EDX,ECX
        ROL     ESI,CL
        MOV     ECX,EAX
        ROL     EDX,CL
        ADD     ESI,[EBX+96]
        ADD     EDX,[EBX+100]
        LEA     ECX,[ESI+ESI+1]
        LEA     EAX,[EDX+EDX+1]
        IMUL    EAX,EDX
        ROL     EAX,5
        XOR     EDI,EAX
        IMUL    ECX,ESI
        ROL     ECX,5
        XOR     EBP,ECX
        ROL     EDI,CL
        MOV     ECX,EAX
        ROL     EBP,CL
        ADD     EDI,[EBX+104]
        ADD     EBP,[EBX+108]
        LEA     ECX,[EDI+EDI+1]
        LEA     EAX,[EBP+EBP+1]
        IMUL    EAX,EBP
        ROL     EAX,5
        XOR     EDX,EAX
        IMUL    ECX,EDI
        ROL     ECX,5
        XOR     ESI,ECX
        ROL     EDX,CL
        MOV     ECX,EAX
        ROL     ESI,CL
        ADD     EDX,[EBX+112]
        ADD     ESI,[EBX+116]
        LEA     ECX,[EDX+EDX+1]
        LEA     EAX,[ESI+ESI+1]
        IMUL    EAX,ESI
        ROL     EAX,5
        XOR     EBP,EAX
        IMUL    ECX,EDX
        ROL     ECX,5
        XOR     EDI,ECX
        ROL     EBP,CL
        MOV     ECX,EAX
        ROL     EDI,CL
        ADD     EBP,[EBX+120]
        ADD     EDI,[EBX+124]
        LEA     ECX,[EBP+EBP+1]
        LEA     EAX,[EDI+EDI+1]
        IMUL    EAX,EDI
        ROL     EAX,5
        XOR     ESI,EAX
        IMUL    ECX,EBP
        ROL     ECX,5
        XOR     EDX,ECX
        ROL     ESI,CL
        MOV     ECX,EAX
        ROL     EDX,CL
        ADD     ESI,[EBX+128]
        ADD     EDX,[EBX+132]
        LEA     ECX,[ESI+ESI+1]
        LEA     EAX,[EDX+EDX+1]
        IMUL    EAX,EDX
        ROL     EAX,5
        XOR     EDI,EAX
        IMUL    ECX,ESI
        ROL     ECX,5
        XOR     EBP,ECX
        ROL     EDI,CL
        MOV     ECX,EAX
        ROL     EBP,CL
        ADD     EDI,[EBX+136]
        ADD     EBP,[EBX+140]
        LEA     ECX,[EDI+EDI+1]
        LEA     EAX,[EBP+EBP+1]
        IMUL    EAX,EBP
        ROL     EAX,5
        XOR     EDX,EAX
        IMUL    ECX,EDI
        ROL     ECX,5
        XOR     ESI,ECX
        ROL     EDX,CL
        MOV     ECX,EAX
        ROL     ESI,CL
        ADD     EDX,[EBX+144]
        ADD     ESI,[EBX+148]
        LEA     ECX,[EDX+EDX+1]
        LEA     EAX,[ESI+ESI+1]
        IMUL    EAX,ESI
        ROL     EAX,5
        XOR     EBP,EAX
        IMUL    ECX,EDX
        ROL     ECX,5
        XOR     EDI,ECX
        ROL     EBP,CL
        MOV     ECX,EAX
        ROL     EDI,CL
        ADD     EBP,[EBX+152]
        ADD     EDI,[EBX+156]
        POP     ECX
        ADD     ESI,[EBX+160]
        ADD     EDX,[EBX+164]
        MOV     [ECX+12],EBP
        MOV     [ECX],ESI
        POP     EBP
        MOV     [ECX+4],EDI
        MOV     [ECX+8],EDX
        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure IntRC6DecryptECB(ID: TRC6ID; P: Pointer);
asm
        PUSH    EBX
        PUSH    EBP
        PUSH    ESI
        LEA     EBX,[EAX+24]
        MOV     ESI,[EDX+12]
        PUSH    EDI
        MOV     EBP,[EDX+8]
        MOV     EDI,[EDX]
        PUSH    EDX
        SUB     EBP,[EBX+164]
        SUB     EDI,[EBX+160]
        PUSH    ECX
        MOV     EDX,[EDX+4]
        LEA     EAX,[EBP+EBP+1]
        IMUL    EAX,EBP
        LEA     ECX,[EDI+EDI+1]
        IMUL    ECX,EDI
        SUB     EDX,[EBX+156]
        ROL     EAX,5
        ROL     ECX,5
        SUB     ESI,[EBX+152]
        ROR     EDX,CL
        XCHG    EAX,ECX
        ROR     ESI,CL
        XOR     EDX,ECX
        XOR     ESI,EAX
        LEA     EAX,[EDX+EDX+1]
        IMUL    EAX,EDX
        LEA     ECX,[ESI+ESI+1]
        SUB     EDI,[EBX+148]
        IMUL    ECX,ESI
        ROL     EAX,5
        ROL     ECX,5
        SUB     EBP,[EBX+144]
        ROR     EDI,CL
        XCHG    EAX,ECX
        ROR     EBP,CL
        XOR     EDI,ECX
        XOR     EBP,EAX
        LEA     EAX,[EDI+EDI+1]
        IMUL    EAX,EDI
        LEA     ECX,[EBP+EBP+1]
        SUB     ESI,[EBX+140]
        IMUL    ECX,EBP
        ROL     EAX,5
        ROL     ECX,5
        SUB     EDX,[EBX+136]
        ROR     ESI,CL
        XCHG    EAX,ECX
        ROR     EDX,CL
        XOR     ESI,ECX
        XOR     EDX,EAX
        LEA     EAX,[ESI+ESI+1]
        IMUL    EAX,ESI
        LEA     ECX,[EDX+EDX+1]
        SUB     EBP,[EBX+132]
        IMUL    ECX,EDX
        ROL     EAX,5
        ROL     ECX,5
        SUB     EDI,[EBX+128]
        ROR     EBP,CL
        XCHG    EAX,ECX
        ROR     EDI,CL
        XOR     EBP,ECX
        XOR     EDI,EAX
        LEA     EAX,[EBP+EBP+1]
        IMUL    EAX,EBP
        LEA     ECX,[EDI+EDI+1]
        SUB     EDX,[EBX+124]
        IMUL    ECX,EDI
        ROL     EAX,5
        ROL     ECX,5
        SUB     ESI,[EBX+120]
        ROR     EDX,CL
        XCHG    EAX,ECX
        ROR     ESI,CL
        XOR     EDX,ECX
        XOR     ESI,EAX
        LEA     EAX,[EDX+EDX+1]
        IMUL    EAX,EDX
        LEA     ECX,[ESI+ESI+1]
        SUB     EDI,[EBX+116]
        IMUL    ECX,ESI
        ROL     EAX,5
        ROL     ECX,5
        SUB     EBP,[EBX+112]
        ROR     EDI,CL
        XCHG    EAX,ECX
        ROR     EBP,CL
        XOR     EDI,ECX
        XOR     EBP,EAX
        LEA     EAX,[EDI+EDI+1]
        IMUL    EAX,EDI
        LEA     ECX,[EBP+EBP+1]
        SUB     ESI,[EBX+108]
        IMUL    ECX,EBP
        ROL     EAX,5
        ROL     ECX,5
        SUB     EDX,[EBX+104]
        ROR     ESI,CL
        XCHG    EAX,ECX
        ROR     EDX,CL
        XOR     ESI,ECX
        XOR     EDX,EAX
        LEA     EAX,[ESI+ESI+1]
        IMUL    EAX,ESI
        LEA     ECX,[EDX+EDX+1]
        SUB     EBP,[EBX+100]
        IMUL    ECX,EDX
        ROL     EAX,5
        ROL     ECX,5
        SUB     EDI,[EBX+96]
        ROR     EBP,CL
        XCHG    EAX,ECX
        ROR     EDI,CL
        XOR     EBP,ECX
        XOR     EDI,EAX
        SUB     EDX,[EBX+92]
        SUB     ESI,[EBX+88]
        LEA     ECX,[EDI+EDI+1]
        IMUL    ECX,EDI
        ROL     ECX,5
        LEA     EAX,[EBP+EBP+1]
        IMUL    EAX,EBP
        ROL     EAX,5
        MOV     [ESP],ECX
        ROR     EDX,CL
        XOR     EDX,EAX
        MOV     ECX,EAX
        ROR     ESI,CL
        XOR     ESI,[ESP]
        LEA     EAX,[EDX+EDX+1]
        IMUL    EAX,EDX
        LEA     ECX,[ESI+ESI+1]
        SUB     EDI,[EBX+84]
        IMUL    ECX,ESI
        ROL     EAX,5
        ROL     ECX,5
        SUB     EBP,[EBX+80]
        ROR     EDI,CL
        XCHG    EAX,ECX
        ROR     EBP,CL
        XOR     EDI,ECX
        XOR     EBP,EAX
        LEA     EAX,[EDI+EDI+1]
        IMUL    EAX,EDI
        LEA     ECX,[EBP+EBP+1]
        SUB     ESI,[EBX+76]
        IMUL    ECX,EBP
        ROL     EAX,5
        ROL     ECX,5
        SUB     EDX,[EBX+72]
        ROR     ESI,CL
        XCHG    EAX,ECX
        ROR     EDX,CL
        XOR     ESI,ECX
        XOR     EDX,EAX
        LEA     EAX,[ESI+ESI+1]
        IMUL    EAX,ESI
        LEA     ECX,[EDX+EDX+1]
        SUB     EBP,[EBX+68]
        IMUL    ECX,EDX
        ROL     EAX,5
        ROL     ECX,5
        SUB     EDI,[EBX+64]
        ROR     EBP,CL
        XCHG    EAX,ECX
        ROR     EDI,CL
        XOR     EBP,ECX
        XOR     EDI,EAX
        LEA     EAX,[EBP+EBP+1]
        IMUL    EAX,EBP
        LEA     ECX,[EDI+EDI+1]
        SUB     EDX,[EBX+60]
        IMUL    ECX,EDI
        ROL     EAX,5
        ROL     ECX,5
        SUB     ESI,[EBX+56]
        ROR     EDX,CL
        XCHG    EAX,ECX
        ROR     ESI,CL
        XOR     EDX,ECX
        XOR     ESI,EAX
        LEA     EAX,[EDX+EDX+1]
        IMUL    EAX,EDX
        LEA     ECX,[ESI+ESI+1]
        SUB     EDI,[EBX+52]
        IMUL    ECX,ESI
        ROL     EAX,5
        ROL     ECX,5
        SUB     EBP,[EBX+48]
        ROR     EDI,CL
        XCHG    EAX,ECX
        ROR     EBP,CL
        XOR     EDI,ECX
        XOR     EBP,EAX
        LEA     EAX,[EDI+EDI+1]
        IMUL    EAX,EDI
        LEA     ECX,[EBP+EBP+1]
        SUB     ESI,[EBX+44]
        IMUL    ECX,EBP
        ROL     EAX,5
        ROL     ECX,5
        SUB     EDX,[EBX+40]
        ROR     ESI,CL
        XCHG    EAX,ECX
        ROR     EDX,CL
        XOR     ESI,ECX
        XOR     EDX,EAX
        LEA     EAX,[ESI+ESI+1]
        IMUL    EAX,ESI
        LEA     ECX,[EDX+EDX+1]
        SUB     EBP,[EBX+36]
        IMUL    ECX,EDX
        ROL     EAX,5
        ROL     ECX,5
        SUB     EDI,[EBX+32]
        ROR     EBP,CL
        XCHG    EAX,ECX
        ROR     EDI,CL
        XOR     EBP,ECX
        XOR     EDI,EAX
        LEA     EAX,[EBP+EBP+1]
        IMUL    EAX,EBP
        LEA     ECX,[EDI+EDI+1]
        SUB     EDX,[EBX+28]
        IMUL    ECX,EDI
        ROL     EAX,5
        ROL     ECX,5
        SUB     ESI,[EBX+24]
        ROR     EDX,CL
        XCHG    EAX,ECX
        ROR     ESI,CL
        XOR     EDX,ECX
        XOR     ESI,EAX
        LEA     EAX,[EDX+EDX+1]
        IMUL    EAX,EDX
        LEA     ECX,[ESI+ESI+1]
        SUB     EDI,[EBX+20]
        IMUL    ECX,ESI
        ROL     EAX,5
        ROL     ECX,5
        SUB     EBP,[EBX+16]
        ROR     EDI,CL
        XCHG    EAX,ECX
        ROR     EBP,CL
        XOR     EDI,ECX
        XOR     EBP,EAX
        LEA     EAX,[EDI+EDI+1]
        IMUL    EAX,EDI
        LEA     ECX,[EBP+EBP+1]
        SUB     ESI,[EBX+12]
        IMUL    ECX,EBP
        ROL     EAX,5
        ROL     ECX,5
        SUB     EDX,[EBX+8]
        ROR     ESI,CL
        XCHG    EAX,ECX
        ROR     EDX,CL
        XOR     ESI,ECX
        XOR     EDX,EAX
        SUB     EBP,[EBX+4]
        SUB     EDI,[EBX]
        LEA     ECX,[EDX+EDX+1]
        IMUL    ECX,EDX
        ROL     ECX,5
        LEA     EAX,[ESI+ESI+1]
        IMUL    EAX,ESI
        ROL     EAX,5
        MOV     [ESP],ECX
        ROR     EBP,CL
        XOR     EBP,EAX
        MOV     ECX,EAX
        ROR     EDI,CL
        POP     EAX
        XOR     EDI,EAX
        SUB     ESI,[EBX-4]
        SUB     EDX,[EBX-8]
        POP     ECX
        MOV     [ECX],EDI
        MOV     [ECX+4],EDX
        POP     EDI
        MOV     [ECX+12],ESI
        MOV     [ECX+8],EBP
        POP     ESI
        POP     EBP
        POP     EBX
end;

procedure Q_RC6SetOrdinaryVector(ID: TRC6ID);
asm
        XOR     EDX,EDX
        MOV     [EAX],EDX
        MOV     [EAX+4],EDX
        MOV     [EAX+8],EDX
        MOV     [EAX+12],EDX
        MOV     EDX,EAX
        CALL    IntRC6EncryptECB
end;

procedure Q_RC6SetInitVector(ID: TRC6ID; const IV: TRC6InitVector);
asm
        MOV     ECX,[EDX]
        MOV     [EAX],ECX
        MOV     ECX,[EDX+4]
        MOV     [EAX+4],ECX
        MOV     ECX,[EDX+8]
        MOV     [EAX+8],ECX
        MOV     ECX,[EDX+12]
        MOV     [EAX+12],ECX
end;

procedure Q_RC6EncryptCBC(ID: TRC6ID; P: Pointer; L: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     ESI,ECX
        PUSH    ECX
        MOV     EDI,EDX
        SHR     ESI,4
        JE      @@nx
@@lp1:  MOV     EAX,[EDI]
        XOR     [EBX],EAX
        MOV     EAX,[EDI+4]
        XOR     [EBX+4],EAX
        MOV     EAX,[EDI+8]
        XOR     [EBX+8],EAX
        MOV     EAX,[EDI+12]
        XOR     [EBX+12],EAX
        MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntRC6EncryptECB
        MOV     EAX,[EBX]
        MOV     [EDI],EAX
        MOV     EAX,[EBX+4]
        MOV     [EDI+4],EAX
        MOV     EAX,[EBX+8]
        MOV     [EDI+8],EAX
        MOV     EAX,[EBX+12]
        MOV     [EDI+12],EAX
        ADD     EDI,$10
        DEC     ESI
        JNE     @@lp1
@@nx:   POP     ESI
        AND     ESI,$F
        JE      @@qt
        MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntRC6EncryptECB
        MOV     ECX,ESI
        SHR     ESI,2
        JE      @@uu
@@lp2:  MOV     EAX,[EBX]
        XOR     EAX,[EDI]
        MOV     [EBX],EAX
        MOV     [EDI],EAX
        ADD     EBX,4
        ADD     EDI,4
        DEC     ESI
        JNE     @@lp2
@@uu:   AND     ECX,3
        JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@qt, @@t1, @@t2, @@t3
@@t3:   MOV     AL,[EBX+2]
        XOR     AL,[EDI+2]
        MOV     [EBX+2],AL
        MOV     [EDI+2],AL
@@t2:   MOV     AL,[EBX+1]
        XOR     AL,[EDI+1]
        MOV     [EBX+1],AL
        MOV     [EDI+1],AL
@@t1:   MOV     AL,[EBX]
        XOR     AL,[EDI]
        MOV     [EBX],AL
        MOV     [EDI],AL
@@qt:   POP     EDI
        POP     ESI
        POP     EBX
end;

procedure Q_RC6DecryptCBC(ID: TRC6ID; P: Pointer; L: Cardinal);
asm
        PUSH    EBX
        PUSH    EBP
        PUSH    ESI
        MOV     EBX,EAX
        PUSH    EDI
        MOV     EBP,ECX
        SUB     ESP,$10
        MOV     EDI,EDX
        MOV     ESI,ESP
        PUSH    ECX
        SHR     EBP,4
        JE      @@nx
@@lp1:  MOV     EAX,[EDI]
        MOV     [ESI],EAX
        MOV     EAX,[EDI+4]
        MOV     [ESI+4],EAX
        MOV     EAX,[EDI+8]
        MOV     [ESI+8],EAX
        MOV     EAX,[EDI+12]
        MOV     [ESI+12],EAX
        MOV     EAX,EBX
        MOV     EDX,EDI
        CALL    IntRC6DecryptECB
        MOV     EAX,[EBX]
        XOR     [EDI],EAX
        MOV     EAX,[EBX+4]
        XOR     [EDI+4],EAX
        MOV     EAX,[EBX+8]
        XOR     [EDI+8],EAX
        MOV     EAX,[EBX+12]
        XOR     [EDI+12],EAX
        MOV     EAX,[ESI]
        MOV     [EBX],EAX
        MOV     EAX,[ESI+4]
        MOV     [EBX+4],EAX
        MOV     EAX,[ESI+8]
        MOV     [EBX+8],EAX
        MOV     EAX,[ESI+12]
        MOV     [EBX+12],EAX
        ADD     EDI,$10
        DEC     EBP
        JNE     @@lp1
@@nx:   XOR     EDX,EDX
        POP     EBP
        MOV     [ESI],EDX
        MOV     [ESI+4],EDX
        MOV     [ESI+8],EDX
        MOV     [ESI+12],EDX
        ADD     ESP,$10
        AND     EBP,$F
        JE      @@qt
        MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntRC6EncryptECB
        MOV     ECX,EBP
        SHR     EBP,2
        JE      @@uu
@@lp2:  MOV     EAX,[EBX]
        MOV     EDX,[EDI]
        XOR     EAX,EDX
        MOV     [EBX],EDX
        MOV     [EDI],EAX
        ADD     EBX,4
        ADD     EDI,4
        DEC     EBP
        JNE     @@lp2
@@uu:   AND     ECX,3
        JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@qt, @@t1, @@t2, @@t3
@@t3:   MOV     AL,[EBX+2]
        MOV     DL,[EDI+2]
        XOR     AL,DL
        MOV     [EBX+2],DL
        MOV     [EDI+2],AL
@@t2:   MOV     AL,[EBX+1]
        MOV     DL,[EDI+1]
        XOR     AL,DL
        MOV     [EBX+1],DL
        MOV     [EDI+1],AL
@@t1:   MOV     AL,[EBX]
        MOV     DL,[EDI]
        XOR     AL,DL
        MOV     [EBX],DL
        MOV     [EDI],AL
@@qt:   POP     EDI
        POP     ESI
        POP     EBP
        POP     EBX
end;

procedure Q_RC6EncryptCFB128(ID: TRC6ID; P: Pointer; L: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     ESI,ECX
        PUSH    ECX
        MOV     EDI,EDX
        SHR     ESI,4
        JE      @@nx
@@lp1:  MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntRC6EncryptECB
        MOV     EAX,[EBX]
        XOR     EAX,[EDI]
        MOV     [EBX],EAX
        MOV     [EDI],EAX
        MOV     EAX,[EBX+4]
        XOR     EAX,[EDI+4]
        MOV     [EBX+4],EAX
        MOV     [EDI+4],EAX
        MOV     EAX,[EBX+8]
        XOR     EAX,[EDI+8]
        MOV     [EBX+8],EAX
        MOV     [EDI+8],EAX
        MOV     EAX,[EBX+12]
        XOR     EAX,[EDI+12]
        MOV     [EBX+12],EAX
        MOV     [EDI+12],EAX
        ADD     EDI,$10
        DEC     ESI
        JNE     @@lp1
@@nx:   POP     ESI
        AND     ESI,$F
        JE      @@qt
        MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntRC6EncryptECB
        MOV     ECX,ESI
        SHR     ESI,2
        JE      @@uu
@@lp2:  MOV     EAX,[EBX]
        XOR     EAX,[EDI]
        MOV     [EBX],EAX
        MOV     [EDI],EAX
        ADD     EBX,4
        ADD     EDI,4
        DEC     ESI
        JNE     @@lp2
@@uu:   AND     ECX,3
        JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@qt, @@t1, @@t2, @@t3
@@t3:   MOV     AL,[EBX+2]
        XOR     AL,[EDI+2]
        MOV     [EBX+2],AL
        MOV     [EDI+2],AL
@@t2:   MOV     AL,[EBX+1]
        XOR     AL,[EDI+1]
        MOV     [EBX+1],AL
        MOV     [EDI+1],AL
@@t1:   MOV     AL,[EBX]
        XOR     AL,[EDI]
        MOV     [EBX],AL
        MOV     [EDI],AL
@@qt:   POP     EDI
        POP     ESI
        POP     EBX
end;

procedure Q_RC6DecryptCFB128(ID: TRC6ID; P: Pointer; L: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     ESI,ECX
        PUSH    ECX
        MOV     EDI,EDX
        SHR     ESI,4
        JE      @@nx
@@lp1:  MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntRC6EncryptECB
        MOV     EAX,[EBX]
        MOV     EDX,[EDI]
        XOR     EAX,EDX
        MOV     [EBX],EDX
        MOV     [EDI],EAX
        MOV     EAX,[EBX+4]
        MOV     EDX,[EDI+4]
        XOR     EAX,EDX
        MOV     [EBX+4],EDX
        MOV     [EDI+4],EAX
        MOV     EAX,[EBX+8]
        MOV     EDX,[EDI+8]
        XOR     EAX,EDX
        MOV     [EBX+8],EDX
        MOV     [EDI+8],EAX
        MOV     EAX,[EBX+12]
        MOV     EDX,[EDI+12]
        XOR     EAX,EDX
        MOV     [EBX+12],EDX
        MOV     [EDI+12],EAX
        ADD     EDI,$10
        DEC     ESI
        JNE     @@lp1
@@nx:   POP     ESI
        AND     ESI,$F
        JE      @@qt
        MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntRC6EncryptECB
        MOV     ECX,ESI
        SHR     ESI,2
        JE      @@uu
@@lp2:  MOV     EAX,[EBX]
        MOV     EDX,[EDI]
        XOR     EAX,EDX
        MOV     [EBX],EDX
        MOV     [EDI],EAX
        ADD     EBX,4
        ADD     EDI,4
        DEC     ESI
        JNE     @@lp2
@@uu:   AND     ECX,3
        JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@qt, @@t1, @@t2, @@t3
@@t3:   MOV     AL,[EBX+2]
        MOV     DL,[EDI+2]
        XOR     AL,DL
        MOV     [EBX+2],DL
        MOV     [EDI+2],AL
@@t2:   MOV     AL,[EBX+1]
        MOV     DL,[EDI+1]
        XOR     AL,DL
        MOV     [EBX+1],DL
        MOV     [EDI+1],AL
@@t1:   MOV     AL,[EBX]
        MOV     DL,[EDI]
        XOR     AL,DL
        MOV     [EBX],DL
        MOV     [EDI],AL
@@qt:   POP     EDI
        POP     ESI
        POP     EBX
end;

procedure Q_RC6EncryptCFB(ID: TRC6ID; P: Pointer; L: Cardinal);
asm
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EBX
        PUSH    EBP
        PUSH    ESI
        MOV     EBX,EAX
        PUSH    EDI
        MOV     EBP,ECX
        SUB     ESP,$10
        MOV     EDI,EDX
        MOV     ESI,ESP
@@lp:   MOV     EAX,[EBX]
        MOV     [ESI],EAX
        MOV     EAX,[EBX+4]
        MOV     [ESI+4],EAX
        MOV     EAX,[EBX+8]
        MOV     [ESI+8],EAX
        MOV     EAX,[EBX+12]
        MOV     [ESI+12],EAX
        MOV     EAX,EBX
        MOV     EDX,ESI
        CALL    IntRC6EncryptECB
        MOV     DL,[EDI]
        XOR     DL,[ESI]
        MOV     [EDI],DL
        MOV     AL,[EBX+1]
        MOV     [EBX],AL
        MOV     AL,[EBX+2]
        MOV     [EBX+1],AL
        MOV     AL,[EBX+3]
        MOV     [EBX+2],AL
        MOV     AL,[EBX+4]
        MOV     [EBX+3],AL
        MOV     AL,[EBX+5]
        MOV     [EBX+4],AL
        MOV     AL,[EBX+6]
        MOV     [EBX+5],AL
        MOV     AL,[EBX+7]
        MOV     [EBX+6],AL
        MOV     AL,[EBX+8]
        MOV     [EBX+7],AL
        MOV     AL,[EBX+9]
        MOV     [EBX+8],AL
        MOV     AL,[EBX+10]
        MOV     [EBX+9],AL
        MOV     AL,[EBX+11]
        MOV     [EBX+10],AL
        MOV     AL,[EBX+12]
        MOV     [EBX+11],AL
        MOV     AL,[EBX+13]
        MOV     [EBX+12],AL
        MOV     AL,[EBX+14]
        MOV     [EBX+13],AL
        MOV     AL,[EBX+15]
        MOV     [EBX+14],AL
        MOV     [EBX+15],DL
        INC     EDI
        DEC     EBP
        JNE     @@lp
        MOV     [ESI],EBP
        MOV     [ESI+4],EBP
        MOV     [ESI+8],EBP
        MOV     [ESI+12],EBP
        ADD     ESP,$10
        POP     EDI
        POP     ESI
        POP     EBP
        POP     EBX
@@qt:
end;

procedure Q_RC6DecryptCFB(ID: TRC6ID; P: Pointer; L: Cardinal);
asm
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EBX
        PUSH    EBP
        PUSH    ESI
        MOV     EBX,EAX
        PUSH    EDI
        MOV     EBP,ECX
        SUB     ESP,$10
        MOV     EDI,EDX
        MOV     ESI,ESP
@@lp:   MOV     EAX,[EBX]
        MOV     [ESI],EAX
        MOV     EAX,[EBX+4]
        MOV     [ESI+4],EAX
        MOV     EAX,[EBX+8]
        MOV     [ESI+8],EAX
        MOV     EAX,[EBX+12]
        MOV     [ESI+12],EAX
        MOV     EAX,EBX
        MOV     EDX,ESI
        CALL    IntRC6EncryptECB
        MOV     DL,[EDI]
        MOV     AL,[ESI]
        XOR     AL,DL
        MOV     [EDI],AL
        MOV     AL,[EBX+1]
        MOV     [EBX],AL
        MOV     AL,[EBX+2]
        MOV     [EBX+1],AL
        MOV     AL,[EBX+3]
        MOV     [EBX+2],AL
        MOV     AL,[EBX+4]
        MOV     [EBX+3],AL
        MOV     AL,[EBX+5]
        MOV     [EBX+4],AL
        MOV     AL,[EBX+6]
        MOV     [EBX+5],AL
        MOV     AL,[EBX+7]
        MOV     [EBX+6],AL
        MOV     AL,[EBX+8]
        MOV     [EBX+7],AL
        MOV     AL,[EBX+9]
        MOV     [EBX+8],AL
        MOV     AL,[EBX+10]
        MOV     [EBX+9],AL
        MOV     AL,[EBX+11]
        MOV     [EBX+10],AL
        MOV     AL,[EBX+12]
        MOV     [EBX+11],AL
        MOV     AL,[EBX+13]
        MOV     [EBX+12],AL
        MOV     AL,[EBX+14]
        MOV     [EBX+13],AL
        MOV     AL,[EBX+15]
        MOV     [EBX+14],AL
        MOV     [EBX+15],DL
        INC     EDI
        DEC     EBP
        JNE     @@lp
        MOV     [ESI],EBP
        MOV     [ESI+4],EBP
        MOV     [ESI+8],EBP
        MOV     [ESI+12],EBP
        ADD     ESP,$10
        POP     EDI
        POP     ESI
        POP     EBP
        POP     EBX
@@qt:
end;

procedure Q_RC6ApplyOFB128(ID: TRC6ID; P: Pointer; L: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     ESI,ECX
        PUSH    ECX
        MOV     EDI,EDX
        SHR     ESI,4
        JE      @@nx
@@lp1:  MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntRC6EncryptECB
        MOV     EAX,[EDI]
        XOR     EAX,[EBX]
        MOV     [EDI],EAX
        MOV     EAX,[EDI+4]
        XOR     EAX,[EBX+4]
        MOV     [EDI+4],EAX
        MOV     EAX,[EDI+8]
        XOR     EAX,[EBX+8]
        MOV     [EDI+8],EAX
        MOV     EAX,[EDI+12]
        XOR     EAX,[EBX+12]
        MOV     [EDI+12],EAX
        ADD     EDI,$10
        DEC     ESI
        JNE     @@lp1
@@nx:   POP     ESI
        AND     ESI,$F
        JE      @@qt
        MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntRC6EncryptECB
        MOV     ECX,ESI
        SHR     ESI,2
        JE      @@uu
@@lp2:  MOV     EAX,[EDI]
        XOR     EAX,[EBX]
        MOV     [EDI],EAX
        ADD     EBX,4
        ADD     EDI,4
        DEC     ESI
        JNE     @@lp2
@@uu:   AND     ECX,3
        JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@qt, @@t1, @@t2, @@t3
@@t3:   MOV     AL,[EDI+2]
        XOR     AL,[EBX+2]
        MOV     [EDI+2],AL
@@t2:   MOV     AL,[EDI+1]
        XOR     AL,[EBX+1]
        MOV     [EDI+1],AL
@@t1:   MOV     AL,[EDI]
        XOR     AL,[EBX]
        MOV     [EDI],AL
@@qt:   POP     EDI
        POP     ESI
        POP     EBX
end;

procedure Q_RC6ApplyOFB(ID: TRC6ID; P: Pointer; L: Cardinal);
asm
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EBX
        PUSH    EBP
        PUSH    ESI
        MOV     EBX,EAX
        PUSH    EDI
        MOV     EBP,ECX
        SUB     ESP,$10
        MOV     EDI,EDX
        MOV     ESI,ESP
@@lp:   MOV     EAX,[EBX]
        MOV     [ESI],EAX
        MOV     EAX,[EBX+4]
        MOV     [ESI+4],EAX
        MOV     EAX,[EBX+8]
        MOV     [ESI+8],EAX
        MOV     EAX,[EBX+12]
        MOV     [ESI+12],EAX
        MOV     EAX,EBX
        MOV     EDX,ESI
        CALL    IntRC6EncryptECB
        MOV     AL,[EDI]
        MOV     DL,[ESI]
        XOR     AL,DL
        MOV     [EDI],AL
        MOV     AL,[EBX+1]
        MOV     [EBX],AL
        MOV     AL,[EBX+2]
        MOV     [EBX+1],AL
        MOV     AL,[EBX+3]
        MOV     [EBX+2],AL
        MOV     AL,[EBX+4]
        MOV     [EBX+3],AL
        MOV     AL,[EBX+5]
        MOV     [EBX+4],AL
        MOV     AL,[EBX+6]
        MOV     [EBX+5],AL
        MOV     AL,[EBX+7]
        MOV     [EBX+6],AL
        MOV     AL,[EBX+8]
        MOV     [EBX+7],AL
        MOV     AL,[EBX+9]
        MOV     [EBX+8],AL
        MOV     AL,[EBX+10]
        MOV     [EBX+9],AL
        MOV     AL,[EBX+11]
        MOV     [EBX+10],AL
        MOV     AL,[EBX+12]
        MOV     [EBX+11],AL
        MOV     AL,[EBX+13]
        MOV     [EBX+12],AL
        MOV     AL,[EBX+14]
        MOV     [EBX+13],AL
        MOV     AL,[EBX+15]
        MOV     [EBX+14],AL
        MOV     [EBX+15],DL
        INC     EDI
        DEC     EBP
        JNE     @@lp
        MOV     [ESI],EBP
        MOV     [ESI+4],EBP
        MOV     [ESI+8],EBP
        MOV     [ESI+12],EBP
        ADD     ESP,$10
        POP     EDI
        POP     ESI
        POP     EBP
        POP     EBX
@@qt:
end;

procedure IntRC6Clear(ID: TRC6ID);
asm
        XOR     EDX,EDX
        CALL    IntFill16
        ADD     EAX,64
        CALL    IntFill32
end;

procedure Q_RC6Done(ID: TRC6ID);
begin
  IntRC6Clear(ID);
  FreeMem(PRC6Data(ID));
end;

function Q_RC6SelfTest: Boolean;
const
  PlainTexts: array[0..1] of string =
    ('00000000000000000000000000000000',
     '02132435465768798A9BACBDCEDFE0F1');
  UserKeys: array[0..5] of string =
    ('00000000000000000000000000000000',
     '0123456789ABCDEF0112233445566778',
     '000000000000000000000000000000000000000000000000',
     '0123456789ABCDEF0112233445566778899AABBCCDDEEFF0',
     '0000000000000000000000000000000000000000000000000000000000000000',
     '0123456789ABCDEF0112233445566778899AABBCCDDEEFF01032547698BADCFE');
  CipherTexts: array[0..5] of string =
    ('8FC3A53656B1F778C129DF4E9848A41E',
     '524E192F4715C6231F51F6367EA43F18',
     '6CD61BCB190B30384E8A3F168690AE82',
     '688329D019E505041E52E92AF95291D4',
     '8F5FBD0510D15FA893FA3FDA6E857EC2',
     'C8241816F0D7E48920AD16A1674E5D48');
var
  ID: TRC6ID;
  K,S: string;
  I: Integer;
begin
  for I := 0 to 5 do
  begin
    K := Q_CodesToStr(UserKeys[I]);
    SetString(K,PChar(K),Length(K));
    Q_RC6Init(ID,Pointer(K),Length(K));
    S := Q_CodesToStr(PlainTexts[I and 1]);
    SetString(S,PChar(S),Length(S));
    IntRC6EncryptECB(ID,Pointer(S));
    if not Q_SameStr(Q_StrToCodes(S),CipherTexts[I]) then
    begin
      Q_RC6Done(ID);
      Result := False;
      Exit;
    end;
    IntRC6DecryptECB(ID,Pointer(S));
    Q_RC6Done(ID);
    if not Q_SameStr(Q_StrToCodes(S),PlainTexts[I and 1]) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

const
  CAST_S1_SBox: array[0..255] of LongWord =
    ($30FB40D4,$9FA0FF0B,$6BECCD2F,$3F258C7A,$1E213F2F,$9C004DD3,$6003E540,$CF9FC949,
     $BFD4AF27,$88BBBDB5,$E2034090,$98D09675,$6E63A0E0,$15C361D2,$C2E7661D,$22D4FF8E,
     $28683B6F,$C07FD059,$FF2379C8,$775F50E2,$43C340D3,$DF2F8656,$887CA41A,$A2D2BD2D,
     $A1C9E0D6,$346C4819,$61B76D87,$22540F2F,$2ABE32E1,$AA54166B,$22568E3A,$A2D341D0,
     $66DB40C8,$A784392F,$004DFF2F,$2DB9D2DE,$97943FAC,$4A97C1D8,$527644B7,$B5F437A7,
     $B82CBAEF,$D751D159,$6FF7F0ED,$5A097A1F,$827B68D0,$90ECF52E,$22B0C054,$BC8E5935,
     $4B6D2F7F,$50BB64A2,$D2664910,$BEE5812D,$B7332290,$E93B159F,$B48EE411,$4BFF345D,
     $FD45C240,$AD31973F,$C4F6D02E,$55FC8165,$D5B1CAAD,$A1AC2DAE,$A2D4B76D,$C19B0C50,
     $882240F2,$0C6E4F38,$A4E4BFD7,$4F5BA272,$564C1D2F,$C59C5319,$B949E354,$B04669FE,
     $B1B6AB8A,$C71358DD,$6385C545,$110F935D,$57538AD5,$6A390493,$E63D37E0,$2A54F6B3,
     $3A787D5F,$6276A0B5,$19A6FCDF,$7A42206A,$29F9D4D5,$F61B1891,$BB72275E,$AA508167,
     $38901091,$C6B505EB,$84C7CB8C,$2AD75A0F,$874A1427,$A2D1936B,$2AD286AF,$AA56D291,
     $D7894360,$425C750D,$93B39E26,$187184C9,$6C00B32D,$73E2BB14,$A0BEBC3C,$54623779,
     $64459EAB,$3F328B82,$7718CF82,$59A2CEA6,$04EE002E,$89FE78E6,$3FAB0950,$325FF6C2,
     $81383F05,$6963C5C8,$76CB5AD6,$D49974C9,$CA180DCF,$380782D5,$C7FA5CF6,$8AC31511,
     $35E79E13,$47DA91D0,$F40F9086,$A7E2419E,$31366241,$051EF495,$AA573B04,$4A805D8D,
     $548300D0,$00322A3C,$BF64CDDF,$BA57A68E,$75C6372B,$50AFD341,$A7C13275,$915A0BF5,
     $6B54BFAB,$2B0B1426,$AB4CC9D7,$449CCD82,$F7FBF265,$AB85C5F3,$1B55DB94,$AAD4E324,
     $CFA4BD3F,$2DEAA3E2,$9E204D02,$C8BD25AC,$EADF55B3,$D5BD9E98,$E31231B2,$2AD5AD6C,
     $954329DE,$ADBE4528,$D8710F69,$AA51C90F,$AA786BF6,$22513F1E,$AA51A79B,$2AD344CC,
     $7B5A41F0,$D37CFBAD,$1B069505,$41ECE491,$B4C332E6,$032268D4,$C9600ACC,$CE387E6D,
     $BF6BB16C,$6A70FB78,$0D03D9C9,$D4DF39DE,$E01063DA,$4736F464,$5AD328D8,$B347CC96,
     $75BB0FC3,$98511BFB,$4FFBCC35,$B58BCF6A,$E11F0ABC,$BFC5FE4A,$A70AEC10,$AC39570A,
     $3F04442F,$6188B153,$E0397A2E,$5727CB79,$9CEB418F,$1CACD68D,$2AD37C96,$0175CB9D,
     $C69DFF09,$C75B65F0,$D9DB40D8,$EC0E7779,$4744EAD4,$B11C3274,$DD24CB9E,$7E1C54BD,
     $F01144F9,$D2240EB1,$9675B3FD,$A3AC3755,$D47C27AF,$51C85F4D,$56907596,$A5BB15E6,
     $580304F0,$CA042CF1,$011A37EA,$8DBFAADB,$35BA3E4A,$3526FFA0,$C37B4D09,$BC306ED9,
     $98A52666,$5648F725,$FF5E569D,$0CED63D0,$7C63B2CF,$700B45E1,$D5EA50F1,$85A92872,
     $AF1FBDA7,$D4234870,$A7870BF3,$2D3B4D79,$42E04198,$0CD0EDE7,$26470DB8,$F881814C,
     $474D6AD7,$7C0C5E5C,$D1231959,$381B7298,$F5D2F4DB,$AB838653,$6E2F1E23,$83719C9E,
     $BD91E046,$9A56456E,$DC39200C,$20C8C571,$962BDA1C,$E1E696FF,$B141AB08,$7CCA89B9,
     $1A69E783,$02CC4843,$A2F7C579,$429EF47D,$427B169C,$5AC9F049,$DD8F0F00,$5C8165BF);

  CAST_S2_SBox: array[0..255] of LongWord =
    ($1F201094,$EF0BA75B,$69E3CF7E,$393F4380,$FE61CF7A,$EEC5207A,$55889C94,$72FC0651,
     $ADA7EF79,$4E1D7235,$D55A63CE,$DE0436BA,$99C430EF,$5F0C0794,$18DCDB7D,$A1D6EFF3,
     $A0B52F7B,$59E83605,$EE15B094,$E9FFD909,$DC440086,$EF944459,$BA83CCB3,$E0C3CDFB,
     $D1DA4181,$3B092AB1,$F997F1C1,$A5E6CF7B,$01420DDB,$E4E7EF5B,$25A1FF41,$E180F806,
     $1FC41080,$179BEE7A,$D37AC6A9,$FE5830A4,$98DE8B7F,$77E83F4E,$79929269,$24FA9F7B,
     $E113C85B,$ACC40083,$D7503525,$F7EA615F,$62143154,$0D554B63,$5D681121,$C866C359,
     $3D63CF73,$CEE234C0,$D4D87E87,$5C672B21,$071F6181,$39F7627F,$361E3084,$E4EB573B,
     $602F64A4,$D63ACD9C,$1BBC4635,$9E81032D,$2701F50C,$99847AB4,$A0E3DF79,$BA6CF38C,
     $10843094,$2537A95E,$F46F6FFE,$A1FF3B1F,$208CFB6A,$8F458C74,$D9E0A227,$4EC73A34,
     $FC884F69,$3E4DE8DF,$EF0E0088,$3559648D,$8A45388C,$1D804366,$721D9BFD,$A58684BB,
     $E8256333,$844E8212,$128D8098,$FED33FB4,$CE280AE1,$27E19BA5,$D5A6C252,$E49754BD,
     $C5D655DD,$EB667064,$77840B4D,$A1B6A801,$84DB26A9,$E0B56714,$21F043B7,$E5D05860,
     $54F03084,$066FF472,$A31AA153,$DADC4755,$B5625DBF,$68561BE6,$83CA6B94,$2D6ED23B,
     $ECCF01DB,$A6D3D0BA,$B6803D5C,$AF77A709,$33B4A34C,$397BC8D6,$5EE22B95,$5F0E5304,
     $81ED6F61,$20E74364,$B45E1378,$DE18639B,$881CA122,$B96726D1,$8049A7E8,$22B7DA7B,
     $5E552D25,$5272D237,$79D2951C,$C60D894C,$488CB402,$1BA4FE5B,$A4B09F6B,$1CA815CF,
     $A20C3005,$8871DF63,$B9DE2FCB,$0CC6C9E9,$0BEEFF53,$E3214517,$B4542835,$9F63293C,
     $EE41E729,$6E1D2D7C,$50045286,$1E6685F3,$F33401C6,$30A22C95,$31A70850,$60930F13,
     $73F98417,$A1269859,$EC645C44,$52C877A9,$CDFF33A6,$A02B1741,$7CBAD9A2,$2180036F,
     $50D99C08,$CB3F4861,$C26BD765,$64A3F6AB,$80342676,$25A75E7B,$E4E6D1FC,$20C710E6,
     $CDF0B680,$17844D3B,$31EEF84D,$7E0824E4,$2CCB49EB,$846A3BAE,$8FF77888,$EE5D60F6,
     $7AF75673,$2FDD5CDB,$A11631C1,$30F66F43,$B3FAEC54,$157FD7FA,$EF8579CC,$D152DE58,
     $DB2FFD5E,$8F32CE19,$306AF97A,$02F03EF8,$99319AD5,$C242FA0F,$A7E3EBB0,$C68E4906,
     $B8DA230C,$80823028,$DCDEF3C8,$D35FB171,$088A1BC8,$BEC0C560,$61A3C9E8,$BCA8F54D,
     $C72FEFFA,$22822E99,$82C570B4,$D8D94E89,$8B1C34BC,$301E16E6,$273BE979,$B0FFEAA6,
     $61D9B8C6,$00B24869,$B7FFCE3F,$08DC283B,$43DAF65A,$F7E19798,$7619B72F,$8F1C9BA4,
     $DC8637A0,$16A7D3B1,$9FC393B7,$A7136EEB,$C6BCC63E,$1A513742,$EF6828BC,$520365D6,
     $2D6A77AB,$3527ED4B,$821FD216,$095C6E2E,$DB92F2FB,$5EEA29CB,$145892F5,$91584F7F,
     $5483697B,$2667A8CC,$85196048,$8C4BACEA,$833860D4,$0D23E0F9,$6C387E8A,$0AE6D249,
     $B284600C,$D835731D,$DCB1C647,$AC4C56EA,$3EBD81B3,$230EABB0,$6438BC87,$F0B5B1FA,
     $8F5EA2B3,$FC184642,$0A036B7A,$4FB089BD,$649DA589,$A345415E,$5C038323,$3E5D3BB9,
     $43D79572,$7E6DD07C,$06DFDF1E,$6C6CC4EF,$7160A539,$73BFBE70,$83877605,$4523ECF1);

  CAST_S3_SBox: array[0..255] of LongWord =
    ($8DEFC240,$25FA5D9F,$EB903DBF,$E810C907,$47607FFF,$369FE44B,$8C1FC644,$AECECA90,
     $BEB1F9BF,$EEFBCAEA,$E8CF1950,$51DF07AE,$920E8806,$F0AD0548,$E13C8D83,$927010D5,
     $11107D9F,$07647DB9,$B2E3E4D4,$3D4F285E,$B9AFA820,$FADE82E0,$A067268B,$8272792E,
     $553FB2C0,$489AE22B,$D4EF9794,$125E3FBC,$21FFFCEE,$825B1BFD,$9255C5ED,$1257A240,
     $4E1A8302,$BAE07FFF,$528246E7,$8E57140E,$3373F7BF,$8C9F8188,$A6FC4EE8,$C982B5A5,
     $A8C01DB7,$579FC264,$67094F31,$F2BD3F5F,$40FFF7C1,$1FB78DFC,$8E6BD2C1,$437BE59B,
     $99B03DBF,$B5DBC64B,$638DC0E6,$55819D99,$A197C81C,$4A012D6E,$C5884A28,$CCC36F71,
     $B843C213,$6C0743F1,$8309893C,$0FEDDD5F,$2F7FE850,$D7C07F7E,$02507FBF,$5AFB9A04,
     $A747D2D0,$1651192E,$AF70BF3E,$58C31380,$5F98302E,$727CC3C4,$0A0FB402,$0F7FEF82,
     $8C96FDAD,$5D2C2AAE,$8EE99A49,$50DA88B8,$8427F4A0,$1EAC5790,$796FB449,$8252DC15,
     $EFBD7D9B,$A672597D,$ADA840D8,$45F54504,$FA5D7403,$E83EC305,$4F91751A,$925669C2,
     $23EFE941,$A903F12E,$60270DF2,$0276E4B6,$94FD6574,$927985B2,$8276DBCB,$02778176,
     $F8AF918D,$4E48F79E,$8F616DDF,$E29D840E,$842F7D83,$340CE5C8,$96BBB682,$93B4B148,
     $EF303CAB,$984FAF28,$779FAF9B,$92DC560D,$224D1E20,$8437AA88,$7D29DC96,$2756D3DC,
     $8B907CEE,$B51FD240,$E7C07CE3,$E566B4A1,$C3E9615E,$3CF8209D,$6094D1E3,$CD9CA341,
     $5C76460E,$00EA983B,$D4D67881,$FD47572C,$F76CEDD9,$BDA8229C,$127DADAA,$438A074E,
     $1F97C090,$081BDB8A,$93A07EBE,$B938CA15,$97B03CFF,$3DC2C0F8,$8D1AB2EC,$64380E51,
     $68CC7BFB,$D90F2788,$12490181,$5DE5FFD4,$DD7EF86A,$76A2E214,$B9A40368,$925D958F,
     $4B39FFFA,$BA39AEE9,$A4FFD30B,$FAF7933B,$6D498623,$193CBCFA,$27627545,$825CF47A,
     $61BD8BA0,$D11E42D1,$CEAD04F4,$127EA392,$10428DB7,$8272A972,$9270C4A8,$127DE50B,
     $285BA1C8,$3C62F44F,$35C0EAA5,$E805D231,$428929FB,$B4FCDF82,$4FB66A53,$0E7DC15B,
     $1F081FAB,$108618AE,$FCFD086D,$F9FF2889,$694BCC11,$236A5CAE,$12DECA4D,$2C3F8CC5,
     $D2D02DFE,$F8EF5896,$E4CF52DA,$95155B67,$494A488C,$B9B6A80C,$5C8F82BC,$89D36B45,
     $3A609437,$EC00C9A9,$44715253,$0A874B49,$D773BC40,$7C34671C,$02717EF6,$4FEB5536,
     $A2D02FFF,$D2BF60C4,$D43F03C0,$50B4EF6D,$07478CD1,$006E1888,$A2E53F55,$B9E6D4BC,
     $A2048016,$97573833,$D7207D67,$DE0F8F3D,$72F87B33,$ABCC4F33,$7688C55D,$7B00A6B0,
     $947B0001,$570075D2,$F9BB88F8,$8942019E,$4264A5FF,$856302E0,$72DBD92B,$EE971B69,
     $6EA22FDE,$5F08AE2B,$AF7A616D,$E5C98767,$CF1FEBD2,$61EFC8C2,$F1AC2571,$CC8239C2,
     $67214CB8,$B1E583D1,$B7DC3E62,$7F10BDCE,$F90A5C38,$0FF0443D,$606E6DC6,$60543A49,
     $5727C148,$2BE98A1D,$8AB41738,$20E1BE24,$AF96DA0F,$68458425,$99833BE5,$600D457D,
     $282F9350,$8334B362,$D91D1120,$2B6D8DA0,$642B1E31,$9C305A00,$52BCE688,$1B03588A,
     $F7BAEFD5,$4142ED9C,$A4315C11,$83323EC5,$DFEF4636,$A133C501,$E9D3531C,$EE353783);

  CAST_S4_SBox: array[0..255] of LongWord =
    ($9DB30420,$1FB6E9DE,$A7BE7BEF,$D273A298,$4A4F7BDB,$64AD8C57,$85510443,$FA020ED1,
     $7E287AFF,$E60FB663,$095F35A1,$79EBF120,$FD059D43,$6497B7B1,$F3641F63,$241E4ADF,
     $28147F5F,$4FA2B8CD,$C9430040,$0CC32220,$FDD30B30,$C0A5374F,$1D2D00D9,$24147B15,
     $EE4D111A,$0FCA5167,$71FF904C,$2D195FFE,$1A05645F,$0C13FEFE,$081B08CA,$05170121,
     $80530100,$E83E5EFE,$AC9AF4F8,$7FE72701,$D2B8EE5F,$06DF4261,$BB9E9B8A,$7293EA25,
     $CE84FFDF,$F5718801,$3DD64B04,$A26F263B,$7ED48400,$547EEBE6,$446D4CA0,$6CF3D6F5,
     $2649ABDF,$AEA0C7F5,$36338CC1,$503F7E93,$D3772061,$11B638E1,$72500E03,$F80EB2BB,
     $ABE0502E,$EC8D77DE,$57971E81,$E14F6746,$C9335400,$6920318F,$081DBB99,$FFC304A5,
     $4D351805,$7F3D5CE3,$A6C866C6,$5D5BCCA9,$DAEC6FEA,$9F926F91,$9F46222F,$3991467D,
     $A5BF6D8E,$1143C44F,$43958302,$D0214EEB,$022083B8,$3FB6180C,$18F8931E,$281658E6,
     $26486E3E,$8BD78A70,$7477E4C1,$B506E07C,$F32D0A25,$79098B02,$E4EABB81,$28123B23,
     $69DEAD38,$1574CA16,$DF871B62,$211C40B7,$A51A9EF9,$0014377B,$041E8AC8,$09114003,
     $BD59E4D2,$E3D156D5,$4FE876D5,$2F91A340,$557BE8DE,$00EAE4A7,$0CE5C2EC,$4DB4BBA6,
     $E756BDFF,$DD3369AC,$EC17B035,$06572327,$99AFC8B0,$56C8C391,$6B65811C,$5E146119,
     $6E85CB75,$BE07C002,$C2325577,$893FF4EC,$5BBFC92D,$D0EC3B25,$B7801AB7,$8D6D3B24,
     $20C763EF,$C366A5FC,$9C382880,$0ACE3205,$AAC9548A,$ECA1D7C7,$041AFA32,$1D16625A,
     $6701902C,$9B757A54,$31D477F7,$9126B031,$36CC6FDB,$C70B8B46,$D9E66A48,$56E55A79,
     $026A4CEB,$52437EFF,$2F8F76B4,$0DF980A5,$8674CDE3,$EDDA04EB,$17A9BE04,$2C18F4DF,
     $B7747F9D,$AB2AF7B4,$EFC34D20,$2E096B7C,$1741A254,$E5B6A035,$213D42F6,$2C1C7C26,
     $61C2F50F,$6552DAF9,$D2C231F8,$25130F69,$D8167FA2,$0418F2C8,$001A96A6,$0D1526AB,
     $63315C21,$5E0A72EC,$49BAFEFD,$187908D9,$8D0DBD86,$311170A7,$3E9B640C,$CC3E10D7,
     $D5CAD3B6,$0CAEC388,$F73001E1,$6C728AFF,$71EAE2A1,$1F9AF36E,$CFCBD12F,$C1DE8417,
     $AC07BE6B,$CB44A1D8,$8B9B0F56,$013988C3,$B1C52FCA,$B4BE31CD,$D8782806,$12A3A4E2,
     $6F7DE532,$58FD7EB6,$D01EE900,$24ADFFC2,$F4990FC5,$9711AAC5,$001D7B95,$82E5E7D2,
     $109873F6,$00613096,$C32D9521,$ADA121FF,$29908415,$7FBB977F,$AF9EB3DB,$29C9ED2A,
     $5CE2A465,$A730F32C,$D0AA3FE8,$8A5CC091,$D49E2CE7,$0CE454A9,$D60ACD86,$015F1919,
     $77079103,$DEA03AF6,$78A8565E,$DEE356DF,$21F05CBE,$8B75E387,$B3C50651,$B8A5C3EF,
     $D8EEB6D2,$E523BE77,$C2154529,$2F69EFDF,$AFE67AFB,$F470C4B2,$F3E0EB5B,$D6CC9876,
     $39E4460C,$1FDA8538,$1987832F,$CA007367,$A99144F8,$296B299E,$492FC295,$9266BEAB,
     $B5676E69,$9BD3DDDA,$DF7E052F,$DB25701C,$1B5E51EE,$F65324E6,$6AFCE36C,$0316CC04,
     $8644213E,$B7DC59D0,$7965291F,$CCD6FD43,$41823979,$932BCDF6,$B657C34D,$4EDFD282,
     $7AE5290C,$3CB9536B,$851E20FE,$9833557E,$13ECF0B0,$D3FFB372,$3F85C5C1,$0AEF7ED2);

procedure CAST_Qi(BETA, KrKm: Pointer);
asm
        MOV     EAX,[EDI+12]
        MOV     ECX,[ESI]
        MOV     EDX,[ESI+4]
        ADD     EDX,EAX
        ROL     EDX,CL
        MOVZX   ECX,DH
        MOV     EAX,DWORD PTR [ECX*4+CAST_S1_SBox]
        MOVZX   ECX,DL
        SHR     EDX,16
        XOR     EAX,DWORD PTR [ECX*4+CAST_S2_SBox]
        MOVZX   ECX,DH
        MOVZX   EDX,DL
        SUB     EAX,DWORD PTR [ECX*4+CAST_S3_SBox]
        ADD     EAX,DWORD PTR [EDX*4+CAST_S4_SBox]
        MOV     ECX,[ESI+8]
        XOR     EAX,[EDI+8]
        MOV     EDX,[ESI+12]
        MOV     [EDI+8],EAX
        XOR     EDX,EAX
        ROL     EDX,CL
        MOVZX   ECX,DH
        MOV     EAX,DWORD PTR [ECX*4+CAST_S1_SBox]
        MOVZX   ECX,DL
        SHR     EDX,16
        SUB     EAX,DWORD PTR [ECX*4+CAST_S2_SBox]
        MOVZX   ECX,DH
        MOVZX   EDX,DL
        ADD     EAX,DWORD PTR [ECX*4+CAST_S3_SBox]
        XOR     EAX,DWORD PTR [EDX*4+CAST_S4_SBox]
        MOV     ECX,[ESI+16]
        XOR     EAX,[EDI+4]
        MOV     EDX,[ESI+20]
        MOV     [EDI+4],EAX
        SUB     EDX,EAX
        ROL     EDX,CL
        MOVZX   ECX,DH
        MOV     EAX,DWORD PTR [ECX*4+CAST_S1_SBox]
        MOVZX   ECX,DL
        SHR     EDX,16
        ADD     EAX,DWORD PTR [ECX*4+CAST_S2_SBox]
        MOVZX   ECX,DH
        MOVZX   EDX,DL
        XOR     EAX,DWORD PTR [ECX*4+CAST_S3_SBox]
        SUB     EAX,DWORD PTR [EDX*4+CAST_S4_SBox]
        MOV     ECX,[ESI+24]
        XOR     EAX,[EDI]
        MOV     EDX,[ESI+28]
        MOV     [EDI],EAX
        ADD     EDX,EAX
        ROL     EDX,CL
        MOVZX   ECX,DH
        MOV     EAX,DWORD PTR [ECX*4+CAST_S1_SBox]
        MOVZX   ECX,DL
        SHR     EDX,16
        XOR     EAX,DWORD PTR [ECX*4+CAST_S2_SBox]
        MOVZX   ECX,DH
        MOVZX   EDX,DL
        SUB     EAX,DWORD PTR [ECX*4+CAST_S3_SBox]
        ADD     EAX,DWORD PTR [EDX*4+CAST_S4_SBox]
        XOR     [EDI+12],EAX
end;

procedure CAST_QBARi(BETA, KrKm: Pointer);
asm
        MOV     EAX,[EDI]
        MOV     EDX,[ESI+28]
        MOV     ECX,[ESI+24]
        ADD     EDX,EAX
        ROL     EDX,CL
        MOVZX   ECX,DH
        MOV     EAX,DWORD PTR [ECX*4+CAST_S1_SBox]
        MOVZX   ECX,DL
        SHR     EDX,16
        XOR     EAX,DWORD PTR [ECX*4+CAST_S2_SBox]
        MOVZX   ECX,DH
        MOVZX   EDX,DL
        SUB     EAX,DWORD PTR [ECX*4+CAST_S3_SBox]
        ADD     EAX,DWORD PTR [EDX*4+CAST_S4_SBox]
        MOV     EDX,[ESI+20]
        XOR     [EDI+12],EAX
        MOV     EAX,[EDI+4]
        MOV     ECX,[ESI+16]
        SUB     EDX,EAX
        ROL     EDX,CL
        MOVZX   ECX,DH
        MOV     EAX,DWORD PTR [ECX*4+CAST_S1_SBox]
        MOVZX   ECX,DL
        SHR     EDX,16
        ADD     EAX,DWORD PTR [ECX*4+CAST_S2_SBox]
        MOVZX   ECX,DH
        MOVZX   EDX,DL
        XOR     EAX,DWORD PTR [ECX*4+CAST_S3_SBox]
        SUB     EAX,DWORD PTR [EDX*4+CAST_S4_SBox]
        MOV     EDX,[ESI+12]
        XOR     [EDI],EAX
        MOV     EAX,[EDI+8]
        MOV     ECX,[ESI+8]
        XOR     EDX,EAX
        ROL     EDX,CL
        MOVZX   ECX,DH
        MOV     EAX,DWORD PTR [ECX*4+CAST_S1_SBox]
        MOVZX   ECX,DL
        SHR     EDX,16
        SUB     EAX,DWORD PTR [ECX*4+CAST_S2_SBox]
        MOVZX   ECX,DH
        MOVZX   EDX,DL
        ADD     EAX,DWORD PTR [ECX*4+CAST_S3_SBox]
        XOR     EAX,DWORD PTR [EDX*4+CAST_S4_SBox]
        MOV     EDX,[ESI+4]
        XOR     [EDI+4],EAX
        MOV     EAX,[EDI+12]
        MOV     ECX,[ESI]
        ADD     EDX,EAX
        ROL     EDX,CL
        MOVZX   ECX,DH
        MOV     EAX,DWORD PTR [ECX*4+CAST_S1_SBox]
        MOVZX   ECX,DL
        SHR     EDX,16
        XOR     EAX,DWORD PTR [ECX*4+CAST_S2_SBox]
        MOVZX   ECX,DH
        MOVZX   EDX,DL
        SUB     EAX,DWORD PTR [ECX*4+CAST_S3_SBox]
        ADD     EAX,DWORD PTR [EDX*4+CAST_S4_SBox]
        XOR     [EDI+8],EAX
end;

procedure CAST_Wi(KAPPA, TmTrAdr: Pointer);
asm
	MOV	EAX,[EDI+64]
	MOV	EDX,[EDI+68]
        MOV     [EDI],EAX
        LEA     ECX,[EDX+16]
        ADD     EAX,$6ED9EBA1
        MOV     [EDI+4],ECX
        ADD     EDX,17
        MOV     [EDI+8],EAX
        LEA     ECX,[EDX+16]
        ADD     EAX,$6ED9EBA1
        MOV     [EDI+12],ECX
        ADD     EDX,17
        MOV     [EDI+16],EAX
        LEA     ECX,[EDX+16]
        ADD     EAX,$6ED9EBA1
        MOV     [EDI+20],ECX
        ADD     EDX,17
        MOV     [EDI+24],EAX
        LEA     ECX,[EDX+16]
        ADD     EAX,$6ED9EBA1
        MOV     [EDI+28],ECX
        ADD     EDX,17
        MOV     [EDI+32],EAX
        LEA     ECX,[EDX+16]
        ADD     EAX,$6ED9EBA1
        MOV     [EDI+36],ECX
        ADD     EDX,17
        MOV     [EDI+40],EAX
        LEA     ECX,[EDX+16]
        ADD     EAX,$6ED9EBA1
        MOV     [EDI+44],ECX
        ADD     EDX,17
        MOV     [EDI+48],EAX
        LEA     ECX,[EDX+16]
        ADD     EAX,$6ED9EBA1
        MOV     [EDI+52],ECX
        ADD     EDX,17
        MOV     [EDI+56],EAX
        LEA     ECX,[EDX+16]
        ADD     EAX,$6ED9EBA1
        MOV     [EDI+60],ECX
        ADD     EDX,17
	MOV	[EDI+64],EAX
	MOV	[EDI+68],EDX
        MOV     EAX,[ESI+28]
        MOV     EDX,[EDI]
        MOV     ECX,[EDI+4]
        ADD     EDX,EAX
        ROL     EDX,CL
        MOVZX   ECX,DH
        MOV     EAX,DWORD PTR [ECX*4+CAST_S1_SBox]
        MOVZX   ECX,DL
        SHR     EDX,16
        XOR     EAX,DWORD PTR [ECX*4+CAST_S2_SBox]
        MOVZX   ECX,DH
        MOVZX   EDX,DL
        SUB     EAX,DWORD PTR [ECX*4+CAST_S3_SBox]
        ADD     EAX,DWORD PTR [EDX*4+CAST_S4_SBox]
        MOV     ECX,[EDI+12]
        XOR     EAX,[ESI+24]
        MOV     EDX,[EDI+8]
        MOV     [ESI+24],EAX
        XOR     EDX,EAX
        ROL     EDX,CL
        MOVZX   ECX,DH
        MOV     EAX,DWORD PTR [ECX*4+CAST_S1_SBox]
        MOVZX   ECX,DL
        SHR     EDX,16
        SUB     EAX,DWORD PTR [ECX*4+CAST_S2_SBox]
        MOVZX   ECX,DH
        MOVZX   EDX,DL
        ADD     EAX,DWORD PTR [ECX*4+CAST_S3_SBox]
        XOR     EAX,DWORD PTR [EDX*4+CAST_S4_SBox]
        MOV     ECX,[EDI+20]
        XOR     EAX,[ESI+20]
        MOV     EDX,[EDI+16]
        MOV     [ESI+20],EAX
        SUB     EDX,EAX
        ROL     EDX,CL
        MOVZX   ECX,DH
        MOV     EAX,DWORD PTR [ECX*4+CAST_S1_SBox]
        MOVZX   ECX,DL
        SHR     EDX,16
        ADD     EAX,DWORD PTR [ECX*4+CAST_S2_SBox]
        MOVZX   ECX,DH
        MOVZX   EDX,DL
        XOR     EAX,DWORD PTR [ECX*4+CAST_S3_SBox]
        SUB     EAX,DWORD PTR [EDX*4+CAST_S4_SBox]
        MOV     ECX,[EDI+28]
        XOR     EAX,[ESI+16]
        MOV     EDX,[EDI+24]
        MOV     [ESI+16],EAX
        ADD     EDX,EAX
        ROL     EDX,CL
        MOVZX   ECX,DH
        MOV     EAX,DWORD PTR [ECX*4+CAST_S1_SBox]
        MOVZX   ECX,DL
        SHR     EDX,16
        XOR     EAX,DWORD PTR [ECX*4+CAST_S2_SBox]
        MOVZX   ECX,DH
        MOVZX   EDX,DL
        SUB     EAX,DWORD PTR [ECX*4+CAST_S3_SBox]
        ADD     EAX,DWORD PTR [EDX*4+CAST_S4_SBox]
        MOV     ECX,[EDI+36]
        XOR     EAX,[ESI+12]
        MOV     EDX,[EDI+32]
        MOV     [ESI+12],EAX
        XOR     EDX,EAX
        ROL     EDX,CL
        MOVZX   ECX,DH
        MOV     EAX,DWORD PTR [ECX*4+CAST_S1_SBox]
        MOVZX   ECX,DL
        SHR     EDX,16
        SUB     EAX,DWORD PTR [ECX*4+CAST_S2_SBox]
        MOVZX   ECX,DH
        MOVZX   EDX,DL
        ADD     EAX,DWORD PTR [ECX*4+CAST_S3_SBox]
        XOR     EAX,DWORD PTR [EDX*4+CAST_S4_SBox]
        MOV     ECX,[EDI+44]
        XOR     EAX,[ESI+8]
        MOV     EDX,[EDI+40]
        MOV     [ESI+8],EAX
        SUB     EDX,EAX
        ROL     EDX,CL
        MOVZX   ECX,DH
        MOV     EAX,DWORD PTR [ECX*4+CAST_S1_SBox]
        MOVZX   ECX,DL
        SHR     EDX,16
        ADD     EAX,DWORD PTR [ECX*4+CAST_S2_SBox]
        MOVZX   ECX,DH
        MOVZX   EDX,DL
        XOR     EAX,DWORD PTR [ECX*4+CAST_S3_SBox]
        SUB     EAX,DWORD PTR [EDX*4+CAST_S4_SBox]
        MOV     ECX,[EDI+52]
        XOR     EAX,[ESI+4]
        MOV     EDX,[EDI+48]
        MOV     [ESI+4],EAX
        ADD     EDX,EAX
        ROL     EDX,CL
        MOVZX   ECX,DH
        MOV     EAX,DWORD PTR [ECX*4+CAST_S1_SBox]
        MOVZX   ECX,DL
        SHR     EDX,16
        XOR     EAX,DWORD PTR [ECX*4+CAST_S2_SBox]
        MOVZX   ECX,DH
        MOVZX   EDX,DL
        SUB     EAX,DWORD PTR [ECX*4+CAST_S3_SBox]
        ADD     EAX,DWORD PTR [EDX*4+CAST_S4_SBox]
        MOV     ECX,[EDI+60]
        XOR     EAX,[ESI]
        MOV     EDX,[EDI+56]
        MOV     [ESI],EAX
        XOR     EDX,EAX
        ROL     EDX,CL
        MOVZX   ECX,DH
        MOV     EAX,DWORD PTR [ECX*4+CAST_S1_SBox]
        MOVZX   ECX,DL
        SHR     EDX,16
        SUB     EAX,DWORD PTR [ECX*4+CAST_S2_SBox]
        MOVZX   ECX,DH
        MOVZX   EDX,DL
        ADD     EAX,DWORD PTR [ECX*4+CAST_S3_SBox]
        XOR     EAX,DWORD PTR [EDX*4+CAST_S4_SBox]
        XOR     [ESI+28],EAX
end;

type
  PCAST6Data = ^TCAST6Data;
  TCAST6Data = record
    Vec: TCAST6InitVector;
    KeyData: array[0..95] of LongWord;
  end;

procedure IntCAST6KeySchedule(ID: TCASTID; Key: Pointer; KeyLen: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        XOR     EBX,EBX
        PUSH    EBP
        MOV     [EAX],EBX
        SUB     ESP,$20
        MOV     [EAX+4],EBX
        MOV     ESI,ESP
        MOV     [EAX+8],EBX
        MOV     [EAX+12],EBX
        MOV     [ESI],EBX
        MOV     [ESI+4],EBX
        MOV     [ESI+8],EBX
        MOV     [ESI+12],EBX
        MOV     [ESI+16],EBX
        MOV     [ESI+20],EBX
        MOV     [ESI+24],EBX
        MOV     [ESI+28],EBX
        LEA     EBX,[EAX+16]
        MOV     EDI,ECX
        SHR     ECX,2
        MOV     EBP,ECX
        JE      @@nu1
@@m1:   MOV     EAX,[EDX+ECX*4-4]
        BSWAP   EAX
        MOV     [ESI+ECX*4-4],EAX
        DEC     ECX
        JNE     @@m1
@@nu1:  AND     EDI,3
        XOR     EAX,EAX
        JMP     DWORD PTR @@DtV[EDI*4]
@@DtV:  DD      @@Dt0,@@Dt1,@@Dt2,@@Dt3
@@Dt3:  MOVZX   EAX,BYTE PTR [EDX+EBP*4+2]
        ROR     EAX,8
@@Dt2:  MOVZX   EDI,BYTE PTR [EDX+EBP*4+1]
        OR      EAX,EDI
        ROR     EAX,8
@@Dt1:  MOVZX   EDI,BYTE PTR [EDX+EBP*4]
        OR      EAX,EDI
        ROR     EAX,8
        MOV     [ESI+EBP*4],EAX
@@Dt0:  SUB	ESP,$48
	MOV	EDI,ESP
	MOV	EAX,$5A827999
	MOV	ECX,19
	MOV	[EDI+64],EAX
	MOV	[EDI+68],ECX
        MOV     EBP,12
@@lp:   CALL    CAST_Wi
        CALL    CAST_Wi
        MOV     EAX,[ESI]
        ADD     EAX,16
        MOV     [EBX],EAX
        MOV     EAX,[ESI+28]
        MOV     [EBX+4],EAX
        MOV     EAX,[ESI+8]
        ADD     EAX,16
        MOV     [EBX+8],EAX
        MOV     EAX,[ESI+20]
        MOV     [EBX+12],EAX
        MOV     EAX,[ESI+16]
        ADD     EAX,16
        MOV     [EBX+16],EAX
        MOV     EAX,[ESI+12]
        MOV     [EBX+20],EAX
        MOV     EAX,[ESI+24]
        ADD     EAX,16
        MOV     [EBX+24],EAX
        MOV     EAX,[ESI+4]
        MOV     [EBX+28],EAX
        ADD     EBX,32
        DEC     EBP
        JNE     @@lp
        XOR     EDX,EDX
        MOV     [EDI],EDX
        MOV     [EDI+4],EDX
        MOV     [EDI+8],EDX
        MOV     [EDI+12],EDX
        MOV     [EDI+16],EDX
        MOV     [EDI+20],EDX
        MOV     [EDI+24],EDX
        MOV     [EDI+28],EDX
        MOV     [EDI+32],EDX
        MOV     [EDI+36],EDX
        LEA     EAX,[EDI+40]
        CALL    IntFill16
        ADD     ESP,$68
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure Q_CAST6Init(var ID: TCASTID; Key: Pointer; KeyLen: Cardinal);
begin
  GetMem(PCAST6Data(ID),SizeOf(TCAST6Data));
  if KeyLen <= 32 then
    IntCAST6KeySchedule(ID,Key,KeyLen)
  else
    IntCAST6KeySchedule(ID,Key,32);
end;

procedure IntCAST6EncryptECB(ID: TCASTID; P: Pointer);
asm
        PUSH    ESI
        PUSH    EDI
        LEA     ESI,[EAX+16]
        MOV     EDI,EDX
        MOV     EAX,[EDX]
        BSWAP   EAX
        MOV     [EDX],EAX
        MOV     EAX,[EDX+4]
        BSWAP   EAX
        MOV     [EDX+4],EAX
        MOV     EAX,[EDX+8]
        BSWAP   EAX
        MOV     [EDX+8],EAX
        MOV     EAX,[EDX+12]
        BSWAP   EAX
        MOV     [EDX+12],EAX
        CALL    CAST_Qi
        ADD     ESI,32
        CALL    CAST_Qi
        ADD     ESI,32
        CALL    CAST_Qi
        ADD     ESI,32
        CALL    CAST_Qi
        ADD     ESI,32
        CALL    CAST_Qi
        ADD     ESI,32
        CALL    CAST_Qi
        ADD     ESI,32
        CALL    CAST_QBARi
        ADD     ESI,32
        CALL    CAST_QBARi
        ADD     ESI,32
        CALL    CAST_QBARi
        ADD     ESI,32
        CALL    CAST_QBARi
        ADD     ESI,32
        CALL    CAST_QBARi
        ADD     ESI,32
        CALL    CAST_QBARi
        MOV     EAX,[EDI]
        BSWAP   EAX
        MOV     [EDI],EAX
        MOV     EAX,[EDI+4]
        BSWAP   EAX
        MOV     [EDI+4],EAX
        MOV     EAX,[EDI+8]
        BSWAP   EAX
        MOV     [EDI+8],EAX
        MOV     EAX,[EDI+12]
        BSWAP   EAX
        MOV     [EDI+12],EAX
        POP     EDI
        POP     ESI
end;

procedure IntCAST6DecryptECB(ID: TCASTID; P: Pointer);
asm
        PUSH    ESI
        PUSH    EDI
        LEA     ESI,[EAX+368]
        MOV     EDI,EDX
        MOV     EAX,[EDX]
        BSWAP   EAX
        MOV     [EDX],EAX
        MOV     EAX,[EDX+4]
        BSWAP   EAX
        MOV     [EDX+4],EAX
        MOV     EAX,[EDX+8]
        BSWAP   EAX
        MOV     [EDX+8],EAX
        MOV     EAX,[EDX+12]
        BSWAP   EAX
        MOV     [EDX+12],EAX
        CALL    CAST_Qi
        SUB     ESI,32
        CALL    CAST_Qi
        SUB     ESI,32
        CALL    CAST_Qi
        SUB     ESI,32
        CALL    CAST_Qi
        SUB     ESI,32
        CALL    CAST_Qi
        SUB     ESI,32
        CALL    CAST_Qi
        SUB     ESI,32
        CALL    CAST_QBARi
        SUB     ESI,32
        CALL    CAST_QBARi
        SUB     ESI,32
        CALL    CAST_QBARi
        SUB     ESI,32
        CALL    CAST_QBARi
        SUB     ESI,32
        CALL    CAST_QBARi
        SUB     ESI,32
        CALL    CAST_QBARi
        MOV     EAX,[EDI]
        BSWAP   EAX
        MOV     [EDI],EAX
        MOV     EAX,[EDI+4]
        BSWAP   EAX
        MOV     [EDI+4],EAX
        MOV     EAX,[EDI+8]
        BSWAP   EAX
        MOV     [EDI+8],EAX
        MOV     EAX,[EDI+12]
        BSWAP   EAX
        MOV     [EDI+12],EAX
        POP     EDI
        POP     ESI
end;

procedure Q_CAST6SetOrdinaryVector(ID: TCASTID);
asm
        XOR     EDX,EDX
        MOV     [EAX],EDX
        MOV     [EAX+4],EDX
        MOV     [EAX+8],EDX
        MOV     [EAX+12],EDX
        MOV     EDX,EAX
        CALL    IntCAST6EncryptECB
end;

procedure Q_CAST6SetInitVector(ID: TCASTID; const IV: TCAST6InitVector);
asm
        MOV     ECX,[EDX]
        MOV     [EAX],ECX
        MOV     ECX,[EDX+4]
        MOV     [EAX+4],ECX
        MOV     ECX,[EDX+8]
        MOV     [EAX+8],ECX
        MOV     ECX,[EDX+12]
        MOV     [EAX+12],ECX
end;

procedure Q_CAST6EncryptCBC(ID: TCASTID; P: Pointer; L: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     ESI,ECX
        PUSH    ECX
        MOV     EDI,EDX
        SHR     ESI,4
        JE      @@nx
@@lp1:  MOV     EAX,[EDI]
        XOR     [EBX],EAX
        MOV     EAX,[EDI+4]
        XOR     [EBX+4],EAX
        MOV     EAX,[EDI+8]
        XOR     [EBX+8],EAX
        MOV     EAX,[EDI+12]
        XOR     [EBX+12],EAX
        MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntCAST6EncryptECB
        MOV     EAX,[EBX]
        MOV     [EDI],EAX
        MOV     EAX,[EBX+4]
        MOV     [EDI+4],EAX
        MOV     EAX,[EBX+8]
        MOV     [EDI+8],EAX
        MOV     EAX,[EBX+12]
        MOV     [EDI+12],EAX
        ADD     EDI,$10
        DEC     ESI
        JNE     @@lp1
@@nx:   POP     ESI
        AND     ESI,$F
        JE      @@qt
        MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntCAST6EncryptECB
        MOV     ECX,ESI
        SHR     ESI,2
        JE      @@uu
@@lp2:  MOV     EAX,[EBX]
        XOR     EAX,[EDI]
        MOV     [EBX],EAX
        MOV     [EDI],EAX
        ADD     EBX,4
        ADD     EDI,4
        DEC     ESI
        JNE     @@lp2
@@uu:   AND     ECX,3
        JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@qt, @@t1, @@t2, @@t3
@@t3:   MOV     AL,[EBX+2]
        XOR     AL,[EDI+2]
        MOV     [EBX+2],AL
        MOV     [EDI+2],AL
@@t2:   MOV     AL,[EBX+1]
        XOR     AL,[EDI+1]
        MOV     [EBX+1],AL
        MOV     [EDI+1],AL
@@t1:   MOV     AL,[EBX]
        XOR     AL,[EDI]
        MOV     [EBX],AL
        MOV     [EDI],AL
@@qt:   POP     EDI
        POP     ESI
        POP     EBX
end;

procedure Q_CAST6DecryptCBC(ID: TCASTID; P: Pointer; L: Cardinal);
asm
        PUSH    EBX
        PUSH    EBP
        PUSH    ESI
        MOV     EBX,EAX
        PUSH    EDI
        MOV     EBP,ECX
        SUB     ESP,$10
        MOV     EDI,EDX
        MOV     ESI,ESP
        PUSH    ECX
        SHR     EBP,4
        JE      @@nx
@@lp1:  MOV     EAX,[EDI]
        MOV     [ESI],EAX
        MOV     EAX,[EDI+4]
        MOV     [ESI+4],EAX
        MOV     EAX,[EDI+8]
        MOV     [ESI+8],EAX
        MOV     EAX,[EDI+12]
        MOV     [ESI+12],EAX
        MOV     EAX,EBX
        MOV     EDX,EDI
        CALL    IntCAST6DecryptECB
        MOV     EAX,[EBX]
        XOR     [EDI],EAX
        MOV     EAX,[EBX+4]
        XOR     [EDI+4],EAX
        MOV     EAX,[EBX+8]
        XOR     [EDI+8],EAX
        MOV     EAX,[EBX+12]
        XOR     [EDI+12],EAX
        MOV     EAX,[ESI]
        MOV     [EBX],EAX
        MOV     EAX,[ESI+4]
        MOV     [EBX+4],EAX
        MOV     EAX,[ESI+8]
        MOV     [EBX+8],EAX
        MOV     EAX,[ESI+12]
        MOV     [EBX+12],EAX
        ADD     EDI,$10
        DEC     EBP
        JNE     @@lp1
@@nx:   XOR     EDX,EDX
        POP     EBP
        MOV     [ESI],EDX
        MOV     [ESI+4],EDX
        MOV     [ESI+8],EDX
        MOV     [ESI+12],EDX
        ADD     ESP,$10
        AND     EBP,$F
        JE      @@qt
        MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntCAST6EncryptECB
        MOV     ECX,EBP
        SHR     EBP,2
        JE      @@uu
@@lp2:  MOV     EAX,[EBX]
        MOV     EDX,[EDI]
        XOR     EAX,EDX
        MOV     [EBX],EDX
        MOV     [EDI],EAX
        ADD     EBX,4
        ADD     EDI,4
        DEC     EBP
        JNE     @@lp2
@@uu:   AND     ECX,3
        JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@qt, @@t1, @@t2, @@t3
@@t3:   MOV     AL,[EBX+2]
        MOV     DL,[EDI+2]
        XOR     AL,DL
        MOV     [EBX+2],DL
        MOV     [EDI+2],AL
@@t2:   MOV     AL,[EBX+1]
        MOV     DL,[EDI+1]
        XOR     AL,DL
        MOV     [EBX+1],DL
        MOV     [EDI+1],AL
@@t1:   MOV     AL,[EBX]
        MOV     DL,[EDI]
        XOR     AL,DL
        MOV     [EBX],DL
        MOV     [EDI],AL
@@qt:   POP     EDI
        POP     ESI
        POP     EBP
        POP     EBX
end;

procedure Q_CAST6EncryptCFB128(ID: TCASTID; P: Pointer; L: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     ESI,ECX
        PUSH    ECX
        MOV     EDI,EDX
        SHR     ESI,4
        JE      @@nx
@@lp1:  MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntCAST6EncryptECB
        MOV     EAX,[EBX]
        XOR     EAX,[EDI]
        MOV     [EBX],EAX
        MOV     [EDI],EAX
        MOV     EAX,[EBX+4]
        XOR     EAX,[EDI+4]
        MOV     [EBX+4],EAX
        MOV     [EDI+4],EAX
        MOV     EAX,[EBX+8]
        XOR     EAX,[EDI+8]
        MOV     [EBX+8],EAX
        MOV     [EDI+8],EAX
        MOV     EAX,[EBX+12]
        XOR     EAX,[EDI+12]
        MOV     [EBX+12],EAX
        MOV     [EDI+12],EAX
        ADD     EDI,$10
        DEC     ESI
        JNE     @@lp1
@@nx:   POP     ESI
        AND     ESI,$F
        JE      @@qt
        MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntCAST6EncryptECB
        MOV     ECX,ESI
        SHR     ESI,2
        JE      @@uu
@@lp2:  MOV     EAX,[EBX]
        XOR     EAX,[EDI]
        MOV     [EBX],EAX
        MOV     [EDI],EAX
        ADD     EBX,4
        ADD     EDI,4
        DEC     ESI
        JNE     @@lp2
@@uu:   AND     ECX,3
        JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@qt, @@t1, @@t2, @@t3
@@t3:   MOV     AL,[EBX+2]
        XOR     AL,[EDI+2]
        MOV     [EBX+2],AL
        MOV     [EDI+2],AL
@@t2:   MOV     AL,[EBX+1]
        XOR     AL,[EDI+1]
        MOV     [EBX+1],AL
        MOV     [EDI+1],AL
@@t1:   MOV     AL,[EBX]
        XOR     AL,[EDI]
        MOV     [EBX],AL
        MOV     [EDI],AL
@@qt:   POP     EDI
        POP     ESI
        POP     EBX
end;

procedure Q_CAST6DecryptCFB128(ID: TCASTID; P: Pointer; L: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     ESI,ECX
        PUSH    ECX
        MOV     EDI,EDX
        SHR     ESI,4
        JE      @@nx
@@lp1:  MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntCAST6EncryptECB
        MOV     EAX,[EBX]
        MOV     EDX,[EDI]
        XOR     EAX,EDX
        MOV     [EBX],EDX
        MOV     [EDI],EAX
        MOV     EAX,[EBX+4]
        MOV     EDX,[EDI+4]
        XOR     EAX,EDX
        MOV     [EBX+4],EDX
        MOV     [EDI+4],EAX
        MOV     EAX,[EBX+8]
        MOV     EDX,[EDI+8]
        XOR     EAX,EDX
        MOV     [EBX+8],EDX
        MOV     [EDI+8],EAX
        MOV     EAX,[EBX+12]
        MOV     EDX,[EDI+12]
        XOR     EAX,EDX
        MOV     [EBX+12],EDX
        MOV     [EDI+12],EAX
        ADD     EDI,$10
        DEC     ESI
        JNE     @@lp1
@@nx:   POP     ESI
        AND     ESI,$F
        JE      @@qt
        MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntCAST6EncryptECB
        MOV     ECX,ESI
        SHR     ESI,2
        JE      @@uu
@@lp2:  MOV     EAX,[EBX]
        MOV     EDX,[EDI]
        XOR     EAX,EDX
        MOV     [EBX],EDX
        MOV     [EDI],EAX
        ADD     EBX,4
        ADD     EDI,4
        DEC     ESI
        JNE     @@lp2
@@uu:   AND     ECX,3
        JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@qt, @@t1, @@t2, @@t3
@@t3:   MOV     AL,[EBX+2]
        MOV     DL,[EDI+2]
        XOR     AL,DL
        MOV     [EBX+2],DL
        MOV     [EDI+2],AL
@@t2:   MOV     AL,[EBX+1]
        MOV     DL,[EDI+1]
        XOR     AL,DL
        MOV     [EBX+1],DL
        MOV     [EDI+1],AL
@@t1:   MOV     AL,[EBX]
        MOV     DL,[EDI]
        XOR     AL,DL
        MOV     [EBX],DL
        MOV     [EDI],AL
@@qt:   POP     EDI
        POP     ESI
        POP     EBX
end;

procedure Q_CAST6EncryptCFB(ID: TCASTID; P: Pointer; L: Cardinal);
asm
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EBX
        PUSH    EBP
        PUSH    ESI
        MOV     EBX,EAX
        PUSH    EDI
        MOV     EBP,ECX
        SUB     ESP,$10
        MOV     EDI,EDX
        MOV     ESI,ESP
@@lp:   MOV     EAX,[EBX]
        MOV     [ESI],EAX
        MOV     EAX,[EBX+4]
        MOV     [ESI+4],EAX
        MOV     EAX,[EBX+8]
        MOV     [ESI+8],EAX
        MOV     EAX,[EBX+12]
        MOV     [ESI+12],EAX
        MOV     EAX,EBX
        MOV     EDX,ESI
        CALL    IntCAST6EncryptECB
        MOV     DL,[EDI]
        XOR     DL,[ESI]
        MOV     [EDI],DL
        MOV     AL,[EBX+1]
        MOV     [EBX],AL
        MOV     AL,[EBX+2]
        MOV     [EBX+1],AL
        MOV     AL,[EBX+3]
        MOV     [EBX+2],AL
        MOV     AL,[EBX+4]
        MOV     [EBX+3],AL
        MOV     AL,[EBX+5]
        MOV     [EBX+4],AL
        MOV     AL,[EBX+6]
        MOV     [EBX+5],AL
        MOV     AL,[EBX+7]
        MOV     [EBX+6],AL
        MOV     AL,[EBX+8]
        MOV     [EBX+7],AL
        MOV     AL,[EBX+9]
        MOV     [EBX+8],AL
        MOV     AL,[EBX+10]
        MOV     [EBX+9],AL
        MOV     AL,[EBX+11]
        MOV     [EBX+10],AL
        MOV     AL,[EBX+12]
        MOV     [EBX+11],AL
        MOV     AL,[EBX+13]
        MOV     [EBX+12],AL
        MOV     AL,[EBX+14]
        MOV     [EBX+13],AL
        MOV     AL,[EBX+15]
        MOV     [EBX+14],AL
        MOV     [EBX+15],DL
        INC     EDI
        DEC     EBP
        JNE     @@lp
        MOV     [ESI],EBP
        MOV     [ESI+4],EBP
        MOV     [ESI+8],EBP
        MOV     [ESI+12],EBP
        ADD     ESP,$10
        POP     EDI
        POP     ESI
        POP     EBP
        POP     EBX
@@qt:
end;

procedure Q_CAST6DecryptCFB(ID: TCASTID; P: Pointer; L: Cardinal);
asm
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EBX
        PUSH    EBP
        PUSH    ESI
        MOV     EBX,EAX
        PUSH    EDI
        MOV     EBP,ECX
        SUB     ESP,$10
        MOV     EDI,EDX
        MOV     ESI,ESP
@@lp:   MOV     EAX,[EBX]
        MOV     [ESI],EAX
        MOV     EAX,[EBX+4]
        MOV     [ESI+4],EAX
        MOV     EAX,[EBX+8]
        MOV     [ESI+8],EAX
        MOV     EAX,[EBX+12]
        MOV     [ESI+12],EAX
        MOV     EAX,EBX
        MOV     EDX,ESI
        CALL    IntCAST6EncryptECB
        MOV     DL,[EDI]
        MOV     AL,[ESI]
        XOR     AL,DL
        MOV     [EDI],AL
        MOV     AL,[EBX+1]
        MOV     [EBX],AL
        MOV     AL,[EBX+2]
        MOV     [EBX+1],AL
        MOV     AL,[EBX+3]
        MOV     [EBX+2],AL
        MOV     AL,[EBX+4]
        MOV     [EBX+3],AL
        MOV     AL,[EBX+5]
        MOV     [EBX+4],AL
        MOV     AL,[EBX+6]
        MOV     [EBX+5],AL
        MOV     AL,[EBX+7]
        MOV     [EBX+6],AL
        MOV     AL,[EBX+8]
        MOV     [EBX+7],AL
        MOV     AL,[EBX+9]
        MOV     [EBX+8],AL
        MOV     AL,[EBX+10]
        MOV     [EBX+9],AL
        MOV     AL,[EBX+11]
        MOV     [EBX+10],AL
        MOV     AL,[EBX+12]
        MOV     [EBX+11],AL
        MOV     AL,[EBX+13]
        MOV     [EBX+12],AL
        MOV     AL,[EBX+14]
        MOV     [EBX+13],AL
        MOV     AL,[EBX+15]
        MOV     [EBX+14],AL
        MOV     [EBX+15],DL
        INC     EDI
        DEC     EBP
        JNE     @@lp
        MOV     [ESI],EBP
        MOV     [ESI+4],EBP
        MOV     [ESI+8],EBP
        MOV     [ESI+12],EBP
        ADD     ESP,$10
        POP     EDI
        POP     ESI
        POP     EBP
        POP     EBX
@@qt:
end;

procedure Q_CAST6ApplyOFB128(ID: TCASTID; P: Pointer; L: Cardinal);
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     ESI,ECX
        PUSH    ECX
        MOV     EDI,EDX
        SHR     ESI,4
        JE      @@nx
@@lp1:  MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntCAST6EncryptECB
        MOV     EAX,[EDI]
        XOR     EAX,[EBX]
        MOV     [EDI],EAX
        MOV     EAX,[EDI+4]
        XOR     EAX,[EBX+4]
        MOV     [EDI+4],EAX
        MOV     EAX,[EDI+8]
        XOR     EAX,[EBX+8]
        MOV     [EDI+8],EAX
        MOV     EAX,[EDI+12]
        XOR     EAX,[EBX+12]
        MOV     [EDI+12],EAX
        ADD     EDI,$10
        DEC     ESI
        JNE     @@lp1
@@nx:   POP     ESI
        AND     ESI,$F
        JE      @@qt
        MOV     EAX,EBX
        MOV     EDX,EBX
        CALL    IntCAST6EncryptECB
        MOV     ECX,ESI
        SHR     ESI,2
        JE      @@uu
@@lp2:  MOV     EAX,[EDI]
        XOR     EAX,[EBX]
        MOV     [EDI],EAX
        ADD     EBX,4
        ADD     EDI,4
        DEC     ESI
        JNE     @@lp2
@@uu:   AND     ECX,3
        JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@qt, @@t1, @@t2, @@t3
@@t3:   MOV     AL,[EDI+2]
        XOR     AL,[EBX+2]
        MOV     [EDI+2],AL
@@t2:   MOV     AL,[EDI+1]
        XOR     AL,[EBX+1]
        MOV     [EDI+1],AL
@@t1:   MOV     AL,[EDI]
        XOR     AL,[EBX]
        MOV     [EDI],AL
@@qt:   POP     EDI
        POP     ESI
        POP     EBX
end;

procedure Q_CAST6ApplyOFB(ID: TCASTID; P: Pointer; L: Cardinal);
asm
        TEST    ECX,ECX
        JE      @@qt
        PUSH    EBX
        PUSH    EBP
        PUSH    ESI
        MOV     EBX,EAX
        PUSH    EDI
        MOV     EBP,ECX
        SUB     ESP,$10
        MOV     EDI,EDX
        MOV     ESI,ESP
@@lp:   MOV     EAX,[EBX]
        MOV     [ESI],EAX
        MOV     EAX,[EBX+4]
        MOV     [ESI+4],EAX
        MOV     EAX,[EBX+8]
        MOV     [ESI+8],EAX
        MOV     EAX,[EBX+12]
        MOV     [ESI+12],EAX
        MOV     EAX,EBX
        MOV     EDX,ESI
        CALL    IntCAST6EncryptECB
        MOV     AL,[EDI]
        MOV     DL,[ESI]
        XOR     AL,DL
        MOV     [EDI],AL
        MOV     AL,[EBX+1]
        MOV     [EBX],AL
        MOV     AL,[EBX+2]
        MOV     [EBX+1],AL
        MOV     AL,[EBX+3]
        MOV     [EBX+2],AL
        MOV     AL,[EBX+4]
        MOV     [EBX+3],AL
        MOV     AL,[EBX+5]
        MOV     [EBX+4],AL
        MOV     AL,[EBX+6]
        MOV     [EBX+5],AL
        MOV     AL,[EBX+7]
        MOV     [EBX+6],AL
        MOV     AL,[EBX+8]
        MOV     [EBX+7],AL
        MOV     AL,[EBX+9]
        MOV     [EBX+8],AL
        MOV     AL,[EBX+10]
        MOV     [EBX+9],AL
        MOV     AL,[EBX+11]
        MOV     [EBX+10],AL
        MOV     AL,[EBX+12]
        MOV     [EBX+11],AL
        MOV     AL,[EBX+13]
        MOV     [EBX+12],AL
        MOV     AL,[EBX+14]
        MOV     [EBX+13],AL
        MOV     AL,[EBX+15]
        MOV     [EBX+14],AL
        MOV     [EBX+15],DL
        INC     EDI
        DEC     EBP
        JNE     @@lp
        MOV     [ESI],EBP
        MOV     [ESI+4],EBP
        MOV     [ESI+8],EBP
        MOV     [ESI+12],EBP
        ADD     ESP,$10
        POP     EDI
        POP     ESI
        POP     EBP
        POP     EBX
@@qt:
end;

procedure IntCAST6Clear(ID: TCASTID);
asm
        XOR     EDX,EDX
        CALL    IntFill32
        ADD     EAX,128
        CALL    IntFill32
        ADD     EAX,128
        CALL    IntFill32
        MOV     [EAX+128],EDX
        MOV     [EAX+132],EDX
        MOV     [EAX+136],EDX
        MOV     [EAX+140],EDX
end;

procedure Q_CAST6Done(ID: TCASTID);
begin
  IntCAST6Clear(ID);
  FreeMem(PCAST6Data(ID));
end;

function Q_CAST6SelfTest: Boolean;
const
  PlainText: string = '00000000000000000000000000000000';
var
  ID: TCASTID;
  K,S,S1: string;
begin
  S1 := Q_CodesToStr(PlainText);
  K := Q_CodesToStr('2342BB9EFA38542C0AF75647F29F615D');
  Q_CAST6Init(ID,Pointer(K),Length(K));
  IntCAST6EncryptECB(ID,Pointer(S1));
  S := Q_StrToCodes(S1);
  if not Q_SameStr(S,'C842A08972B43D20836C91D1B7530F6B') then
  begin
    Q_CAST6Done(ID);
    Result := False;
    Exit;
  end;
  IntCAST6DecryptECB(ID,Pointer(S1));
  Q_CAST6Done(ID);
  S := Q_StrToCodes(S1);
  if not Q_SameStr(S,PlainText) then
  begin
    Result := False;
    Exit;
  end;
  K := Q_CodesToStr('2342BB9EFA38542CBED0AC83940AC298BAC77A7717942863');
  Q_CAST6Init(ID,Pointer(K),Length(K));
  IntCAST6EncryptECB(ID,Pointer(S1));
  S := Q_StrToCodes(S1);
  if not Q_SameStr(S,'1B386C0210DCADCBDD0E41AA08A7A7E8') then
  begin
    Q_CAST6Done(ID);
    Result := False;
    Exit;
  end;
  IntCAST6DecryptECB(ID,Pointer(S1));
  Q_CAST6Done(ID);
  S := Q_StrToCodes(S1);
  if not Q_SameStr(S,PlainText) then
  begin
    Result := False;
    Exit;
  end;
  K := Q_CodesToStr('2342BB9EFA38542CBED0AC83940AC2988D7C47CE264908461CC1B5137AE6B604');
  Q_CAST6Init(ID,Pointer(K),Length(K));
  IntCAST6EncryptECB(ID,Pointer(S1));
  S := Q_StrToCodes(S1);
  if not Q_SameStr(S,'4F6A2038286897B9C9870136553317FA') then
  begin
    Q_CAST6Done(ID);
    Result := False;
    Exit;
  end;
  IntCAST6DecryptECB(ID,Pointer(S1));
  Q_CAST6Done(ID);
  S := Q_StrToCodes(S1);
  Result := Q_SameStr(S,PlainText);
end;

type
  PSHA1Data = ^TSHA1Data;
  TSHA1Data = record
    Buffer: array[0..63] of Byte;
    LHi,LLo,Index: LongWord;
    Hash: TSHA1Digest;
    Tmp: array[0..79] of LongWord;
  end;

procedure IntSHA1Compress(ID: TSHAID);
asm
        PUSH    EBX
        PUSH    ESI
        XOR     ECX,ECX
        PUSH    EDI
        MOV     [EAX+72],ECX
        PUSH    EBP
        LEA     ESI,[EAX].TSHA1Data.Tmp
        MOV     EBX,[EAX]
        BSWAP   EBX
        MOV     [ESI],EBX
        MOV     EBX,[EAX+4]
        BSWAP   EBX
        MOV     [ESI+4],EBX
        MOV     EBX,[EAX+8]
        BSWAP   EBX
        MOV     [ESI+8],EBX
        MOV     EBX,[EAX+12]
        BSWAP   EBX
        MOV     [ESI+12],EBX
        MOV     EBX,[EAX+16]
        BSWAP   EBX
        MOV     [ESI+16],EBX
        MOV     EBX,[EAX+20]
        BSWAP   EBX
        MOV     [ESI+20],EBX
        MOV     EBX,[EAX+24]
        BSWAP   EBX
        MOV     [ESI+24],EBX
        MOV     EBX,[EAX+28]
        BSWAP   EBX
        MOV     [ESI+28],EBX
        MOV     EBX,[EAX+32]
        BSWAP   EBX
        MOV     [ESI+32],EBX
        MOV     EBX,[EAX+36]
        BSWAP   EBX
        MOV     [ESI+36],EBX
        MOV     EBX,[EAX+40]
        BSWAP   EBX
        MOV     [ESI+40],EBX
        MOV     EBX,[EAX+44]
        BSWAP   EBX
        MOV     [ESI+44],EBX
        MOV     EBX,[EAX+48]
        BSWAP   EBX
        MOV     [ESI+48],EBX
        MOV     EBX,[EAX+52]
        BSWAP   EBX
        MOV     [ESI+52],EBX
        MOV     EBX,[EAX+56]
        BSWAP   EBX
        MOV     [ESI+56],EBX
        MOV     EBX,[EAX+60]
        BSWAP   EBX
        LEA     EDI,[ESI+64]
        MOV     [ESI+60],EBX
        PUSH    EAX
        LEA     EDX,[EAX].TSHA1Data.Hash
        MOV     ECX,16
@@lp0:  MOV     EAX,[EDI-64]
        MOV     EBX,[EDI-56]
        XOR     EAX,[EDI-32]
        XOR     EBX,[EDI-12]
        XOR     EAX,EBX
        ROL     EAX,1
        MOV     [EDI],EAX
        MOV     EAX,[EDI-60]
        MOV     EBX,[EDI-52]
        XOR     EAX,[EDI-28]
        XOR     EBX,[EDI-8]
        XOR     EAX,EBX
        ROL     EAX,1
        MOV     [EDI+4],EAX
        MOV     EAX,[EDI-56]
        XOR     EAX,[EDI-24]
        MOV     EBX,[EDI-48]
        XOR     EBX,[EDI-4]
        XOR     EAX,EBX
        ROL     EAX,1
        MOV     [EDI+8],EAX
        MOV     EAX,[EDI-52]
        MOV     EBX,[EDI-44]
        XOR     EAX,[EDI-20]
        XOR     EBX,[EDI]
        XOR     EAX,EBX
        ROL     EAX,1
        MOV     [EDI+12],EAX
        ADD     EDI,16
        DEC     ECX
        JNE     @@lp0
        MOV     ECX,5
        MOV     EAX,[EDX]
        PUSH    ECX
        MOV     EDI,[EDX+4]
        MOV     EBX,[EDX+8]
        MOV     EBP,[EDX+12]
        MOV     ECX,[EDX+16]
        MOV     EDX,EAX
@@lp1:  ROL     EAX,5
        ADD     EAX,ECX
        MOV     ECX,EBX
        XOR     ECX,EBP
        ADD     EAX,[ESI]
        AND     ECX,EDI
        XOR     ECX,EBP
        ADD     ECX,$5A827999
        ADD     EAX,ECX
        MOV     ECX,EBP
        ROR     EDI,2
        MOV     EBP,EAX
        ROL     EAX,5
        ADD     EAX,ECX
        MOV     ECX,EDI
        XOR     ECX,EBX
        AND     ECX,EDX
        ADD     EAX,[ESI+4]
        XOR     ECX,EBX
        ROR     EDX,2
        ADD     ECX,$5A827999
        ADD     EAX,ECX
        MOV     ECX,EBX
        MOV     EBX,EAX
        ROL     EAX,5
        ADD     EAX,ECX
        MOV     ECX,EDX
        ADD     EAX,[ESI+8]
        XOR     ECX,EDI
        AND     ECX,EBP
        ROR     EBP,2
        XOR     ECX,EDI
        ADD     ECX,$5A827999
        ADD     EAX,ECX
        MOV     ECX,EDI
        MOV     EDI,EAX
        ROL     EAX,5
        ADD     EAX,ECX
        MOV     ECX,EBP
        ADD     EAX,[ESI+12]
        XOR     ECX,EDX
        AND     ECX,EBX
        XOR     ECX,EDX
        ADD     ECX,$5A827999
        ROR     EBX,2
        ADD     EAX,ECX
        MOV     ECX,EDX
        MOV     EDX,EAX
        ADD     ESI,16
        DEC     DWORD PTR [ESP]
        JNE     @@lp1
        MOV     DWORD PTR [ESP],5
@@lp2:  ROL     EAX,5
        ADD     EAX,ECX
        MOV     ECX,EDI
        ADD     EAX,[ESI]
        XOR     ECX,EBX
        XOR     ECX,EBP
        ADD     ECX,$6ED9EBA1
        ADD     EAX,ECX
        MOV     ECX,EBP
        ROR     EDI,2
        MOV     EBP,EAX
        ROL     EAX,5
        ADD     EAX,ECX
        MOV     ECX,EDX
        XOR     ECX,EDI
        XOR     ECX,EBX
        ADD     ECX,$6ED9EBA1
        ROR     EDX,2
        ADD     EAX,[ESI+4]
        ADD     EAX,ECX
        MOV     ECX,EBX
        MOV     EBX,EAX
        ROL     EAX,5
        ADD     EAX,ECX
        MOV     ECX,EBP
        XOR     ECX,EDX
        ROR     EBP,2
        XOR     ECX,EDI
        ADD     EAX,[ESI+8]
        ADD     ECX,$6ED9EBA1
        ADD     EAX,ECX
        MOV     ECX,EDI
        MOV     EDI,EAX
        ROL     EAX,5
        ADD     EAX,ECX
        MOV     ECX,EBX
        XOR     ECX,EBP
        XOR     ECX,EDX
        ADD     EAX,[ESI+12]
        ADD     ECX,$6ED9EBA1
        ADD     EAX,ECX
        MOV     ECX,EDX
        ROR     EBX,2
        MOV     EDX,EAX
        ADD     ESI,16
        DEC     DWORD PTR [ESP]
        JNE     @@lp2
        MOV     DWORD PTR [ESP],5
        PUSH    ECX
@@lp3:  MOV     EAX,EDI
        MOV     ECX,EDI
        AND     EAX,EBX
        OR      ECX,EBX
        AND     ECX,EBP
        OR      EAX,ECX
        MOV     ECX,EDX
        ADD     EAX,[ESI]
        ROL     ECX,5
        ADD     EAX,$8F1BBCDC
        ADD     ECX,[ESP]
        ADD     EAX,ECX
        ROR     EDI,2
        MOV     [ESP],EBP
        MOV     EBP,EAX
        MOV     EAX,EDX
        MOV     ECX,EDX
        AND     EAX,EDI
        OR      ECX,EDI
        AND     ECX,EBX
        OR      EAX,ECX
        MOV     ECX,EBP
        ADD     EAX,[ESI+4]
        ROL     ECX,5
        ADD     EAX,$8F1BBCDC
        ADD     ECX,[ESP]
        ADD     EAX,ECX
        ROR     EDX,2
        MOV     [ESP],EBX
        MOV     EBX,EAX
        MOV     EAX,EBP
        MOV     ECX,EBP
        AND     EAX,EDX
        OR      ECX,EDX
        AND     ECX,EDI
        OR      EAX,ECX
        MOV     ECX,EBX
        ADD     EAX,[ESI+8]
        ROL     ECX,5
        ADD     EAX,$8F1BBCDC
        ADD     ECX,[ESP]
        ADD     EAX,ECX
        ROR     EBP,2
        MOV     [ESP],EDI
        MOV     EDI,EAX
        MOV     EAX,EBX
        MOV     ECX,EBX
        AND     EAX,EBP
        OR      ECX,EBP
        AND     ECX,EDX
        OR      EAX,ECX
        MOV     ECX,EDI
        ADD     EAX,[ESI+12]
        ROL     ECX,5
        ADD     EAX,$8F1BBCDC
        ADD     ECX,[ESP]
        ADD     EAX,ECX
        ROR     EBX,2
        MOV     [ESP],EDX
        MOV     EDX,EAX
        ADD     ESI,16
        DEC     DWORD PTR [ESP+4]
        JNE     @@lp3
        POP     ECX
        MOV     DWORD PTR [ESP],5
@@lp4:  ROL     EAX,5
        ADD     EAX,ECX
        MOV     ECX,EDI
        XOR     ECX,EBX
        XOR     ECX,EBP
        ROR     EDI,2
        ADD     EAX,[ESI]
        ADD     ECX,$CA62C1D6
        ADD     EAX,ECX
        MOV     ECX,EBP
        MOV     EBP,EAX
        ROL     EAX,5
        ADD     EAX,ECX
        MOV     ECX,EDX
        ADD     EAX,[ESI+4]
        XOR     ECX,EDI
        XOR     ECX,EBX
        ADD     ECX,$CA62C1D6
        ADD     EAX,ECX
        MOV     ECX,EBX
        ROR     EDX,2
        MOV     EBX,EAX
        ROL     EAX,5
        ADD     EAX,ECX
        MOV     ECX,EBP
        ADD     EAX,[ESI+8]
        XOR     ECX,EDX
        XOR     ECX,EDI
        ROR     EBP,2
        ADD     ECX,$CA62C1D6
        ADD     EAX,ECX
        MOV     ECX,EDI
        MOV     EDI,EAX
        ROL     EAX,5
        ADD     EAX,ECX
        MOV     ECX,EBX
        ADD     EAX,[ESI+12]
        XOR     ECX,EBP
        XOR     ECX,EDX
        ADD     ECX,$CA62C1D6
        ADD     EAX,ECX
        MOV     ECX,EDX
        ROR     EBX,2
        MOV     EDX,EAX
        ADD     ESI,16
        DEC     DWORD PTR [ESP]
        JNE     @@lp4
        POP     EAX
        POP     EAX
        ADD     [EAX+76],EDX
        ADD     [EAX+80],EDI
        ADD     [EAX+84],EBX
        ADD     [EAX+88],EBP
        ADD     [EAX+92],ECX
        POP     EBP
        POP     EDI
        XOR     EDX,EDX
        CALL    IntFill16
        POP     ESI
        POP     EBX
end;

procedure IntSHA1Clear(ID: TSHAID);
asm
        XOR     EDX,EDX
        MOV     [EAX+64],EDX
        MOV     [EAX+68],EDX
        MOV     [EAX+72],EDX
        MOV     [EAX+76],EDX
        MOV     [EAX+80],EDX
        MOV     [EAX+84],EDX
        MOV     [EAX+88],EDX
        MOV     [EAX+92],EDX
        LEA     EAX,[EAX].TSHA1Data.Tmp
        CALL    IntFill32
        ADD     EAX,128
        CALL    IntFill32
        ADD     EAX,128
        CALL    IntFill16
end;

procedure IntSHA1Init(ID: TSHAID);
begin
  with PSHA1Data(ID)^ do
  begin
    Hash[0]:= $67452301;
    Hash[1]:= $EFCDAB89;
    Hash[2]:= $98BADCFE;
    Hash[3]:= $10325476;
    Hash[4]:= $C3D2E1F0;
  end;
end;

procedure Q_SHA1Init(var ID: TSHAID);
begin
  GetMem(PSHA1Data(ID),SizeOf(TSHA1Data));
  IntFill16(Pointer(ID),0);
  with PSHA1Data(ID)^ do
  begin
    LHi := 0;
    LLo := 0;
    Index := 0;
  end;
  IntSHA1Init(ID);
end;

procedure Q_SHA1Update(ID: TSHAID; P: Pointer; L: Cardinal);
var
  N: LongWord;
begin
  with PSHA1Data(ID)^ do
  begin
    N := L shl 3;
    Inc(LLo, N);
    if LLo < N then
      Inc(LHi);
    Inc(LHi, L shr 29);
    while L <> 0 do
    begin
      N := SizeOf(Buffer)-Index;
      if N <= L then
      begin
        Q_CopyMem(P,@Buffer[Index],N);
        Inc(PByte(P),N);
        Dec(L,N);
        IntSHA1Compress(ID);
      end else
      begin
        Q_CopyMem(P,@Buffer[Index],L);
        Inc(Index,L);
        Exit;
      end;
    end;
  end;
end;

procedure IntSHA1Final(ID: TSHAID; var Digest: TSHA1Digest);
var
  U: LongWord;
begin
  with PSHA1Data(ID)^ do
  begin
    Buffer[Index]:= $80;
    if Index>= 56 then
      IntSHA1Compress(ID);
    U := LHi;
    asm
        MOV     EAX,U
        BSWAP   EAX
        MOV     U,EAX
    end;
    PLong(@Buffer[56])^:= U;
    U := LLo;
    asm
        MOV     EAX,U
        BSWAP   EAX
        MOV     U,EAX
    end;
    PLong(@Buffer[60])^:= U;
    IntSHA1Compress(ID);
    Digest[0] := Hash[0];
    Digest[1] := Hash[1];
    Digest[2] := Hash[2];
    Digest[3] := Hash[3];
    Digest[4] := Hash[4];
  end;
end;

procedure Q_SHA1Final(ID: TSHAID; var Digest: TSHA1Digest);
begin
  IntSHA1Final(ID,Digest);
  IntSHA1Clear(ID);
  FreeMem(PSHA1Data(ID));
end;

procedure Q_SHA1(const S: string; var Digest: TSHA1Digest);
var
  ID: TSHAID;
begin
  Q_SHA1Init(ID);
  Q_SHA1Update(ID,Pointer(S),Length(S));
  Q_SHA1Final(ID,Digest);
end;

procedure Q_SHA1(P: Pointer; L: Cardinal; var Digest: TSHA1Digest);
var
  ID: TSHAID;
begin
  Q_SHA1Init(ID);
  Q_SHA1Update(ID,P,L);
  Q_SHA1Final(ID,Digest);
end;

procedure Q_SHA1(const SourceDigest: TSHA1Digest; var Digest: TSHA1Digest);
var
  ID: TSHAID;
begin
  Q_SHA1Init(ID);
  Q_SHA1Update(ID,@SourceDigest,SizeOf(TSHA1Digest));
  Q_SHA1Final(ID,Digest);
end;

procedure Q_SHA1(const SourceDigest: TMixDigest; var Digest: TSHA1Digest);
var
  ID: TSHAID;
begin
  Q_SHA1Init(ID);
  Q_SHA1Update(ID,@SourceDigest,SizeOf(TMixDigest));
  Q_SHA1Final(ID,Digest);
end;

function Q_SHA1SelfTest: Boolean;
var
  ID: TSHAID;
  Dig: TSHA1Digest;
  S: string;
begin
  Q_SHA1('abc',Dig);
  if not ((Dig[0]=$A9993E36) and (Dig[1]=$4706816A) and (Dig[2]=$BA3E2571) and
    (Dig[3]=$7850C26C) and (Dig[4]=$9CD0D89D)) then
  begin
    Result := False;
    Exit;
  end;
  S := StringOfChar('a',200000);
  Q_SHA1Init(ID);
  Q_SHA1Update(ID,Pointer(S),200000);
  Q_SHA1Update(ID,Pointer(S),200000);
  Q_SHA1Update(ID,Pointer(S),200000);
  Q_SHA1Update(ID,Pointer(S),200000);
  Q_SHA1Update(ID,Pointer(S),200000);
  Q_SHA1Final(ID,Dig);
  if not ((Dig[0]=$34AA973C) and (Dig[1]=$D4C4DAA4) and (Dig[2]=$F61EEB2B)
    and (Dig[3]=$DBAD2731) and (Dig[4]=$6534016F)) then
  begin
    Result := False;
    Exit;
  end;
  Q_SHA1Init(ID);
  S := 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq';
  Q_SHA1Update(ID,Pointer(S),Length(S));
  Q_SHA1Final(ID,Dig);
  Result := (Dig[0]=$84983E44) and (Dig[1]=$1C3BD26E) and (Dig[2]=$BAAE4AA1) and
    (Dig[3]=$F95129E5) and (Dig[4]=$E54670F1);
end;

const
  MixBufferSize = 8193;

type
  PMixData = ^TMixData;
  TMixData = record
    Dig1,Dig2: TSHA1Digest;
    DigPass: TSHA1Digest;
    Place: array[0..11] of LongWord;
    SHAID: TSHAID;
    CASTID: TCASTID;
    Index: Cardinal;
    Hash: TMixDigest;
    Buffer: array[0..MixBufferSize-1] of Byte;
  end;

procedure IntMixCompress(ID: TMixID);
var
  BO,BI: Cardinal;
begin
  with PMixData(ID)^ do
  begin
    if Index <> 1 then
    begin
      BO := (Index+2) div 3;
      BI := Index-(BO shl 1);
      IntSHA1Init(SHAID);
      Q_SHA1Update(SHAID,@Buffer,BO+BI);
      IntSHA1Final(SHAID,Dig1);
      Place[0] := Hash[5] xor Dig1[0];
      Place[2] := Hash[6] xor Dig1[1];
      Place[4] := Hash[7] xor Dig1[2];
      Place[6] := Hash[8] xor Dig1[3];
      Place[8] := Hash[9] xor Dig1[4];
      IntSHA1Init(SHAID);
      Q_SHA1Update(SHAID,@Buffer[BO],BO+BI);
      IntSHA1Final(SHAID,Dig2);
      Place[1] := Hash[0] xor Dig2[0];
      Place[3] := Hash[1] xor Dig2[1];
      Place[5] := Hash[2] xor Dig2[2];
      Place[7] := Hash[3] xor Dig2[3];
      Place[9] := Hash[4] xor Dig2[4];
      IntSHA1Init(SHAID);
      Q_SHA1Update(SHAID,@Buffer[BO+BI],BO);
      Q_SHA1Update(SHAID,@Buffer,BO);
      IntSHA1Final(SHAID,DigPass);
    end else
    begin
      IntSHA1Init(SHAID);
      Q_SHA1Update(SHAID,@Buffer,1);
      IntSHA1Final(SHAID,Dig1);
      Place[0] := Hash[5] xor Dig1[0];
      Place[2] := Hash[6] xor Dig1[1];
      Place[4] := Hash[7] xor Dig1[2];
      Place[6] := Hash[8] xor Dig1[3];
      Place[8] := Hash[9] xor Dig1[4];
      Buffer[0] := not Buffer[0];
      IntSHA1Init(SHAID);
      Q_SHA1Update(SHAID,@Buffer,1);
      IntSHA1Final(SHAID,Dig2);
      Place[1] := Hash[0] xor Dig2[0];
      Place[3] := Hash[1] xor Dig2[1];
      Place[5] := Hash[2] xor Dig2[2];
      Place[7] := Hash[3] xor Dig2[3];
      Place[9] := Hash[4] xor Dig2[4];
      Buffer[0] := Buffer[0] xor $AA;
      IntSHA1Init(SHAID);
      Q_SHA1Update(SHAID,@Buffer,1);
      IntSHA1Final(SHAID,DigPass);
    end;
    IntCAST6KeySchedule(CASTID,@DigPass,SizeOf(DigPass));
    IntCAST6EncryptECB(CASTID,@Place);
    IntCAST6EncryptECB(CASTID,@Place[2]);
    IntCAST6EncryptECB(CASTID,@Place[4]);
    IntCAST6EncryptECB(CASTID,@Place[6]);
    Place[10] := Place[0];
    Place[11] := Place[1];
    IntCAST6EncryptECB(CASTID,@Place[8]);
    Place[0] := Place[10];
    Place[1] := Place[11];
    IntCAST6EncryptECB(CASTID,@Place);
    IntCAST6EncryptECB(CASTID,@Place[2]);
    IntCAST6EncryptECB(CASTID,@Place[4]);
    IntCAST6EncryptECB(CASTID,@Place[6]);
    Place[10] := Place[0];
    Place[11] := Place[1];
    IntCAST6EncryptECB(CASTID,@Place[8]);
    Place[0] := Place[10];
    Place[1] := Place[11];
    IntCAST6EncryptECB(CASTID,@Place);
    IntCAST6EncryptECB(CASTID,@Place[2]);
    IntCAST6EncryptECB(CASTID,@Place[4]);
    IntCAST6EncryptECB(CASTID,@Place[6]);
    Place[10] := Place[0];
    Place[11] := Place[1];
    IntCAST6EncryptECB(CASTID,@Place[8]);
    Place[0] := Place[10];
    Place[1] := Place[11];
    IntCAST6EncryptECB(CASTID,@Place);
    IntCAST6EncryptECB(CASTID,@Place[2]);
    IntCAST6EncryptECB(CASTID,@Place[4]);
    IntCAST6EncryptECB(CASTID,@Place[6]);
    Place[10] := Place[0];
    Place[11] := Place[1];
    IntCAST6EncryptECB(CASTID,@Place[8]);
    Place[0] := Place[10];
    Place[1] := Place[11];
    Q_XORByRandom(@Place,40,Dig1[0]);
    Q_XORByRandom(@Place,40,Dig1[1]);
    Q_XORByRandom(@Place,40,Dig1[2]);
    Q_XORByRandom(@Place,40,Dig1[3]);
    Q_XORByRandom(@Place,40,Dig1[4]);
    Q_XORByRandom(@Place,40,Dig2[0]);
    Q_XORByRandom(@Place,40,Dig2[1]);
    Q_XORByRandom(@Place,40,Dig2[2]);
    Q_XORByRandom(@Place,40,Dig2[3]);
    Q_XORByRandom(@Place,40,Dig2[4]);
    Inc(Hash[0],Place[0]);
    Inc(Hash[1],Place[1]);
    Inc(Hash[2],Place[2]);
    Inc(Hash[3],Place[3]);
    Inc(Hash[4],Place[4]);
    Inc(Hash[5],Place[5]);
    Inc(Hash[6],Place[6]);
    Inc(Hash[7],Place[7]);
    Inc(Hash[8],Place[8]);
    Inc(Hash[9],Place[9]);
    Index := 0;
  end;
end;

procedure Q_MixHashInit(var ID: TMixID);
begin
  GetMem(PMixData(ID),SizeOf(TMixData));
  with PMixData(ID)^ do
  begin
    Q_SHA1Init(SHAID);
    GetMem(PCAST6Data(CASTID),SizeOf(TCAST6Data));
    Index := 0;
    Hash[0] := $A6A163F4;
    Hash[1] := $3E4A378C;
    Hash[2] := $FEDA4FF3;
    Hash[3] := $D5D632CB;
    Hash[4] := $2A6DFD49;
    Hash[5] := $06153197;
    Hash[6] := $92ED05D1;
    Hash[7] := $50FB311D;
    Hash[8] := $193FEEC4;
    Hash[9] := $870D7415;
  end;
end;

procedure Q_MixHashUpdate(ID: TMixID; P: Pointer; L: Cardinal);
var
  N: LongWord;
begin
  with PMixData(ID)^ do
    while L <> 0 do
    begin
      N := MixBufferSize-Index;
      if N <= L then
      begin
        Q_CopyMem(P,@Buffer[Index],N);
        Inc(PByte(P),N);
        Index := MixBufferSize;
        IntMixCompress(ID);
        Dec(L,N);
      end else
      begin
        Q_CopyMem(P,@Buffer[Index],L);
        Inc(Index,L);
        Exit;
      end;
    end;
end;

procedure Q_MixHashFinal(ID: TMixID; var Digest: TMixDigest);
begin
  with PMixData(ID)^ do
  begin
    if Index <> 0 then
      IntMixCompress(ID);
    Digest[0] := Hash[0];
    Digest[1] := Hash[1];
    Digest[2] := Hash[2];
    Digest[3] := Hash[3];
    Digest[4] := Hash[4];
    Digest[5] := Hash[5];
    Digest[6] := Hash[6];
    Digest[7] := Hash[7];
    Digest[8] := Hash[8];
    Digest[9] := Hash[9];
    IntSHA1Clear(SHAID);
    FreeMem(PSHA1Data(SHAID));
    IntCAST6Clear(CASTID);
    FreeMem(PCAST6Data(CASTID));
  end;
  Q_ZeroMem(PMixData(ID),SizeOf(TMixData));
  FreeMem(PMixData(ID));
end;

procedure Q_MixHash(const S: string; var Digest: TMixDigest);
var
  ID: TMixID;
begin
  Q_MixHashInit(ID);
  Q_MixHashUpdate(ID,Pointer(S),Length(S));
  Q_MixHashFinal(ID,Digest);
end;

procedure Q_MixHash(P: Pointer; L: Cardinal; var Digest: TMixDigest);
var
  ID: TMixID;
begin
  Q_MixHashInit(ID);
  Q_MixHashUpdate(ID,P,L);
  Q_MixHashFinal(ID,Digest);
end;

procedure Q_MixHash(const SourceDigest: TMixDigest; var Digest: TMixDigest);
var
  ID: TMixID;
begin
  Q_MixHashInit(ID);
  Q_MixHashUpdate(ID,@SourceDigest,SizeOf(TMixDigest));
  Q_MixHashFinal(ID,Digest);
end;

function Q_MixHashSelfTest: Boolean;
var
  ID: TMixID;
  Dig: TMixDigest;
  S: string;
begin
  Q_MixHash('abc',Dig);
  if not(
    (Dig[0]=$13E03225) and (Dig[1]=$C0FCE942) and (Dig[2]=$B2941408) and
    (Dig[3]=$FEBF7552) and (Dig[4]=$13F6E6D9) and (Dig[5]=$5144238D) and
    (Dig[6]=$700CFDDC) and (Dig[7]=$CA0599D2) and (Dig[8]=$53436365) and
    (Dig[9]=$F2FB6032)) then
  begin
    Result := False;
    Exit;
  end;
  Q_MixHash('a',Dig);
  if not(
    (Dig[0]=$A7C1F85E) and (Dig[1]=$2B6FD0D0) and (Dig[2]=$74FB63D2) and
    (Dig[3]=$FD422D6B) and (Dig[4]=$3E16DEC2) and (Dig[5]=$A563C5E7) and
    (Dig[6]=$46FABC5A) and (Dig[7]=$6DD22B0F) and (Dig[8]=$98F8DAFE) and
    (Dig[9]=$0690D2FF)) then
  begin
    Result := False;
    Exit;
  end;
  S := StringOfChar('a',200000);
  Q_MixHashInit(ID);
  Q_MixHashUpdate(ID,Pointer(S),200000);
  Q_MixHashUpdate(ID,Pointer(S),200000);
  Q_MixHashUpdate(ID,Pointer(S),200000);
  Q_MixHashUpdate(ID,Pointer(S),200000);
  Q_MixHashUpdate(ID,Pointer(S),200000);
  Q_MixHashFinal(ID,Dig);
  if not(
    (Dig[0]=$30B87BD5) and (Dig[1]=$39F6CC0A) and (Dig[2]=$63237276) and
    (Dig[3]=$B4E2D835) and (Dig[4]=$29BC7CA2) and (Dig[5]=$1D667478) and
    (Dig[6]=$785C95BD) and (Dig[7]=$A1E4240F) and (Dig[8]=$04B11FA9) and
    (Dig[9]=$C8CEDEDD)) then
  begin
    Result := False;
    Exit;
  end;
  Q_MixHashInit(ID);
  Q_FillRandom(Pointer(S),MixBufferSize,$1DC72E0B);
  Q_MixHashUpdate(ID,Pointer(S),MixBufferSize);
  Q_MixHashFinal(ID,Dig);
  Result :=
    (Dig[0]=$316E05FC) and (Dig[1]=$8EFA384E) and (Dig[2]=$8466B62B) and
    (Dig[3]=$75623B64) and (Dig[4]=$86A49C3F) and (Dig[5]=$02A61F92) and
    (Dig[6]=$12EF2262) and (Dig[7]=$C194B090) and (Dig[8]=$2672360A) and
    (Dig[9]=$B7CE0A39);
end;

type
  PRandData = ^TRandData;
  TRandData = record
    MT: TRandVector;
    Index: Integer;
    SHAID: TSHAID;
    Ps: Integer;
  end;

procedure IntRandInit(P: Pointer; Seed: LongWord; Count: Cardinal);
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EAX
        MOV     EAX,EDX
        MOV     ESI,69069
@@lp:   MOV     [EDI],EAX
        MUL     ESI
        MOV     [EDI+4],EAX
        MUL     ESI
        MOV     [EDI+8],EAX
        MUL     ESI
        MOV     [EDI+12],EAX
        MUL     ESI
        MOV     [EDI+16],EAX
        MUL     ESI
        MOV     [EDI+20],EAX
        MUL     ESI
        MOV     [EDI+24],EAX
        MUL     ESI
        MOV     [EDI+28],EAX
        MUL     ESI
        ADD     EDI,32
        DEC     ECX
        JNE     @@lp
        POP     ESI
        POP     EDI
end;

procedure Q_RandInit(var ID: TMTID; Seed: LongWord); overload;
begin
  GetMem(PRandData(ID),SizeOf(TRandData));
  with PRandData(ID)^ do
  begin
    IntRandInit(@MT,Seed,78);
    Index := 624;
    SHAID := TSHAID(nil);
    Ps := -1;
  end;
end;

procedure Q_RandInit(var ID: TMTID; const InitVector: TRandVector); overload;
begin
  GetMem(PRandData(ID),SizeOf(TRandData));
  with PRandData(ID)^ do
  begin
    Q_CopyLongs(@InitVector,@MT,624);
    Index := 624;
    SHAID := TSHAID(nil);
    Ps := -1;
  end;
end;

procedure IntCAST6RandEncrypt(ID: TCASTID; P: Pointer);
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        PUSH    EDI
        PUSH    EDX
        MOV     EDI,EDX
        MOV     ESI,62
@@lp:   MOV     EAX,EBX
        MOV     EDX,EDI
        CALL    IntCAST6EncryptECB
        MOV     EAX,EBX
        LEA     EDX,[EDI+8]
        CALL    IntCAST6EncryptECB
        MOV     EAX,EBX
        LEA     EDX,[EDI+16]
        CALL    IntCAST6EncryptECB
        MOV     EAX,EBX
        LEA     EDX,[EDI+24]
        CALL    IntCAST6EncryptECB
        MOV     EAX,EBX
        LEA     EDX,[EDI+32]
        CALL    IntCAST6EncryptECB
        ADD     EDI,40
        DEC     ESI
        JNE     @@lp
        MOV     EAX,EBX
        MOV     EDX,EDI
        CALL    IntCAST6EncryptECB
        POP     ESI
        LEA     EDX,[ESP-16]
        MOV     ESP,EDX
        MOV     EAX,[EDI+8]
        MOV     [EDX],EAX
        MOV     EAX,[EDI+12]
        MOV     [EDX+4],EAX
        MOV     EAX,[ESI]
        MOV     [EDX+8],EAX
        MOV     EAX,[ESI+4]
        MOV     [EDX+12],EAX
        MOV     EAX,EBX
        MOV     EBX,EDX
        CALL    IntCAST6EncryptECB
        MOV     EAX,[EBX]
        MOV     [EDI+8],EAX
        MOV     EAX,[EBX+4]
        MOV     [EDI+12],EAX
        MOV     EAX,[EBX+8]
        MOV     [ESI],EAX
        MOV     EAX,[EBX+12]
        MOV     [ESI+4],EAX
        XOR     EAX,EAX
        MOV     [EBX],EAX
        MOV     [EBX+4],EAX
        MOV     [EBX+8],EAX
        MOV     [EBX+12],EAX
        ADD     ESP,16
        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure Q_RandCAST6Update(ID: TMTID; const S: string); overload;
var
  L: LongWord;
  CsID: TCASTID;
  P: PByte;
begin
  L := Length(S);
  if L <> 0 then
  begin
    P := Pointer(S);
    GetMem(PCAST6Data(CsID),SizeOf(TCAST6Data));
    while L > 32 do
    begin
      IntCAST6KeySchedule(CsID,P,32);
      IntCAST6RandEncrypt(CsID,PRandData(ID));
      Inc(P,32);
      Dec(L,32);
    end;
    IntCAST6KeySchedule(CsID,P,L);
    with PRandData(ID)^ do
    begin
      IntCAST6RandEncrypt(CsID,@MT);
      Index := 624;
      Ps := -1;
    end;
    Q_CAST6Done(CsID);
  end;
end;

procedure Q_RandCAST6Update(ID: TMTID; P: Pointer; L: Cardinal); overload;
var
  CsID: TCASTID;
begin
  if L <> 0 then
  begin
    GetMem(PCAST6Data(CsID),SizeOf(TCAST6Data));
    while L > 32 do
    begin
      IntCAST6KeySchedule(CsID,P,32);
      IntCAST6RandEncrypt(CsID,PRandData(ID));
      Inc(PByte(P),32);
      Dec(L,32);
    end;
    IntCAST6KeySchedule(CsID,P,L);
    with PRandData(ID)^ do
    begin
      IntCAST6RandEncrypt(CsID,@MT);
      Index := 624;
      Ps := -1;
    end;
    Q_CAST6Done(CsID);
  end;
end;

procedure Q_RandCAST6Update(ID: TMTID; const Digest: TSHA1Digest); overload;
var
  CsID: TCASTID;
begin
  Q_CAST6Init(CsID,@Digest,SizeOf(TSHA1Digest));
  with PRandData(ID)^ do
  begin
    IntCAST6RandEncrypt(CsID,@MT);
    Index := 624;
    Ps := -1;
  end;
  Q_CAST6Done(CsID);
end;

procedure IntRC6RandEncrypt(ID: TRC6ID; P: Pointer);
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        PUSH    EDI
        PUSH    EDX
        MOV     EDI,EDX
        MOV     ESI,62
@@lp:   MOV     EAX,EBX
        MOV     EDX,EDI
        CALL    IntRC6EncryptECB
        MOV     EAX,EBX
        LEA     EDX,[EDI+8]
        CALL    IntRC6EncryptECB
        MOV     EAX,EBX
        LEA     EDX,[EDI+16]
        CALL    IntRC6EncryptECB
        MOV     EAX,EBX
        LEA     EDX,[EDI+24]
        CALL    IntRC6EncryptECB
        MOV     EAX,EBX
        LEA     EDX,[EDI+32]
        CALL    IntRC6EncryptECB
        ADD     EDI,40
        DEC     ESI
        JNE     @@lp
        MOV     EAX,EBX
        MOV     EDX,EDI
        CALL    IntRC6EncryptECB
        POP     ESI
        LEA     EDX,[ESP-16]
        MOV     ESP,EDX
        MOV     EAX,[EDI+8]
        MOV     [EDX],EAX
        MOV     EAX,[EDI+12]
        MOV     [EDX+4],EAX
        MOV     EAX,[ESI]
        MOV     [EDX+8],EAX
        MOV     EAX,[ESI+4]
        MOV     [EDX+12],EAX
        MOV     EAX,EBX
        MOV     EBX,EDX
        CALL    IntRC6EncryptECB
        MOV     EAX,[EBX]
        MOV     [EDI+8],EAX
        MOV     EAX,[EBX+4]
        MOV     [EDI+12],EAX
        MOV     EAX,[EBX+8]
        MOV     [ESI],EAX
        MOV     EAX,[EBX+12]
        MOV     [ESI+4],EAX
        XOR     EAX,EAX
        MOV     [EBX],EAX
        MOV     [EBX+4],EAX
        MOV     [EBX+8],EAX
        MOV     [EBX+12],EAX
        ADD     ESP,16
        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure Q_RandRC6Update(ID: TMTID; const S: string); overload;
var
  L: LongWord;
  RcID: TRC6ID;
  P: PByte;
begin
  L := Length(S);
  if L <> 0 then
  begin
    P := Pointer(S);
    GetMem(PRC6Data(RcID),SizeOf(TRC6Data));
    while L > 48 do
    begin
      IntRC6Init(RcID,P,48);
      IntRC6RandEncrypt(RcID,PRandData(ID));
      Inc(P,48);
      Dec(L,48);
    end;
    IntRC6Init(RcID,P,L);
    with PRandData(ID)^ do
    begin
      IntRC6RandEncrypt(RcID,@MT);
      Index := 624;
      Ps := -1;
    end;
    Q_RC6Done(RcID);
  end;
end;

procedure Q_RandRC6Update(ID: TMTID; P: Pointer; L: Cardinal); overload;
var
  RcID: TRC6ID;
begin
  if L <> 0 then
  begin
    GetMem(PRC6Data(RcID),SizeOf(TRC6Data));
    while L > 48 do
    begin
      IntRC6Init(RcID,P,48);
      IntRC6RandEncrypt(RcID,PRandData(ID));
      Q_RC6Done(RcID);
      Inc(PByte(P),48);
      Dec(L,48);
    end;
    IntRC6Init(RcID,P,L);
    with PRandData(ID)^ do
    begin
      IntRC6RandEncrypt(RcID,@MT);
      Index := 624;
      Ps := -1;
    end;
    Q_RC6Done(RcID);
  end;
end;

procedure Q_RandRC6Update(ID: TMTID; const Digest: TSHA1Digest); overload;
var
  RcID: TRC6ID;
begin
  Q_RC6Init(RcID,@Digest,SizeOf(TSHA1Digest));
  with PRandData(ID)^ do
  begin
    IntRC6RandEncrypt(RcID,@MT);
    Index := 624;
    Ps := -1;
  end;
  Q_RC6Done(RcID);
end;

procedure Q_RandRC6Update(ID: TMTID; const Digest: TMixDigest); overload;
var
  RcID: TRC6ID;
begin
  Q_RC6Init(RcID,@Digest,SizeOf(TMixDigest));
  with PRandData(ID)^ do
  begin
    IntRC6RandEncrypt(RcID,@MT);
    Index := 624;
    Ps := -1;
  end;
  Q_RC6Done(RcID);
end;

procedure Q_RandGetVector(ID: TMTID; var Vector: TRandVector);
begin
  with PRandData(ID)^ do
  begin
    Q_CopyLongs(@MT,@Vector,624);
    Index := 624;
    Ps := -1;
  end;
end;

procedure Q_RandSetVector(ID: TMTID; const Vector: TRandVector);
begin
  with PRandData(ID)^ do
  begin
    Q_CopyLongs(@Vector,@MT,624);
    Index := 624;
    Ps := -1;
  end;
end;

function Q_RandNext(ID: TMTID): LongWord;
asm
        MOV     ECX,[EAX].TRandData.Index
        CMP     ECX,624
        JE      @@mk
@@nx:   MOV     EDX,[EAX+ECX*4]
        INC     ECX
        MOV     [EAX].TRandData.Index,ECX
        MOV     EAX,EDX
        SHR     EDX,11
        XOR     EAX,EDX
        MOV     EDX,EAX
        SHL     EAX,7
        AND     EAX,$9D2C5680
        XOR     EDX,EAX
        MOV     EAX,EDX
        SHL     EDX,15
        AND     EDX,$EFC60000
        XOR     EDX,EAX
        MOV     EAX,EDX
        SHR     EDX,18
        XOR     EAX,EDX
        RET
@@ku:   DD      0,$9908B0DF
@@mk:   PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EDI,EAX
        MOV     ECX,227
        PUSH    EAX
@@lp1:  MOV     EAX,[EDI]
        MOV     EDX,[EDI+4]
        AND     EAX,$80000000
        AND     EDX,$7FFFFFFF
        OR      EAX,EDX
        MOV     EDX,EAX
        MOV     EBX,[EDI+1588]
        SHR     EAX,1
        AND     EDX,1
        XOR     EBX,EAX
        XOR     EBX,DWORD PTR @@ku[EDX*4]
        MOV     [EDI],EBX
        ADD     EDI,4
        DEC     ECX
        JNE     @@lp1
        MOV     ECX,198
        MOV     EAX,[EDI]
@@lp2:  MOV     EDX,[EDI+4]
        MOV     ESI,EDX
        AND     EDX,$7FFFFFFF
        AND     EAX,$80000000
        OR      EAX,EDX
        MOV     EBX,[EDI-908]
        MOV     EDX,EAX
        AND     EDX,1
        SHR     EAX,1
        XOR     EBX,EAX
        XOR     EBX,DWORD PTR @@ku[EDX*4]
        MOV     [EDI],EBX
        MOV     EDX,[EDI+8]
        MOV     EAX,EDX
        AND     EDX,$7FFFFFFF
        AND     ESI,$80000000
        OR      ESI,EDX
        MOV     EBX,[EDI-904]
        MOV     EDX,ESI
        AND     EDX,1
        SHR     ESI,1
        XOR     EBX,ESI
        XOR     EBX,DWORD PTR @@ku[EDX*4]
        MOV     [EDI+4],EBX
        ADD     EDI,8
        DEC     ECX
        JNE     @@lp2
        AND     EAX,$80000000
        POP     EDI
        MOV     EDX,[EDI]
        AND     EDX,$7FFFFFFF
        OR      EAX,EDX
        MOV     EBX,[EDI+1584]
        MOV     EDX,EAX
        AND     EDX,1
        SHR     EAX,1
        XOR     EBX,EAX
        XOR     EBX,DWORD PTR @@ku[EDX*4]
        MOV     [EDI+2492],EBX
        MOV     EAX,EDI
        POP     EBX
        POP     ESI
        POP     EDI
        JMP     @@nx
end;

function Q_RandUniform(ID: TMTID): Extended;
const
  InvMax: Double = 1/$4000000000000000;
asm
        PUSH    EAX
        CALL    Q_RandNext
        AND     EAX,$3FFFFFFF
        MOV     EDX,[ESP]
        MOV     [ESP],EAX
        MOV     EAX,EDX
        CALL    Q_RandNext
        PUSH    EAX
        FILD    QWORD PTR [ESP]
        ADD     ESP,8
        FMUL    InvMax
end;

function Q_RandUInt32(ID: TMTID; Range: Cardinal): Cardinal;
asm
        PUSH    EDX
        CALL    Q_RandNext
        POP     EDX
        MUL     EDX
        MOV     EAX,EDX
end;

function Q_RandUInt64(ID: TMTID; Range: Int64): Int64;
var
  X: array[0..1] of LongWord;
  C: Comp absolute X;
begin
  X[0] := Q_RandNext(ID);
  X[1] := Q_RandNext(ID) and $7FFFFFFF;
  Result := Round(C-Int(C/Range)*Range);
end;

function Q_RandGauss(ID: TMTID; ExtraNumber: Pointer): Extended;
const
  InvMax: Double = 1/$2000000000000000;
  XX1: Double = 1;
  XX2: Double = -2;
asm
        PUSH    EBX
        PUSH    EDI
        PUSH    ESI
        LEA     EDI,[ESP-24]
        SUB     ESP,32
        AND     EDI,$FFFFFFF8
        PUSH    EDX
        MOV     EBX,EAX
@@lp:   MOV     EAX,EBX
        CALL    Q_RandNext
        AND     EAX,$3FFFFFFF
        MOV     ESI,EAX
        MOV     EAX,EBX
        CALL    Q_RandNext
        PUSH    ESI
        PUSH    EAX
        FILD    QWORD PTR [ESP]
        FMUL    InvMax
        FSUB    XX1
        FST     QWORD PTR [EDI+8]
        MOV     EAX,EBX
        FMUL    ST(0),ST(0)
        CALL    Q_RandNext
        AND     EAX,$3FFFFFFF
        MOV     ESI,EAX
        MOV     EAX,EBX
        CALL    Q_RandNext
        MOV     [ESP],EAX
        MOV     [ESP+4],ESI
        FILD    QWORD PTR [ESP]
        FMUL    InvMax
        FSUB    XX1
        ADD     ESP,8
        FST     QWORD PTR [EDI+16]
        FMUL    ST(0),ST(0)
        FADDP
        FCOM    XX1
        FSTSW   AX
        FSTP    QWORD PTR [EDI]
        SAHF
        JNB     @@lp
        FLDLN2
        FLD     QWORD PTR [EDI]
        FYL2X
        FMUL    XX2
        FDIV    QWORD PTR [EDI]
        POP     EDX
        FSQRT
        TEST    EDX,EDX
        JE      @@qt
        FLD     ST(0)
        FMUL    QWORD PTR [EDI+16]
        FSTP    QWORD PTR [EDX]
@@qt:   FMUL    QWORD PTR [EDI+8]
        ADD     ESP,32
        POP     ESI
        POP     EDI
        POP     EBX
end;

function Q_SecureRandNext(ID: TMTID): LongWord;
begin
  with PRandData(ID)^ do
  begin
    if Ps >= 0 then
    begin
      with PSHA1Data(SHAID)^ do
        Result := Hash[Ps];
      Dec(Ps);
    end else
    begin
      if Pointer(SHAID) <> nil then
        IntSHA1Init(SHAID)
      else
        Q_SHA1Init(SHAID);
      with PSHA1Data(SHAID)^ do
      begin
        PLong(@Buffer[0])^ := Q_RandNext(ID);
        PLong(@Buffer[4])^ := Q_RandNext(ID);
        PLong(@Buffer[8])^ := Q_RandNext(ID);
        PLong(@Buffer[12])^ := Q_RandNext(ID);
        PLong(@Buffer[16])^ := Q_RandNext(ID);
        PLong(@Buffer[20])^ := Q_RandNext(ID);
        PLong(@Buffer[24])^ := Q_RandNext(ID);
        PLong(@Buffer[28])^ := Q_RandNext(ID);
        PLong(@Buffer[32])^ := Q_RandNext(ID);
        PLong(@Buffer[36])^ := Q_RandNext(ID);
        PLong(@Buffer[40])^ := Q_RandNext(ID);
        PLong(@Buffer[44])^ := Q_RandNext(ID);
        PLong(@Buffer[48])^ := Q_RandNext(ID);
        PLong(@Buffer[52])^ := Q_RandNext(ID);
        Buffer[55] := $80;
        Buffer[62] := $01;
        Buffer[63] := $B8;
        IntSHA1Compress(SHAID);
        Result := Hash[4];
      end;
      Ps := 3;
    end;
  end;
end;

procedure Q_RandFill(ID: TMTID; P: Pointer; L: Cardinal);
asm
        PUSH    EBX
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,ECX
        AND     ECX,7
        MOV     ESI,EAX
        MOV     EBX,EDX
        PUSH    ECX
        SHR     EDI,3
        JE      @@nx
@@lp:   MOV     EAX,ESI
        CALL    Q_RandNext
        MOV     [EBX],EAX
        ADD     EBX,4
        MOV     EAX,ESI
        CALL    Q_RandNext
        MOV     [EBX],EAX
        ADD     EBX,4
        DEC     EDI
        JNE     @@lp
@@nx:   POP     ECX
        JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@qt, @@t1, @@t2, @@t3
        DD      @@t4, @@t5, @@t6, @@t7
@@t1:   MOV     EAX,ESI
        CALL    Q_RandNext
        MOV     BYTE PTR [EBX],AL
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t2:   MOV     EAX,ESI
        CALL    Q_RandNext
        MOV     WORD PTR [EBX],AX
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t3:   MOV     EAX,ESI
        CALL    Q_RandNext
        MOV     WORD PTR [EBX],AX
        SHR     EAX,16
        MOV     BYTE PTR [EBX+2],AL
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t4:   MOV     EAX,ESI
        CALL    Q_RandNext
        MOV     [EBX],EAX
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t5:   MOV     EAX,ESI
        CALL    Q_RandNext
        MOV     [EBX],EAX
        MOV     EAX,ESI
        CALL    Q_RandNext
        MOV     BYTE PTR [EBX+4],AL
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t6:   MOV     EAX,ESI
        CALL    Q_RandNext
        MOV     [EBX],EAX
        MOV     EAX,ESI
        CALL    Q_RandNext
        MOV     WORD PTR [EBX+4],AX
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t7:   MOV     EAX,ESI
        CALL    Q_RandNext
        MOV     [EBX],EAX
        MOV     EAX,ESI
        CALL    Q_RandNext
        MOV     WORD PTR [EBX+4],AX
        SHR     EAX,16
        MOV     BYTE PTR [EBX+6],AL
@@qt:   POP     ESI
        POP     EDI
        POP     EBX
end;

procedure Q_SecureRandFill(ID: TMTID; P: Pointer; L: Cardinal);
asm
        PUSH    EBX
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,ECX
        MOV     EBX,EDX
        AND     ECX,7
        MOV     ESI,EAX
        PUSH    ECX
        SHR     EDI,3
        JE      @@nx
@@lp:   MOV     EAX,ESI
        CALL    Q_SecureRandNext
        MOV     [EBX],EAX
        ADD     EBX,4
        MOV     EAX,ESI
        CALL    Q_SecureRandNext
        MOV     [EBX],EAX
        ADD     EBX,4
        DEC     EDI
        JNE     @@lp
@@nx:   POP     ECX
        JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@qt, @@t1, @@t2, @@t3
        DD      @@t4, @@t5, @@t6, @@t7
@@t1:   MOV     EAX,ESI
        CALL    Q_SecureRandNext
        MOV     BYTE PTR [EBX],AL
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t2:   MOV     EAX,ESI
        CALL    Q_SecureRandNext
        MOV     WORD PTR [EBX],AX
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t3:   MOV     EAX,ESI
        CALL    Q_SecureRandNext
        MOV     WORD PTR [EBX],AX
        SHR     EAX,16
        MOV     BYTE PTR [EBX+2],AL
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t4:   MOV     EAX,ESI
        CALL    Q_SecureRandNext
        MOV     [EBX],EAX
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t5:   MOV     EAX,ESI
        CALL    Q_SecureRandNext
        MOV     [EBX],EAX
        MOV     EAX,ESI
        CALL    Q_SecureRandNext
        MOV     BYTE PTR [EBX+4],AL
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t6:   MOV     EAX,ESI
        CALL    Q_SecureRandNext
        MOV     [EBX],EAX
        MOV     EAX,ESI
        CALL    Q_SecureRandNext
        MOV     WORD PTR [EBX+4],AX
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t7:   MOV     EAX,ESI
        CALL    Q_SecureRandNext
        MOV     [EBX],EAX
        MOV     EAX,ESI
        CALL    Q_SecureRandNext
        MOV     WORD PTR [EBX+4],AX
        SHR     EAX,16
        MOV     BYTE PTR [EBX+6],AL
@@qt:   POP     ESI
        POP     EDI
        POP     EBX
end;

procedure Q_SecureRandXOR(ID: TMTID; P: Pointer; L: Cardinal);
asm
        PUSH    EBX
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,ECX
        MOV     EBX,EDX
        AND     ECX,7
        MOV     ESI,EAX
        PUSH    ECX
        SHR     EDI,3
        JE      @@nx
@@lp:   MOV     EAX,ESI
        CALL    Q_SecureRandNext
        XOR     [EBX],EAX
        ADD     EBX,4
        MOV     EAX,ESI
        CALL    Q_SecureRandNext
        XOR     [EBX],EAX
        ADD     EBX,4
        DEC     EDI
        JNE     @@lp
@@nx:   POP     ECX
        JMP     DWORD PTR @@tV[ECX*4]
@@tV:   DD      @@qt, @@t1, @@t2, @@t3
        DD      @@t4, @@t5, @@t6, @@t7
@@t1:   MOV     EAX,ESI
        CALL    Q_SecureRandNext
        XOR     BYTE PTR [EBX],AL
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t2:   MOV     EAX,ESI
        CALL    Q_SecureRandNext
        XOR     WORD PTR [EBX],AX
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t3:   MOV     EAX,ESI
        CALL    Q_SecureRandNext
        XOR     WORD PTR [EBX],AX
        SHR     EAX,16
        XOR     BYTE PTR [EBX+2],AL
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t4:   MOV     EAX,ESI
        CALL    Q_SecureRandNext
        XOR     [EBX],EAX
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t5:   MOV     EAX,ESI
        CALL    Q_SecureRandNext
        XOR     [EBX],EAX
        MOV     EAX,ESI
        CALL    Q_SecureRandNext
        XOR     BYTE PTR [EBX+4],AL
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t6:   MOV     EAX,ESI
        CALL    Q_SecureRandNext
        XOR     [EBX],EAX
        MOV     EAX,ESI
        CALL    Q_SecureRandNext
        XOR     WORD PTR [EBX+4],AX
        POP     ESI
        POP     EDI
        POP     EBX
        RET
@@t7:   MOV     EAX,ESI
        CALL    Q_SecureRandNext
        XOR     [EBX],EAX
        MOV     EAX,ESI
        CALL    Q_SecureRandNext
        XOR     WORD PTR [EBX+4],AX
        SHR     EAX,16
        XOR     BYTE PTR [EBX+6],AL
@@qt:   POP     ESI
        POP     EDI
        POP     EBX
end;

procedure Q_RandDone(ID: TMTID);
begin
  with PRandData(ID)^ do
    if SHAID <> TSHAID(nil) then
    begin
      IntSHA1Clear(SHAID);
      FreeMem(PSHA1Data(SHAID));
    end;
  Q_ZeroMem(PRandData(ID),SizeOf(TRandData));
  FreeMem(PRandData(ID));
end;

type
  PDH4253Data = ^TDH4253Data;
  TDH4253Data = array[0..265] of LongWord;

procedure IntDH4253_SPLIT(X, Y: PDH4253Data);
asm
        LEA     ESI,[EAX+532]
        MOV     ECX,33
        MOV     EAX,[ESI-4]
        MOV     EBX,EAX
        AND     EAX,$1FFFFFFF
        MOV     [ESI-4],EAX
        SHR     EBX,29
@@lp:   MOV     EAX,[ESI]
        MOV     EDI,EAX
        SHL     EAX,3
        OR      EAX,EBX
        SHR     EDI,29
        MOV     [EDX],EAX
        MOV     EAX,[ESI+4]
        MOV     EBX,EAX
        SHL     EAX,3
        OR      EAX,EDI
        SHR     EBX,29
        MOV     [EDX+4],EAX
        MOV     EAX,[ESI+8]
        MOV     EDI,EAX
        SHL     EAX,3
        OR      EAX,EBX
        SHR     EDI,29
        MOV     [EDX+8],EAX
        MOV     EAX,[ESI+12]
        MOV     EBX,EAX
        SHL     EAX,3
        OR      EAX,EDI
        SHR     EBX,29
        MOV     [EDX+12],EAX
        ADD     ESI,16
        ADD     EDX,16
        DEC     ECX
        JNE     @@lp
        MOV     EAX,[ESI]
        SHL     EAX,3
        OR      EAX,EBX
        MOV     [EDX],EAX
end;

procedure IntDH4253_ADD(X, Y: PDH4253Data);
asm
	MOV	EDI,EAX
	MOV	ESI,EDX
	XOR	EBX,EBX
	MOV	ECX,19
@@lp1:  SHR	EBX,1
        MOV     EBP,[EDI]
        ADC     EBP,[ESI]
        MOV     [EDI],EBP
        MOV     EBP,[EDI+4]
        ADC     EBP,[ESI+4]
        MOV     [EDI+4],EBP
        MOV     EBP,[EDI+8]
        ADC     EBP,[ESI+8]
        MOV     [EDI+8],EBP
        MOV     EBP,[EDI+12]
        ADC     EBP,[ESI+12]
        MOV     [EDI+12],EBP
        MOV     EBP,[EDI+16]
        ADC     EBP,[ESI+16]
        MOV     [EDI+16],EBP
        MOV     EBP,[EDI+20]
        ADC     EBP,[ESI+20]
        MOV     [EDI+20],EBP
        MOV     EBP,[EDI+24]
        ADC     EBP,[ESI+24]
        MOV     [EDI+24],EBP
	ADC	EBX,0
	ADD	EDI,28
	ADD	ESI,28
	DEC	ECX
	JNE	@@lp1
	MOV	ESI,[EAX+528]
	TEST	ESI,$20000000
	JNE	@@sb
	XOR	ESI,$1FFFFFFF
	JNE	@@qt
	MOV	ECX,131
@@lp2:	MOV	ESI,[EAX+ECX*4]
	XOR	ESI,$FFFFFFFF
	JNE	@@qt
	DEC	ECX
        JNS	@@lp2
@@sb:	MOV	ECX,22
        XOR     EDX,EDX
	XOR	EBX,EBX
        NOT     EDX
@@lp3:	SHR	EBX,1
	MOV	EBP,[EAX]
	SBB	EBP,EDX
	MOV	[EAX],EBP
	MOV	EBP,[EAX+4]
	SBB	EBP,EDX
	MOV	[EAX+4],EBP
	MOV	EBP,[EAX+8]
	SBB	EBP,EDX
	MOV	[EAX+8],EBP
	MOV	EBP,[EAX+12]
	SBB	EBP,EDX
	MOV	[EAX+12],EBP
	MOV	EBP,[EAX+16]
	SBB	EBP,EDX
	MOV	[EAX+16],EBP
	MOV	EBP,[EAX+20]
	SBB	EBP,EDX
	MOV	[EAX+20],EBP
	ADC	EBX,0
	ADD	EAX,24
	DEC	ECX
	JNE	@@lp3
	MOV	EBP,[EAX]
	SUB	EBP,$1FFFFFFF
        SUB     EBP,EBX
	MOV	[EAX],EBP
@@qt:
end;

procedure IntDH4253_MUL(X, Y, Z, T: PDH4253Data);
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        PUSH    EBP
        PUSH    EAX
        PUSH    EDX
        MOV     ESI,EDX
        MOV     EDI,ECX
        PUSH    ECX
        MOV     EBP,[EAX]
        MOV     ECX,19
        XOR     EBX,EBX
@@lp1:  MOV     EAX,[ESI]
        MUL     EBP
        ADD     EAX,EBX
        MOV     EBX,EDX
        MOV     [EDI],EAX
        ADC     EBX,0
        MOV     EAX,[ESI+4]
        MUL     EBP
        ADD     EAX,EBX
        MOV     EBX,EDX
        MOV     [EDI+4],EAX
        ADC     EBX,0
        MOV     EAX,[ESI+8]
        MUL     EBP
        ADD     EAX,EBX
        MOV     EBX,EDX
        MOV     [EDI+8],EAX
        ADC     EBX,0
        MOV     EAX,[ESI+12]
        MUL     EBP
        ADD     EAX,EBX
        MOV     EBX,EDX
        MOV     [EDI+12],EAX
        ADC     EBX,0
        MOV     EAX,[ESI+16]
        MUL     EBP
        ADD     EAX,EBX
        MOV     EBX,EDX
        MOV     [EDI+16],EAX
        ADC     EBX,0
        MOV     EAX,[ESI+20]
        MUL     EBP
        ADD     EAX,EBX
        MOV     EBX,EDX
        MOV     [EDI+20],EAX
        ADC     EBX,0
        MOV     EAX,[ESI+24]
        MUL     EBP
        ADD     EAX,EBX
        MOV     EBX,EDX
        MOV     [EDI+24],EAX
        ADC     EBX,0
        ADD     ESI,28
        ADD     EDI,28
        DEC     ECX
        JNE     @@lp1
        MOV     [EDI],EBX
        LEA     EAX,[EDI+4]
        XOR     EDX,EDX
        CALL    IntFill32
        ADD     EAX,128
        CALL    IntFill32
        ADD     EAX,128
        CALL    IntFill32
        ADD     EAX,128
        CALL    IntFill32
        MOV     [EAX+128],EDX
        MOV     [EAX+132],EDX
        MOV     [EAX+136],EDX
        MOV     [EAX+140],EDX
        PUSH    EDX
        MOV     EAX,132
        PUSH    EAX
@@lp2:  MOV     EAX,[ESP+4]
        MOV     EDX,[ESP+16]
        ADD     EAX,4
        MOV     EBP,[EDX+EAX]
        MOV     [ESP+4],EAX
        MOV     ESI,[ESP+12]
        MOV     EDI,[ESP+44]
        MOV     ECX,19
        XOR     EBX,EBX
@@lp3:  MOV     EAX,[ESI]
        MUL     EBP
        ADD     EAX,EBX
        MOV     EBX,EDX
        MOV     [EDI],EAX
        ADC     EBX,0
        MOV     EAX,[ESI+4]
        MUL     EBP
        ADD     EAX,EBX
        MOV     EBX,EDX
        MOV     [EDI+4],EAX
        ADC     EBX,0
        MOV     EAX,[ESI+8]
        MUL     EBP
        ADD     EAX,EBX
        MOV     EBX,EDX
        MOV     [EDI+8],EAX
        ADC     EBX,0
        MOV     EAX,[ESI+12]
        MUL     EBP
        ADD     EAX,EBX
        MOV     EBX,EDX
        MOV     [EDI+12],EAX
        ADC     EBX,0
        MOV     EAX,[ESI+16]
        MUL     EBP
        ADD     EAX,EBX
        MOV     EBX,EDX
        MOV     [EDI+16],EAX
        ADC     EBX,0
        MOV     EAX,[ESI+20]
        MUL     EBP
        ADD     EAX,EBX
        MOV     EBX,EDX
        MOV     [EDI+20],EAX
        ADC     EBX,0
        MOV     EAX,[ESI+24]
        MUL     EBP
        ADD     EAX,EBX
        MOV     EBX,EDX
        MOV     [EDI+24],EAX
        ADC     EBX,0
        ADD     ESI,28
        ADD     EDI,28
        DEC     ECX
        JNE     @@lp3
        MOV     [EDI],EBX
        MOV     ESI,[ESP+44]
        MOV     EDI,[ESP+8]
        XOR     EBX,EBX
        MOV     EAX,[ESP+4]
        MOV     ECX,19
        ADD     EDI,EAX
@@lp4:  SHR     EBX,1
        MOV     EAX,[EDI]
        ADC     EAX,[ESI]
        MOV     [EDI],EAX
        MOV     EAX,[EDI+4]
        ADC     EAX,[ESI+4]
        MOV     [EDI+4],EAX
        MOV     EAX,[EDI+8]
        ADC     EAX,[ESI+8]
        MOV     [EDI+8],EAX
        MOV     EAX,[EDI+12]
        ADC     EAX,[ESI+12]
        MOV     [EDI+12],EAX
        MOV     EAX,[EDI+16]
        ADC     EAX,[ESI+16]
        MOV     [EDI+16],EAX
        MOV     EAX,[EDI+20]
        ADC     EAX,[ESI+20]
        MOV     [EDI+20],EAX
        MOV     EAX,[EDI+24]
        ADC     EAX,[ESI+24]
        MOV     [EDI+24],EAX
	ADC	EBX,0
	ADD	ESI,28
	ADD	EDI,28
        DEC     ECX
        JNE     @@lp4
        MOV     EAX,[EDI]
        ADD     EAX,[ESI]
        ADD     EAX,EBX
        MOV     [EDI],EAX
        DEC     DWORD PTR [ESP]
        JNE     @@lp2
        MOV     EAX,[ESP+8]
        MOV     EDX,[ESP+44]
        CALL    IntDH4253_SPLIT
        MOV     EAX,[ESP+8]
        MOV     EDX,[ESP+44]
        CALL    IntDH4253_ADD
        ADD     ESP,20
        POP     EBP
        POP     EBX
        POP     EDI
        POP     ESI
end;

procedure Q_DHCreatePublicKey(G, PrivateKey: PDHKey4253; PublicKey: PDHKey4253);
type
  PDH4253Arr = ^TDH4253Arr;
  TDH4253Arr = array[0..3] of TDH4253Data;
var
  P: PDH4253Arr;
  PPE,PPY,PPQ,PPD: PDH4253Data;
  I: Integer;
begin
  GetMem(P,SizeOf(TDH4253Arr));
  PPE := @P[0];
  PPY := @P[1];
  PPQ := @P[2];
  PPD := @P[3];
  Q_CopyLongs(G,PPQ,133);
  PPQ^[132] := PPQ^[132] and $1FFFFFFF;
  if PrivateKey^[0] and 1 = 0 then
  begin
    Q_FillLong(0,@PPY^[1],132);
    PPY^[0] := 1;
  end else
    Q_CopyLongs(PPQ,PPY,133);
  for I := 1 to 4252 do
  begin
    IntDH4253_MUL(PPQ,PPQ,PPE,PPD);
    Q_Exchange(PPQ,PPE);
    if Q_BitTest(PrivateKey,I) then
    begin
      IntDH4253_MUL(PPY,PPQ,PPE,PPD);
      Q_Exchange(PPY,PPE);
    end;
  end;
  Q_CopyLongs(PPY,PublicKey,133);
  Q_FillLong(0,P,1064);
  FreeMem(P);
end;

procedure Q_DHGetCipherKey(PublicKey, PrivateKey: PDHKey4253;
  CipherKey: Pointer; KeySize: Integer);
var
  InterKey: TDHKey4253;
  ID: TMTID;
begin
  Q_DHCreatePublicKey(PublicKey,PrivateKey,@InterKey);
  Q_RandInit(ID);
  Q_RandCAST6Update(ID,@InterKey,532);
  Q_FillLong(0,@InterKey,133);
  Q_SecureRandFill(ID,CipherKey,KeySize);
  Q_RandDone(ID);
end;

function Q_DHSelfTest: Boolean;
type
  TSampleKey = array[0..7] of LongWord;
const
  S: TSampleKey =
    ($5B7C29C9,$5CA4433F,$B3AF3C32,$4861E34C,$0CCA7650,$A3625CCD,$F40D998A,$0A92E7D1);
var
  A,B,C,Y: TDHKey4253;
  K: TSampleKey;
begin
  Q_FillRandom(@A,532,$AB3F609D);
  Q_FillRandom(@B,532,$50D12BE2);
  Q_FillRandom(@C,532,$E6583F27);
  Q_DHCreatePublicKey(@A,@B,@Y);
  Q_DHGetCipherKey(@Y,@C,@K,32);
  Result := Q_CompLongs(@K,@S,8);
end;

{$IFDEF USE_DYNAMIC_TABLES}

initialization
  Int256Chars(@ToUpperChars);
  CharToOemBuff(@ToUpperChars,@ToOemChars,256);
  OemToCharBuff(@ToUpperChars,@ToAnsiChars,256);
  CharUpperBuff(@ToUpperChars,256);
  Int256Chars(@ToLowerChars);
  CharLowerBuff(@ToLowerChars,256);

{$ENDIF}

end.
