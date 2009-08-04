%%% File    : ewg.hrl
%%% Author  : Matteo Redaelli <matteo.redaelli@libero.it>
%%% Description : 
%%% Created :  3 Aug 2009 by Matteo Redaelli <matteo.redaelli@libero.it>


% Validator options
-define(CHARACTERS, "acb").
-define(MIN_LENGTH, 1).
-define(MAX_LENGTH, 4).

-define(CHAR_OCCURS, [{"a",1,4},{"bc",0,3}]).

-define(MAX_CONSECUTIVE_CHAR_OCCURS, [{"a",1},{"bc",2}]).

% Dumper options

-define(WORDS_FILE, "/tmp/words.txt").
-define(PREFIX, "prefix").
-define(POSTFIX, "postfix").


