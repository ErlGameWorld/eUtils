-type matchExpression() :: [ matchFunction(), ... ].

-type matchFunction() :: { matchHead(), matchConditions(), matchBody()}.

-type matchHead() :: matchVariable() | '_' | { matchHeadPart(), ...}.

-type matchHeadPart() :: term() | matchVariable() | '_'.

-type matchVariable() :: '$<number>'.

-type matchConditions() :: [ matchCondition(), ...] | [].

-type matchCondition() :: { guardFunction() } | { guardFunction(), conditionExpression(), ... }.
-type boolFunction() :: is_atom | is_float | is_integer | is_list | is_number | is_pid | is_port | is_reference | is_tuple | is_map | map_is_key | is_binary | is_function | is_record | 'and' | 'or' | 'not' | 'xor' | 'andalso' | 'orelse'.
-type conditionExpression() :: exprMatchVariable() | { guardFunction() } | { guardFunction(), conditionExpression(), ... } | termConstruct().
-type exprMatchVariable() :: matchVariable() (bound in the MatchHead) | '$_' | '$$'
-type termConstruct():: {{}} | {{ conditionExpression(), ... }} | [] | [conditionExpression(), ...] | #{} | #{term() => conditionExpression(), ...} | nonCompositeTerm() | constant().
-type nonCompositeTerm() :: term(). %%   (not list or tuple or map)
-type constant() :: {const, term()}.
-type guardFunction() :: boolFunction() | abs | element | hd | length | map_get | map_size | node | round | size | bit_size | tl | trunc | '+' | '-' | '*' | 'div' | 'rem' | 'band' | 'bor' | 'bxor' | 'bnot' | 'bsl' | 'bsr' | '>' | '>=' | '<' | '=<' | '=:=' | '==' | '=/=' | '/=' | self.
-type matchBody() :: [ conditionExpression(), ... ].