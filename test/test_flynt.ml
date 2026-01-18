let () =
	Alcotest.run "flynt" [
		"trie", Test_trie.tests;
		"tokens", Test_token.tests;
		"reader", Test_reader.tests;
		"lexer", Test_lexer.tests;
		"shell parser", Test_shell_parser.tests;
		"core parser", Test_core_parser.tests;
	]
