let () =
	Alcotest.run "flynt" [
		"trie", Test_trie.tests;
		"vector", Test_vector.tests;
		"tokens", Test_token.tests;
		"reader", Test_reader.tests;
		"lexer", Test_lexer.tests;
		"parser", Test_parser.tests;
	]
