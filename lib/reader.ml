type t = {
	reader : Eio.Buf_read.t [@show.opaque];
	mutable pos : int;
	mutable line : int;
	mutable col : int;
} [@@deriving show]

let init (flow : _ Eio.Flow.source) (max_size : int) : t = {
	reader = (Eio.Buf_read.of_flow flow ~max_size:max_size);
	pos = 0;
	line = 0;
	col = 0;
}

let pos (s : t) : int = s.pos

let line (s : t) : int = s.line

let col (s : t) : int = s.col

let peek (s : t) : char option = Eio.Buf_read.peek_char s.reader

let get (s : t) : char =
	match Eio.Buf_read.any_char s.reader with
	| '\n' -> (
		s.pos <- s.pos + 1;
		s.line <- s.line + 1;
		s.col <- 0;
		'\n'
	)
	| _ as c -> (
		s.pos <- s.pos + 1;
		s.col <- s.col + 1;
		c
	)

let skip (s : t) : unit =
	match Eio.Buf_read.any_char s.reader with
	| '\n' -> (
		s.pos <- s.pos + 1;
		s.line <- s.line + 1;
		s.col <- 0;
	)
	| _ -> (
		s.pos <- s.pos + 1;
		s.col <- s.col + 1;
	)

let rec skip_whitespace (s : t) : unit =
	match peek s with
	| Some c when Char.Ascii.is_white c -> (
		skip s;
		skip_whitespace s
	)
	| _ -> ()
