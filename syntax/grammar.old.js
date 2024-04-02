/*
 * https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html
 */
module.exports = grammar({
	name: 'shell',
	rules: {
		source_file: $ => $.program,
		WORD: $ => /[\w_][\w\d_]*/,
		IO_NUMBER: $ => /[1-9][\d]*/,
		linebreak: $ => $.newline_list,
		program: $ => seq(
			$.linebreak,
			optional(
				seq($.complete_command, repeat(seq($.newline_list, $.complete_command)), $.linebreak)
			)
		),
		complete_command: $ => seq($.list, optional($.separator_op)),
		list: $ => prec.left(seq($.and_or, repeat(seq($.separator_op, $.and_or)))),
		and_or: $ => seq($.pipeline, repeat(seq(choice('&&', '||'), $.linebreak, $.pipeline))),
		pipeline: $ => seq(optional('!'), $.pipe_sequence),
		pipe_sequence: $ => seq($.command, repeat(seq('|', $.linebreak, $.command))),
		command: $ => choice(
			$.simple_command,
			seq($.compound_command, optional($.redirect_list)),
			$.function_definition
		),
		compound_command: $ => choice(
			$.brace_group,
			$.subshell,
			$.for_clause,
			$.case_clause,
			$.if_clause,
			$.while_clause,
			$.until_clause
		),
		subshell: $ => seq('(', $.compound_list, ')'),
		compound_list: $ => seq($.linebreak, $.term, optional($.separator)),
		term: $ => prec.right(seq($.and_or, repeat(seq($.separator, $.and_or)))),
		for_clause: $ => seq(
			'for',
			$._name,
			optional(seq(optional(seq($.linebreak, 'in', repeat($.WORD))), $.sequential_sep)),
			$.do_group
		),
		_name: $ => $.WORD,
		case_clause: $ => seq(
			'case',
			$.WORD,
			$.linebreak,
			'in',
			$.linebreak,
			optional(choice($.case_list, $.case_list_ns)),
			'esac'
		),
		case_list_ns: $ => seq(optional($.case_list), $.case_item_ns),
		case_list: $ => prec.right(repeat1($.case_item)),
		case_item_ns: $ => seq(optional('('), $.pattern, ')', choice($.linebreak, $.compound_list)),
		case_item: $ => seq(
			optional('('),
			$.pattern,
			')',
			choice($.linebreak, $.compound_list),
			';;',
			$.linebreak
		),
		pattern: $ => seq($.WORD, repeat(seq('|', $.WORD))),
		if_clause: $ => seq(
			'if',
			$.compound_list,
			'then',
			$.compound_list,
			optional(
				seq(
					repeat(seq('elif', $.compound_list, 'then', $.compound_list)),
					choice(seq('elif', $.compound_list, 'then'), 'else'),
					$.compound_list
				)
			),
			'fi'
		),
		while_clause: $ => seq('while', $.compound_list, $.do_group),
		until_clause: $ => seq('until', $.compound_list, $.do_group),
		function_definition: $ => seq($.fname, '(', ')', $.linebreak, $.function_body),
		function_body: $ => seq($.compound_command, optional($.redirect_list)),
		fname: $ => $.WORD,
		brace_group: $ => seq('{', $.compound_list, '}'),
		do_group: $ => seq('do', $.compound_list, 'done'),
		simple_command: $ => choice(
			seq($.cmd_prefix, optional(seq($.cmd_word, optional($.cmd_prefix)))),
			seq($.cmd_name, optional($.cmd_prefix))
		),
		cmd_name: $ => prec(0, $.WORD),
		cmd_word: $ => $.WORD,
		cmd_prefix: $ => prec.right(repeat1(prec(1, choice($.io_redirect, $.WORD)))),
		redirect_list: $ => repeat1($.io_redirect),
		io_redirect: $ => seq(optional($.IO_NUMBER), choice($.io_file, $.io_here)),
		io_file: $ => seq(
			choice('<', '<&', '>', '>&', '>>', '<>', '>|'),
			$.filename
		),
		filename: $ => $.WORD,
		io_here: $ => seq(choice('<<', '<<-'), $.here_end),
		here_end: $ => $.WORD,
		newline_list: $ => repeat1('\n'),
		separator_op: $ => choice('&', ';'),
		separator: $ => choice(seq($.separator_op, $.linebreak), $.newline_list),
		sequential_sep: $ => choice(seq(';', $.linebreak), $.newline_list),
	}
});
