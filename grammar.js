// This file was autogenerated by parse_grammar.lua from grammar.pretty.ebnf.
/*
 * inline constants: 
 */
module.exports = grammar({
    name: 'shell',
    word: $ => $.WORD,
    rules: {
        source_file: $ => choice(
            repeat('\n'),
            optional(seq($.complete_command, repeat(seq(repeat1('\n'), $.complete_command)), repeat('\n')))
        ),
        complete_command: $ => seq($.list, optional($.separator_op)),
        list: $ => prec.left(seq($.and_or, repeat(seq($.separator_op, $.and_or)))),
        and_or: $ => seq($.pipeline, repeat(seq(choice('&&', '||'), repeat('\n'), $.pipeline))),
        pipeline: $ => seq(optional('!'), $.pipe_sequence),
        pipe_sequence: $ => seq($.command, repeat(seq('|', repeat('\n'), $.command))),
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
        compound_list: $ => seq(repeat('\n'), $.term, optional($.separator)),
        term: $ => prec.left(seq($.and_or, repeat(seq($.separator, $.and_or)))),
        for_clause: $ => seq(
            'for',
            $.name,
            optional(seq(optional(seq(repeat('\n'), 'in', repeat($.WORD))), $.sequential_sep)),
            $.do_group
        ),
        case_list_ns: $ => seq(optional($.case_list), $.case_item_ns),
        case_list: $ => prec.left(repeat1($.case_item)),
        case_item_ns: $ => seq(optional('('), $.pattern, ')', choice(repeat('\n'), $.compound_list)),
        case_item: $ => seq(
            optional('('),
            $.pattern,
            ')',
            choice(repeat('\n'), $.compound_list),
            ';;',
            repeat('\n')
        ),
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
        function_definition: $ => seq($.fname, '(', ')', repeat('\n'), $.function_body),
        function_body: $ => seq($.compound_command, optional($.redirect_list)),
        brace_group: $ => seq('{', $.compound_list, '}'),
        do_group: $ => seq('do', $.compound_list, 'done'),
        simple_command: $ => choice(
            seq($.cmd_prefix, optional(seq($.cmd_word, optional($.cmd_prefix)))),
            seq($.cmd_name, optional($.cmd_prefix))
        ),
        redirect_list: $ => repeat1($.io_redirect),
        io_file: $ => seq(choice('<', '<&', '>', '>&', '>>', '<>', '>|'), $.filename),
        io_here: $ => seq(choice('<<', '<<-'), $.here_end),
        separator_op: $ => choice('&', ';'),
        separator: $ => choice(seq($.separator_op, repeat('\n')), repeat1('\n')),
        sequential_sep: $ => choice(seq(';', repeat('\n')), repeat1('\n')),
        io_redirect: $ => seq(field('fd', optional(/[1-9][0-9]*/)), choice($.io_file, $.io_here)),
        case_clause: $ => seq(
            'case',
            $.WORD,
            repeat('\n'),
            'in',
            repeat('\n'),
            optional(choice($.case_list, $.case_list_ns)),
            'esac'
        ),
        WORD: $ => /[a-zA-Z__][a-zA-Z_0-9_]*/,
        cmd_prefix: $ => prec.left(choice($.io_redirect, repeat1($.assignment))),
        cmd_suffix: $ => prec.left(choice($.io_redirect, repeat1($.WORD))),
        pattern: $ => seq($.WORD, repeat(seq('|', $.WORD))),
        fname: $ => $.WORD,
        name: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,
        assignment: $ => seq($.name, '=', $.WORD),
        cmd_name: $ => $.WORD,
        cmd_word: $ => $.assignment,
        filename: $ => $.WORD,
        here_end: $ => $.WORD,
    }
});
