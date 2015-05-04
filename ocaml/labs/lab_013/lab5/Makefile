#
# Makefile for ocaml lab 5.
#

# Default compilation target.
default: lexer_test parser_test ast_test

#
# Variables.
#

OCAMLLEX  = ocamllex
OCAMLYACC = ocamlyacc
OCAMLC    = ocamlc

INTERFACES = \
    sexpr.cmi           \
    parser.cmi          \
    parser_utils.cmi    \
    lexer_utils.cmi     \
    ast.cmi

OBJS = \
    sexpr.cmo           \
    parser.cmo          \
    lexer.cmo           \
    parser_utils.cmo    \
    lexer_utils.cmo     \
    ast.cmo


#
# Patterns.
#

%.ml: %.mll
	$(OCAMLLEX) $<

%.ml %.mli: %.mly
	$(OCAMLYACC) $<

%.cmo: %.ml %.cmi
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<


#
# Compilation targets.
#

lexer_test: $(INTERFACES) $(OBJS) lexer_test.cmo
	ocamlc -o $@ $(OBJS) lexer_test.cmo

parser_test: $(INTERFACES) $(OBJS) parser_test.cmo
	ocamlc -o $@ $(OBJS) parser_test.cmo

ast_test: $(INTERFACES) $(OBJS) ast_test.cmo
	ocamlc -o $@ $(OBJS) ast_test.cmo

lexer.cmo: lexer.ml
	$(OCAMLC) -c $<

lexer_test.cmo: lexer_test.ml
	$(OCAMLC) -c $<

parser_test.cmo: parser_test.ml
	$(OCAMLC) -c $<

ast_test.cmo: ast_test.ml
	$(OCAMLC) -c $<


#
# Tests.
#

test_lexer: lexer_test
	lexer_test tokens.bs

test_parser: parser_test
	parser_test factorial.bs

test_ast: ast_test
	ast_test factorial.bs


#
# Cleanup.
#

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml lexer_test parser_test ast_test

