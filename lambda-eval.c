#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Sstr {
	int length;
	char *cstr;
};

struct Sstr *sstr_make(char *str, int length) {
	assert(str != NULL);
	struct Sstr *ret = malloc(sizeof(struct Sstr));
	assert(ret != NULL);

	ret->length = length;
	ret->cstr = calloc((length + 1), sizeof(char));
	assert(ret->cstr != NULL);

	memcpy(ret->cstr, str, length);
	ret->cstr[length] = '\0';

	return ret;
}

struct Sstr *sstr_of_cstr(char *cstr) {
	assert(cstr != NULL);
	struct Sstr *str = malloc(sizeof(struct Sstr));
	assert(str != NULL);
	str->length = strlen(cstr);
	str->cstr = strdup(cstr);
	assert(str->cstr != NULL);

	return str;
}

void sstr_free(struct Sstr *str) {
	free(str->cstr);
	free(str);
}

void sstr_print(struct Sstr *str) {
	fputs(str->cstr, stdout);
}

typedef struct Sstr *sstr;

int sstr_eq(sstr a, sstr b) {
	if(a->length != b->length) {
		return 0;
	}
	return strcmp(a->cstr, b->cstr) == 0;
}

sstr sstr_dup(sstr str) {
	return sstr_of_cstr(str->cstr);
}

char sstr_sub(sstr str, int index) {
	if(index >= str->length) {
		return 0;
	}
	return str->cstr[index];
}

sstr sstr_slice(sstr str, int start, int length) {
	assert(start + length <= str->length);
	return sstr_make(&str->cstr[start], length);
}

int sstr_length(sstr str) {
	return str->length;
}

struct STerm;
typedef struct STerm *lambda_term;
struct SAbstraction {
	sstr variable;
	lambda_term term;
};
typedef struct SAbstraction *lambda_abstraction;

struct SApplication {
	lambda_term function;
	lambda_term input;
};
typedef struct SApplication *lambda_application;

struct STerm {
	enum { VARIABLE, ABSTRACTION, APPLICATION } type;
	union {
		lambda_term function;
		sstr variable;
	};
	union {
		lambda_term input;
		lambda_term term;
	};
};

void lambda_term_print(lambda_term term);

lambda_term term_dup(lambda_term t);

lambda_term make_term_abstraction(sstr variable, lambda_term term) {
	lambda_term r = malloc(sizeof(struct STerm));
	assert(r != NULL);
	r->type = ABSTRACTION;
	r->variable = sstr_dup(variable);
	r->term = term_dup(term);
	return r;
}

lambda_term make_term_application(lambda_term f, lambda_term i) {
	lambda_term r = malloc(sizeof(struct STerm));
	assert(r != NULL);
	r->type = APPLICATION;
	r->function = term_dup(f);
	r->input = term_dup(i);
	return r;
}

lambda_term make_term_variable(sstr variable) {
	lambda_term r = malloc(sizeof(struct STerm));
	assert(r != NULL);
	r->type = VARIABLE;
	r->variable = sstr_dup(variable);
	return r;
}

lambda_term term_dup(lambda_term t) {
	switch(t->type) {
	case VARIABLE : {
		return make_term_variable(t->variable);
	} break;
	case APPLICATION : {
		return make_term_application(t->function, t->input);
	} break;
	case ABSTRACTION : {
		return make_term_abstraction(t->variable, t->term);
	} break;
	default : abort();
	}
}

lambda_term beta_reduce(lambda_term term, sstr subvar, lambda_term what) {
	if(term->type == VARIABLE) {
		if(sstr_eq(term->variable, subvar)) {
			return term_dup(what);
		} else {
			return term_dup(term);
		}
	}
	if(term->type == APPLICATION) {
		return make_term_application(
			beta_reduce(term->function, subvar, what),
			beta_reduce(term->input, subvar, what));
	}
	if(term->type == ABSTRACTION) {
		if(sstr_eq(term->variable, subvar)) {
			return term_dup(term);
		} else {
			return make_term_abstraction(
				term->variable,
				beta_reduce(term->term, subvar, what));
		}
	}

	abort();
}

lambda_term eval(lambda_term term) {
	if(term->type == APPLICATION) {
		lambda_term input;
		if(term->function->type == APPLICATION) {
			input = eval(term->function);
		} else {
			input = term;
		}
		return beta_reduce(term->function->term,
				   term->function->variable,
				   term->input);
	}
	return term;
}

void lambda_term_print(lambda_term term) {
	if(term->type == VARIABLE) {
		sstr_print(term->variable);
		return;
	}
	if(term->type == APPLICATION) {
		printf("(");
		lambda_term_print(term->function);
		printf(")");
		lambda_term_print(term->input);
		return;
	}
	if(term->type == ABSTRACTION) {
		printf("\\");
		sstr_print(term->variable);
		printf(".");
		lambda_term_print(term->term);
		return;
	}
	assert(0 && "tried printing an unknown type");
}

typedef enum {
	LAMBDA,
	DOT,
	LPAREN,
	ID,
	RPAREN
} token_type;
typedef struct {
	token_type type;
	int start;
	int length;
} token;

void token_print(token *t) {
	printf("%d to %d, ", t->start, t->length);
	switch(t->type) {
	case LAMBDA : printf("LAMBDA"); break;
	case DOT : printf("DOT"); break;
	case RPAREN : printf("RPAREN"); break;
	case LPAREN : printf("LPAREN"); break;
	case ID : printf("ID"); break;
	}
	printf("\n");
}

typedef enum {
	lexer_error,
	lexer_continue,
	lexer_done,
} lexer_status;

typedef struct {
	sstr input;
	int line;
	int lp;
	token *tokens;
	lexer_status status;
} lexer;

void lexer_init(lexer *lexer, sstr input) {
	lexer->input = input;
	lexer->lp = 0;
	lexer->line = 0;
	lexer->status = lexer_continue;
	//lexer->tokens = make_dary(0, sizeof(token));
}

int try_lex_lambda(const lexer *lexer) {
	char c = sstr_sub(lexer->input, lexer->lp);
	if(c == '\\') {
		return 1;
	}
	return 0;
}

int try_lex_lparen(const lexer *lexer) {
	char c = sstr_sub(lexer->input, lexer->lp);
	if(c == '(') {
		return 1;
	}
	return 0;
}

int try_lex_rparen(const lexer *lexer) {
	char c = sstr_sub(lexer->input, lexer->lp);
	if(c == ')') {
		return 1;
	}
	return 0;
}

int try_lex_dot(const lexer *lexer) {
	char c = sstr_sub(lexer->input, lexer->lp);
	if(c == '.') {
		return 1;
	}
	return 0;
}

int try_lex_id(const lexer *lexer) {
	int i;
	for(i = lexer->lp; i < sstr_length(lexer->input); i++) {
		char c = sstr_sub(lexer->input, i);
		if(!('a' <= c && c <= 'z')) {
			return i - lexer->lp;
		}
	}
	return i - lexer->lp;
}

int is_whitespace(char c) {
	return (c == '\t' ||
		c == ' ' ||
		c == '\n');
}

lexer_status lexer_get_token(lexer *lexer, token *token) {
	int start;
	int length;
	for(;;) {
		char c = sstr_sub(lexer->input, lexer->lp);
		if(c == 0) {
			lexer->status = lexer_done;
			return lexer_done;
		}

		start = lexer->lp;
		length = 0;

		if(is_whitespace(c)) { lexer->lp++; continue; }

		length = try_lex_dot(lexer);
		if(length) { token->type = DOT; goto succeded; }
		length = try_lex_lparen(lexer);
		if(length) { token->type = LPAREN; goto succeded; }
		length = try_lex_rparen(lexer);
		if(length) { token->type = RPAREN; goto succeded; }
		length = try_lex_id(lexer);
		if(length) { token->type = ID; goto succeded; }
		length = try_lex_lambda(lexer);
		if(length) { token->type = LAMBDA; goto succeded; }

		printf("ignoring '%c' character\n", c);
		lexer->lp++;
	}
succeded:
	token->start = start; 
	token->length = length;
	lexer->lp += token->length - 1;
end:
	lexer->lp++;
	lexer->status = lexer_continue;
	return lexer_continue;
}

lexer_status lexer_unget_token(lexer *lexer, token *token) {
	lexer->lp -= token->length;
	return lexer_continue;
}

sstr lexer_get_token_sstr(lexer *lexer, token *t) {
	return sstr_slice(lexer->input, t->start, t->length);
}

lambda_term parse_term(lexer *lexer);
lambda_term parse_variable(lexer *lexer);
lambda_term parse(lexer *lexer);

lambda_term parse_abstraction(lexer *lexer) {
	token t;
	lexer_status status;

	status = lexer_get_token(lexer, &t);
	assert(status == lexer_continue && t.type == ID);
	sstr var = lexer_get_token_sstr(lexer, &t);

	status = lexer_get_token(lexer, &t);
	assert(status == lexer_continue && t.type == DOT);

	lambda_term abs = make_term_abstraction(var, parse(lexer));
	return abs;
}

lambda_term parse_term(lexer *lexer) {
	token t;

	lexer_status status = lexer_get_token(lexer, &t);
	if(status == lexer_done) {
		return NULL;
	}

	if(t.type == ID) {
		lambda_term var = make_term_variable(lexer_get_token_sstr(lexer, &t));
		return var;
	} else if(t.type == RPAREN) {
		lexer_unget_token(lexer, &t);
		return NULL;
	} else if(t.type == LAMBDA) {
		lambda_term abs = parse_abstraction(lexer);
		return abs;
	} else if(t.type == LPAREN) {
		lambda_term exp = parse(lexer);
		lexer_get_token(lexer, &t);
		assert(t.type == RPAREN);
		return exp;
	}

	assert(t.type != DOT);
}

lambda_term parse(lexer *lexer) {
	lambda_term terms[256];
	int lpc = 0;

	while(lexer->status == lexer_continue) {
		terms[lpc] = parse_term(lexer);
		if(terms[lpc] == NULL) {
			break;
		}
		lpc++;
	}

	assert(lpc != 0);

	if(lpc == 1) {
		return terms[0];
	}

	lambda_term app;
	app = make_term_application(terms[0], terms[1]);
	for(int i = 2; i < lpc; i++) {
		app = make_term_application(term_dup(app), terms[i]);
	}

	return app;
}

int main() {
	char input[256];
	int running = 1;
	for(;;) {
		fgets(input, 256, stdin);
		if(strncmp(input, "quit\n", 256) == 0) {
			printf("Bye!\n");
			break;
		}

		lexer l;
		lexer_init(&l, sstr_of_cstr(input));
		lambda_term lt = parse(&l);
		lambda_term_print(eval(lt));
		printf("\n");
	}

	return 0;
}
