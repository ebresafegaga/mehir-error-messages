%start main
%token DIV
%token EOF
%token <int> INT
%token LPAREN
%token MINUS
%token PLUS
%token RPAREN
%token TIMES
%left MINUS PLUS
%left DIV TIMES
%nonassoc UMINUS
%type <unit> main
%on_error_reduce expr
%%

main:
  expr EOF
    {}

expr:
  INT
    {}
| LPAREN expr RPAREN
    {}
| expr PLUS expr
    {}
| expr MINUS expr
    {}
| expr TIMES expr
    {}
| expr DIV expr
    {}
| MINUS expr %prec UMINUS
    {}

%%
