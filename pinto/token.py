from enum import Enum
from typing import Optional, Union


# Token Types
#
# EOF (end-of-file) token is used to indicate that
# there is no more input left for lexical analysis
class TokenType(Enum):
    INTEGER       = 'INTEGER'
    REAL          = 'REAL'
    INTEGER_CONST = 'INTEGER_CONST'
    REAL_CONST    = 'REAL_CONST'
    PLUS          = 'PLUS'
    MINUS         = 'MINUS'
    MUL           = 'MUL'
    INTEGER_DIV   = 'INTEGER_DIV'
    FLOAT_DIV     = 'FLOAT_DIV'
    LPAREN        = 'LPAREN'
    RPAREN        = 'RPAREN'
    ID            = 'ID'
    ASSIGN        = 'ASSIGN'
    BEGIN         = 'BEGIN'
    END           = 'END'
    SEMI          = 'SEMI'
    DOT           = 'DOT'
    PROGRAM       = 'PROGRAM'
    VAR           = 'VAR'
    COLON         = 'COLON'
    COMMA         = 'COMMA'
    EOF           = 'EOF'


class Token:
    def __init__(self,
                 type: TokenType,
                 value: Optional[Union[int, float, str]]) -> None:
        self.type = type
        # Token Value: non-negative integer value, '+', '-', '*', '/' or None
        self.value = value

    def __str__(self) -> str:
        """String representation of the Token instance.

        Examples:
        Token(INTEGER, 0)
        Token(PLUS, '+')
        """
        return f'Token({self.type, self.value!r})'

    def __repr__(self) -> str:
        return self.__str__()
