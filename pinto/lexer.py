from typing import Optional

from pinto.token import Token, TokenType


RESERVED_KEYWORDS = {
    'PROGRAM': Token(TokenType.PROGRAM, 'PROGRAM'),
    'VAR': Token(TokenType.VAR, 'VAR'),
    'BEGIN': Token(TokenType.BEGIN, 'BEGIN'),
    'END': Token(TokenType.END, 'END'),
    'INTEGER': Token(TokenType.INTEGER, 'INTEGER'),
    'REAL': Token(TokenType.REAL, 'REAL'),
    'DIV': Token(TokenType.INTEGER_DIV, 'DIV')
}


class Lexer:
    def __init__(self, text: str) -> None:
        # client string input, e.g. "3 * 5", "12 / 3 * 4", etc
        self.text = text
        # self.pos is an index into self.text
        self.pos = 0
        self.current_char: Optional[str] = self.text[self.pos]

    def error(self) -> None:
        raise Exception('Invalid character')

    def peek(self) -> Optional[str]:
        peek_pos = self.pos + 1
        if peek_pos > len(self.text) - 1:
            return None
        else:
            return self.text[peek_pos]

    def advance(self) -> None:
        """Advance the 'pos' pointer and set the 'current_char' variable."""
        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None  # Indicates end of input
        else:
            self.current_char = self.text[self.pos]

    def skip_whitespace(self) -> None:
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def skip_comment(self):
        while self.current_char != '}':
            self.advance()
        self.advance()  # the closing curly brace

    def number(self):
        """Return a (multidigit) integer or float consumed from the input."""
        result = ''
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()

        if self.current_char == '.':
            result += self.current_char
            self.advance()

            while (
                self.current_char is not None and
                self.current_char.isdigit()
            ):
                result += self.current_char
                self.advance()

            token = Token(TokenType.REAL_CONST, float(result))
        else:
            token = Token(TokenType.INTEGER_CONST, int(result))

        return token

    def _id(self) -> Token:
        result = ''
        while (self.current_char is not None and 
                (self.current_char.isalnum() or self.current_char == '_')):
            result += self.current_char
            self.advance()

        token = RESERVED_KEYWORDS.get(result, Token(TokenType.ID, result))
        return token

    def get_next_token(self) -> Token:
        """Lexical analyzer (also known as scanner or tokenizer)

        This method is responsible for breaking a sentence
        apart into tokens.
        """
        while self.current_char is not None:

            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char == '{':
                self.advance()
                self.skip_comment()
                continue

            if self.current_char.isalpha() or self.current_char == '_':
                return self._id()

            if self.current_char.isdigit():
                return self.number()

            if self.current_char == ':' and self.peek() == '=':
                self.advance()
                self.advance()
                return Token(TokenType.ASSIGN, ':=')

            if self.current_char == ':':
                self.advance()
                return Token(TokenType.COLON, ':')

            if self.current_char == ',':
                self.advance()
                return Token(TokenType.COMMA, ',')

            if self.current_char == ';':
                self.advance()
                return Token(TokenType.SEMI, ';')

            if self.current_char == '+':
                self.advance()
                return Token(TokenType.PLUS, '+')

            if self.current_char == '-':
                self.advance()
                return Token(TokenType.MINUS, '-')

            if self.current_char == '*':
                self.advance()
                return Token(TokenType.MUL, '*')

            if self.current_char == '/':
                self.advance()
                return Token(TokenType.FLOAT_DIV, '/')

            if self.current_char == '(':
                self.advance()
                return Token(TokenType.LPAREN, '(')

            if self.current_char == ')':
                self.advance()
                return Token(TokenType.RPAREN, ')')

            if self.current_char == '.':
                self.advance()
                return Token(TokenType.DOT, '.')

            self.error()

        return Token(TokenType.EOF, None)
