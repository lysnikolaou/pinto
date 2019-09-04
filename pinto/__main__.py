import sys

from pinto.lexer import Lexer
from pinto.parser import Parser
from pinto.interpreter import Interpreter

def main():
    text = open(sys.argv[1], 'r').read()

    lexer = Lexer(text)
    parser = Parser(lexer)
    interpreter = Interpreter(parser)
    result = interpreter.interpret()

    for k, v in sorted(interpreter.GLOBAL_SCOPE.items()):
        print('{} = {}'.format(k, v))


if __name__ == '__main__':
    main()
