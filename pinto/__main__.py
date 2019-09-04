import sys

from pinto.lexer import Lexer
from pinto.parser import Parser
from pinto.symbol import SymbolTableBuilder
from pinto.interpreter import Interpreter

def main():
    text = open(sys.argv[1], 'r').read()

    lexer = Lexer(text)
    parser = Parser(lexer)
    tree = parser.parse()
    stb = SymbolTableBuilder()
    stb.visit(tree)
    print(f'Symbol Table Contents:\n{stb.symtab}\n')
    interpreter = Interpreter(tree)
    result = interpreter.interpret()

    for k, v in sorted(interpreter.GLOBAL_SCOPE.items()):
        print('{} = {}'.format(k, v))


if __name__ == '__main__':
    main()
