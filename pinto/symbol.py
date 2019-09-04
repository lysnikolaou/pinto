from typing import Dict, Optional

from pinto.parser import (
    Block, Program, BinOp, Num, UnaryOp, Compound, NoOp, VarDecl, Assign, Var
)
from pinto.visitor import NodeVisitor


class Symbol:
    def __init__(self, name: str, type: Optional['Symbol'] = None) -> None:
        self.name = name
        self.type = type


class BuiltinTypeSymbol(Symbol):
    def __init__(self, name: str) -> None:
        super().__init__(name)

    def __str__(self) -> str:
        return self.name

    __repr__ = __str__


class VarSymbol(Symbol):
    def __init__(self, name: str, type: Optional[Symbol] = None) -> None:
        super().__init__(name, type)

    def __str__(self) -> str:
        return f'<{self.name}:{self.type}>'
    
    __repr__ = __str__


class SymbolTable:
    def __init__(self) -> None:
        self._symbols: Dict[str, Symbol] = {}
        self._init_builtins()

    def _init_builtins(self) -> None:
        self.define(BuiltinTypeSymbol('INTEGER'))
        self.define(BuiltinTypeSymbol('REAL'))

    def __str__(self) -> str:
        return f'Symbols: {[value for value in self._symbols.values()]}'

    __repr__ = __str__

    def define(self, symbol: Symbol) -> None:
        print(f'Define: {symbol}')
        self._symbols[symbol.name] = symbol

    def lookup(self, name: str) -> Optional[Symbol]:
        print(f'Lookup: {name}')
        return self._symbols.get(name)


class SymbolTableBuilder(NodeVisitor):
    def __init__(self) -> None:
        self.symtab = SymbolTable()

    def visit_Block(self, node: Block) -> None:
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_Program(self, node: Program) -> None:
        self.visit(node.block)

    def visit_BinOp(self, node: BinOp) -> None:
        self.visit(node.left)
        self.visit(node.right)

    def visit_Num(self, node: Num) -> None:
        pass

    def visit_UnaryOp(self, node: UnaryOp) -> None:
        self.visit(node.expr)

    def visit_Compound(self, node: Compound) -> None:
        for child in node.children:
            self.visit(child)

    def visit_NoOp(self, node: NoOp) -> None:
        pass

    def visit_VarDecl(self, node: VarDecl) -> None:
        type_name = node.type_node.value
        assert isinstance(type_name, str)
        type_symbol = self.symtab.lookup(type_name)
        var_name = node.var_node.value
        assert isinstance(var_name, str)
        var_symbol = VarSymbol(var_name, type_symbol)
        self.symtab.define(var_symbol)

    def visit_Assign(self, node: Assign):
        var_name = node.left.value
        assert isinstance(var_name, str)
        var_symbol = self.symtab.lookup(var_name)
        if var_symbol is None:
            raise NameError(repr(var_name))

        self.visit(node.right)

    def visit_Var(self, node: Var):
        var_name = node.value
        assert isinstance(var_name, str)
        var_symbol = self.symtab.lookup(var_name)

        if var_symbol is None:
            raise NameError(repr(var_name))
