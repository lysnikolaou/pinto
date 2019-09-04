from typing import Union, Dict, Optional

from pinto.token import TokenType
from pinto.parser import (
    AST, Program, Block, VarDecl, Type, NoOp, Compound, Parser, Assign, Var,
    BinOp, UnaryOp, Num
)
from pinto.visitor import NodeVisitor


class Interpreter(NodeVisitor):
    def __init__(self, tree: AST) -> None:
        self.tree = tree
        self.GLOBAL_SCOPE: Dict[str, int] = {}

    def visit_Program(self, node: Program) -> None:
        self.visit(node.block)

    def visit_Block(self, node: Block) -> None:
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node: VarDecl) -> None:
        # Do nothing
        pass

    def visit_Type(self, node: Type) -> None:
        # Do nothing
        pass

    def visit_NoOp(self, node: NoOp) -> None:
        pass

    def visit_Compound(self, node: Compound) -> None:
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node: Assign) -> None:
        var_name = node.left.value
        assert isinstance(var_name, str)
        self.GLOBAL_SCOPE[var_name] = self.visit(node.right)

    def visit_Var(self, node: Var) -> Union[int, float]:
        var_name = node.value
        assert isinstance(var_name, str)
        val = self.GLOBAL_SCOPE.get(var_name)
        if val is None:
            raise NameError(repr(var_name))
        else:
            return val

    def visit_BinOp(self, node: BinOp) -> Union[int, float]:
        if node.op.type == TokenType.PLUS:
            return self.visit(node.left) + self.visit(node.right)
        if node.op.type == TokenType.MINUS:
            return self.visit(node.left) - self.visit(node.right)
        if node.op.type == TokenType.MUL:
            return self.visit(node.left) * self.visit(node.right)
        if node.op.type == TokenType.FLOAT_DIV:
            return self.visit(node.left) / self.visit(node.right)
        if node.op.type == TokenType.INTEGER_DIV:
            return self.visit(node.left) // self.visit(node.right)
        assert False

    def visit_UnaryOp(self, node: UnaryOp) -> Union[int, float]:
        op = node.op.type
        if op == TokenType.PLUS:
            return +self.visit(node.expr)
        if op == TokenType.MINUS:
            return -self.visit(node.expr)
        assert False

    def visit_Num(self, node: Num) -> Union[int, float]:
        assert isinstance(node.value, (int, float))
        return node.value

    def interpret(self) -> Optional[Union[int, float]]:
        tree = self.tree
        if tree is None:
            return -1
        return self.visit(tree)
