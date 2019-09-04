from pinto.parser import AST

class NodeVisitor:
    def visit(self, node: AST) -> int:
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node: AST):
        raise Exception('No visit_{} method'.format(type(node).__name__))
