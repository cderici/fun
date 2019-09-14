# binary search tree

# let's not worry about the remove yet (too many cases and I'm lazy)

class Node(object):
    def __init__(self, data):
        self.data = data
        self.right = None
        self.left = None

    def _add(self, data):
        if self.data == data:
            # let's not allow duplicates for now
            # we could push it to the left or right
            return
        if data > self.data:
            if not self.right:
                self.right = Node(data)
            else:
                self.right._add(data)
        else:
            if not self.left:
                self.left = Node(data)
            else:
                self.left._add(data)
        return

    def _find(self, data):
        if self.data == data:
            return True
        elif data > self.data and self.right:
            return self.right._find(data)
        elif data < self.data and self.left:
            return self.left._find(data)
        return False

    def inorder_print(self):
        if self:
            if self.left:
                self.left.inorder_print()
            print self.data
            if self.right:
                self.right.inorder_print()

class Tree(object):
    def __init__(self):
        self.root = None

    def add(self, data):
        if not self.root:
            self.root = Node(data)
            return
        self.root._add(data)
        return

    def find(self, data):
        if not self.root:
            return False # could error etc.
        return self.root._find(data)

    def print_in_order(self):
        if not self.root:
            return
        self.root.inorder_print()

t = Tree()
t.add(15)
t.add(9)
t.add(17)
t.add(5)
t.add(3)
t.add(6)
t.add(19)
t.add(18)

#print t.root.left.left.data

assert t.find(5)
t.print_in_order()
