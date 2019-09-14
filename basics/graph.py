# graphs

"""
adj list
- better when sparse and we often need to access the neighbors
- slower for dense graphs (lot of edges -> lot of hops among lists)

adj matrix
- takes too much space for sparse graphs
- easier when weighted edges
- symmetric around the diagonal if undirected
"""


# adj list

# graph - adj list
class Vertex(object):
    def __init__(self, data):
        self.data = data
        self.neighbors = []

    def add_edge(self, v):
        if v not in self.neighbors:
            self.neighbors.append(v)

    def _print(self):
        print "%s : %s" % (self.data, [v.data for v in self.neighbors])

class Graph(object):
    vertices = {}

    def add_vertex(self, v):
        if isinstance(v, Vertex) and v.data not in self.vertices:
            self.vertices[v.data] = v
            return True
        return False

    def add_vertex_by_data(self, data):
        if data not in self.vertices:
            self.vertices[data] = Vertex(data)
            return True
        return False

    def add_edge_by_data(self, data1, data2):
        if data1 in self.vertices and data2 in self.vertices:
            self.vertices[data1].add_edge(self.vertices[data2])
            return True
        return False

    def add_edge(self, u, v):
        if u.data in self.vertices and v.data in self.vertices:
            self.vertices[u.data].add_edge(v)
            return True
        return False

    def print_graph(self):
        for k, v in self.vertices.iteritems():
            v._print()

g = Graph()
g.add_vertex_by_data('a')
g.add_vertex_by_data('b')
g.add_vertex_by_data('c')
g.add_vertex_by_data('d')
g.add_vertex_by_data('e')
g.add_edge_by_data('a','b')
g.add_edge_by_data('a','c')
g.add_edge_by_data('a','e')
g.add_edge_by_data('b','a')
g.add_edge_by_data('b','c')
g.add_edge_by_data('c','a')
g.add_edge_by_data('c','b')
g.add_edge_by_data('c','d')
g.add_edge_by_data('c','e')
g.add_edge_by_data('d','c')
g.add_edge_by_data('e','a')
g.add_edge_by_data('e','c')

g.print_graph()


# adj matrix (undirected)

class VertexM(object):
    def __init__(self, data):
        self.data = data

class GraphM(object):
    vertices = {}
    matrix = []
    vertex_indices = {}

    def add_vertex(self, data):
        if data not in self.vertices:
            self.vertices[data] = VertexM(data)
            for row in self.matrix:
                row.append(0)
            self.matrix.append([0] * len(self.vertices))
            self.vertex_indices[data] = len(self.vertex_indices)
            return True
        return False

    def add_edge(self, data1, data2, weight=1):
        if data1 in self.vertices and data2 in self.vertices:
            i = self.vertex_indices[data1]
            j = self.vertex_indices[data2]
            self.matrix[i][j] = weight
            self.matrix[j][i] = weight
            return True
        return False

    def print_graph(self):
        num_of_ver = len(self.matrix)
        ret = "\n"
        for k, i in self.vertex_indices.iteritems():
            ret += "%s " % k
            for j in range(num_of_ver):
                ret += " %s " % self.matrix[i][j]
            ret += " \n"
        print ret


g = GraphM()
g.add_vertex('a')
g.add_vertex('b')
g.add_vertex('c')
g.add_vertex('d')
g.add_vertex('e')
g.add_edge('a','b')
g.add_edge('a','c')
g.add_edge('a','e')
g.add_edge('b','a')
g.add_edge('b','c')
g.add_edge('c','a')
g.add_edge('c','b')
g.add_edge('c','d')
g.add_edge('c','e')
g.add_edge('d','c')
g.add_edge('e','a')
g.add_edge('e','c')

g.print_graph()
