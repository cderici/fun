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

class Queue(object): # will be used in bfs
    def __init__(self):
        self.container = []

    def is_empty(self):
        return self.container == []

    def enqueue(self, data):
        self.container.append(data)

    def dequeue(self):
        return self.container.pop(0)

# graph - adj list
class Vertex(object):
    def __init__(self, data):
        self.data = data
        self.neighbors = []
        self.visited = False

    def add_edge(self, v):
        if v not in self.neighbors:
            self.neighbors.append(v)

    def _print(self):
        print "%s : %s" % (self.data, [v.data for v in self.neighbors])

    def _dfs(self, target_data):
        if self.visited:
            return False
        self.visited = True
        if self.data == target_data:
            return self
        for n in self.neighbors:
            t = n._dfs(target_data)
            if t:
                return t
        return False

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

    def depth_first_search(self, source_data, target_data):
        if source_data not in self.vertices:
            return False

        # reset the visited flags
        for k,v in self.vertices.iteritems():
            v.visited = False
        # go
        return self.vertices[source_data]._dfs(target_data)

    def breadth_first_search(self, source_data, target_data):
        if source_data not in self.vertices:
            return False

        # reset the visited flags (to avoid cycles)
        for k, v in self.vertices.iteritems():
            v.visited = False

        # go
        source_vertex = self.vertices[source_data]
        # crete empty queue
        q = Queue()
        source_vertex.visited = True
        # put the start in the queue
        q.enqueue(source_vertex)

        while not q.is_empty():
            # dequeue a node to inspect
            node = q.dequeue()
            node.visited = True
            if node.data == target_data:
                return node
            for nd in node.neighbors:
                # enqueue all the non-visited neighbors
                if not nd.visited:
                    q.enqueue(nd)
        return False

    # bidirectional bfs
    def meeting_point(self, source1_data, source2_data):
        if source1_data not in self.vertices or\
           source2_data not in self.vertices:
            return False

        # reset the visited flags (to avoid cycles)
        for k, v in self.vertices.iteritems():
            v.visited = False


        source1_vertex = self.vertices[source1_data]
        source2_vertex = self.vertices[source2_data]
        # create visited sets and put these in them
        visited_by_1 = set([source1_vertex])
        visited_by_2 = set([source2_vertex])
        # create an empty queue
        qu = Queue()
        # put the first two vertices in it
        qu.enqueue(source1_vertex)
        qu.enqueue(source2_vertex)

        while not qu.is_empty():
            node = qu.dequeue()
            if node in visited_by_1 and node in visited_by_2:
                return node
            for nd in node.neighbors:
                if not nd.visited:
                    if node in visited_by_1:
                        visited_by_1.add(nd)
                    if node in visited_by_2:
                        visited_by_2.add(nd)
                    qu.enqueue(nd)
        return False

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
assert g.depth_first_search('a', 'd')
assert g.breadth_first_search('a', 'd')
assert g.meeting_point('a', 'd').data == 'c'

# adj matrix (undirected)

class VertexM(object):
    def __init__(self, data):
        self.data = data
        self.visited = False

class GraphM(object):
    vertices = {}
    matrix = []
    vertex_indices = {}
    # this is not ideal, not easy to find a vertex by an index in the matrix

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
