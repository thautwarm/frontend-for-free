import graphviz_artist as ga
import graphviz_artist.attr as a

import json
with open("test.json") as f:
    graph = json.load(f)

starts = {v: k for k, v in graph['starts'].items()}

g = ga.Graph(directed=True)
nodes = {int(k): v for k, v in graph['nodes'].items()}
gnodes = {}
for i, node in nodes.items():
    kind = node['kind']
    ctor = kind['ctor']
    if ctor == "NEntity":
        val = kind['val']
        ctor = val['ctor']
        if ctor == "ETerm":
            label = repr(val['case'])
        elif ctor == "ENonTerm":
            label = val['name']
        else:
            label = ctor
    elif ctor == "NReturn":
        label = "return"
    else:
        label = ctor
    if label == 'Start':
        label += " " + str(starts[i])
    label += " %d" % i
    gnodes[i] = g.new(a.Label(label))

for i, node in nodes.items():
    gnode = gnodes[i]
    for each in node['followed']:
        gnode > gnodes[each]

g.view()