import json
import xmlrpclib
import subprocess
import time
import settings
import pytest
import os

def test_square():
    num = 7
    assert num*num == 49

# Functional test that creates a generalization and then matches against it
#@pytest.mark.skip(reason='SSH in CI is not correctly set up yet.')
def test_concept_learner_server():

    cmd = 'ssh {} {}/start_concept_learner.sh {}'.format(settings.CONCEPT_LEARNER_HOST,
                                                         settings.CONCEPT_LEARNER_PATH,
                                                         settings.CONCEPT_LEARNER_PORT)
    out = subprocess.Popen(cmd,stdout=subprocess.PIPE, stderr=subprocess.STDOUT,shell=True)
    time.sleep(15) # Need to wait for lisp to start
# Objects
    http_str = 'http://{}:{}/ConceptLearner'.format(settings.CONCEPT_LEARNER_HOST,
                                                    settings.CONCEPT_LEARNER_PORT)
    server=xmlrpclib.ServerProxy(http_str)

    r = server.create_reasoning_symbol(xmlrpclib.Binary(json.dumps({'symbol':'RRed'})))

    res = json.loads(r.data)
    assert res['gpool'] == 'RRedMt'
    assert res['numSymbols'] == 1
    gpool = res['gpool']

    data = {"facts":[["isa","O1","CVRed"],["isa","O1","RRed"],["isa","O1","CVCylinder"]],
            "context":"Test1",
            "concept":"RRed"}
    r = server.store(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)
    assert res['numExamples'] == 1
    assert res['numGeneralizations'] == 0

    data = {"facts":[["isa","O3","CVRed"],["isa","O3","CVCube"]],
            "pattern":["isa","O3","RRed"]}
    r = server.query(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)
    assert res['matches'] == None # Overspecific

    data = {"facts":[["isa","O2","CVRed"],["isa","O2","RRed"],["isa","O2","CVSphere"]],
            "context":"Test2",
            "concept":"RRed"}
    r = server.store(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)
    assert res['numExamples'] == 0
    assert res['numGeneralizations'] == 1

    data = {"facts":[["isa","O3","CVBlue"],["isa","O3","CVPyramid"]],
            "pattern":["isa","O3","RRed"]}
    r = server.query(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)
    assert res['matches'] is None

    data = {"facts":[["isa","O3","CVRed"],["isa","O3","CVCube"]],
            "pattern":["isa","O3","RRed"]}
    r = server.query(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)
    assert len(res['matches']) == 1
    assert res['matches'] == [['isa', 'O3', 'RRed']]

# #Relations
#     r = server.create_reasoning_predicate(xmlrpclib.Binary(json.dumps({'predicate':'rRight'})))
#     res = json.loads(r.data)
#     assert res['gpool'] == 'rRightMt'
#     assert res['numPredicates'] == 1 #
#
#     data = {"facts":[["isa","Obj8A","CVCylinder"], ["isa","Obj8A","CVRed"],
#                      ["isa","Obj8B","CVCube"], ["isa","Obj8B","CVBlue"],
#                                 ["n","Obj8A","Obj8B"], ["ec","Obj8A","Obj8B"],
#                                 ["rRight","Obj8A","Obj8B"]],
#             "context":"Test8",
#             "concept":"rRight"}
#     r = server.store(xmlrpclib.Binary(json.dumps(data)))
#     res = json.loads(r.data)
#     assert res['numExamples'] == 1
#     assert res['numGeneralizations'] == 0
#
#     data = {"facts":[["isa","Obj9A","CVCube"], ["isa","Obj9A","CVRed"],
#                                 ["isa","Obj9B","CVCube"], ["isa","Obj9B","CVGreen"],
#                                 ["n","Obj9A","Obj9B"], ["dc","Obj9A","Obj9B"],
#                                 ["rRight","Obj9A","Obj9B"]],
#             "context":"Test9",
#             "concept":"rRight"}
#     r = server.store(xmlrpclib.Binary(json.dumps(data)))
#     res = json.loads(r.data)
#     assert res['numExamples'] == 0
#     assert res['numGeneralizations'] == 1
#
#     data = {"facts": [["isa","Obj10A","CVPyramid"], ["isa","Obj10A","CVBlue"],
#                         ["isa","Obj10B","CVPyramid"], ["isa","Obj10B","CVGreen"],
#                         ["n","Obj10A","Obj10B"], ["dc","Obj10A","Obj10B"]],
#             "pattern": ["rRight","Obj10A","Obj10B"]}
#     r = server.query(xmlrpclib.Binary(json.dumps(data)))
#     res = json.loads(r.data)
#     assert res['matches'] is None # Learned concept is overly specific at this point
#
#     data = {"facts": [["isa","Obj10A","CVPyramid"], ["isa","Obj10A","CVRed"],
#                         ["isa","Obj10B","CVCube"], ["isa","Obj10B","CVGreen"],
#                         ["n","Obj10A","Obj10B"], ["dc","Obj10A","Obj10B"]],
#             "pattern": ["rRight","Obj10A","Obj10B"]}
#     r = server.query(xmlrpclib.Binary(json.dumps(data)))
#     res = json.loads(r.data)
# #    assert len(res['matches']) == 1
#     # This should match, but it doesn't currently. Klenk have a request in with QRG 12/18
#
#     # add more cases to the generalization
#     data = {"facts":[["isa","Obj11A","CVCylinder"], ["isa","Obj11A","CVGreen"],
#                           ["isa","Obj11B","CVCylinder"], ["isa","Obj11B","CVBlue"],
#                                 ["n","Obj11A","Obj11B"], ["ec","Obj11A","Obj11B"],
#                                 ["rRight","Obj11A","Obj11B"]],
#             "context":"Test11",
#             "concept":"rRight"}
#     r = server.store(xmlrpclib.Binary(json.dumps(data)))
#     res = json.loads(r.data)
#     assert res['numExamples'] == 0
#     assert res['numGeneralizations'] == 1
#
#     data = {"facts":[["isa","Obj12A","CVPyramid"], ["isa","Obj12A","CVGreen"],
#                           ["isa","Obj12B","CVCylinder"], ["isa","Obj12B","CVBlue"],
#                                 ["n","Obj12A","Obj11B"], ["dc","Obj12A","Obj12B"],
#                                 ["rRight","Obj12A","Obj12B"]],
#             "context":"Test12",
#             "concept":"rRight"}
#     r = server.store(xmlrpclib.Binary(json.dumps(data)))
#     res = json.loads(r.data)
#     assert res['numExamples'] == 0
#     assert res['numGeneralizations'] == 1
#
#
#     data = {"facts": [["isa","Obj10A","CVPyramid"], ["isa","Obj10A","CVGreen"],
#                                ["isa","Obj10B","CVCube"], ["isa","Obj10B","CVBlue"],
#                                 ["n","Obj10A","Obj10B"], ["dc","Obj10A","Obj10B"]
#                                 ],
#             "pattern": ["rRight","Obj10A","Obj10B"]}
#     r = server.query(xmlrpclib.Binary(json.dumps(data)))
#     res = json.loads(r.data)
#     assert len(res['matches']) == 1
#     assert res['matches']==[["rRight","Obj10A","Obj10B"]]
#
#     data = {"facts": [["isa", "Obj10A", "CVPyramid"], ["isa", "Obj10A", "CVBlue"],
#                      ["isa", "Obj10B", "CVCube"], ["isa", "Obj10B", "CVGreen"],
#                     ["s", "Obj10A", "Obj10B"], ["dc", "Obj10A", "Obj10B"]],
#             "pattern": ["rRight","Obj10A","Obj10B"]}
#     r = server.query(xmlrpclib.Binary(json.dumps(data)))
#     res = json.loads(r.data)
#     assert res['matches'] is None #negative case
#
#
# # relations by pattern
#     data = {"facts": [["isa", "Obj10A", "CVPyramid"], ["isa", "Obj10A", "CVBlue"],
#          ["isa", "Obj10B", "CVCube"], ["isa", "Obj10B", "CVGreen"],
#          ["s", "Obj10A", "Obj10B"], ["dc", "Obj10A", "Obj10B"],
#          ["isa", "Obj10C", "CVPyramid"], ["isa", "Obj10C", "CVBlue"],
#          ["s", "Obj10A", "Obj10C"], ["dc", "Obj10A", "Obj10C"],
#          ["s", "Obj10B", "Obj10C"], ["po", "Obj10B", "Obj10C"]],
#             "pattern": ["rRight","Obj10A","?Obj"]}
#     r = server.query(xmlrpclib.Binary(json.dumps(data)))
#     res = json.loads(r.data)
#     assert res['matches'] is None #negative case
#
#     data = {"facts": [["isa", "Obj10A", "CVPyramid"], ["isa", "Obj10A", "CVBlue"],
#          ["isa", "Obj10B", "CVCube"], ["isa", "Obj10B", "CVGreen"],
#          ["n", "Obj10A", "Obj10B"], ["dc", "Obj10A", "Obj10B"],
#          ["isa", "Obj10C", "CVPyramid"], ["isa", "Obj10C", "CVBlue"],
#          ["n", "Obj10A", "Obj10C"], ["dc", "Obj10A", "Obj10C"],
#          ["s", "Obj10B", "Obj10C"], ["po", "Obj10B", "Obj10C"]],
#             "pattern": ["rRight","Obj10A","?Obj"]}
#     r = server.query(xmlrpclib.Binary(json.dumps(data)))
#     res = json.loads(r.data)
#     assert len(res['matches']) == 2
#     assert ["rRight","Obj10A","Obj10B"] in res['matches']
#     assert ["rRight", "Obj10A", "Obj10C"] in res['matches']
#
#     data = {"facts": [["isa", "Obj10A", "CVPyramid"], ["isa", "Obj10A", "CVBlue"],
#          ["isa", "Obj10B", "CVCube"], ["isa", "Obj10B", "CVGreen"],
#          ["n", "Obj10A", "Obj10B"], ["dc", "Obj10A", "Obj10B"],
#          ["isa", "Obj10C", "CVPyramid"], ["isa", "Obj10C", "CVBlue"],
#          ["n", "Obj10A", "Obj10C"], ["dc", "Obj10A", "Obj10C"],
#          ["n", "Obj10B", "Obj10C"], ["po", "Obj10B", "Obj10C"]],
#             "pattern": ["rRight","?Obj1","?Obj2"]}
#     r = server.query(xmlrpclib.Binary(json.dumps(data)))
#     res = json.loads(r.data)
#     assert len(res['matches']) == 3
#     assert ["rRight","Obj10A","Obj10B"] in res['matches']
#     assert ["rRight", "Obj10A", "Obj10C"] in res['matches']
#     assert ["rRight", "Obj10B", "Obj10C"] in res['matches']
#
#
#     # Action Learning and Recognition
#     r = server.create_reasoning_action(xmlrpclib.Binary(json.dumps({'action':'rMove',
#                                                                     'arity':2})))
#     res = json.loads(r.data)
#     assert res['numActions'] == 1 #
#
#     with open(os.path.join(os.path.dirname(os.path.abspath(__file__)),'data/action12.json'),
#               'r') as myfile:
#         data = myfile.read()
#     r = server.store(xmlrpclib.Binary(data))
#     res = json.loads(r.data)
#     assert res['numExamples'] == 1
#     assert res['numGeneralizations'] == 0
#
#     with open(os.path.join(os.path.dirname(os.path.abspath(__file__)),'data/action13.json'),
#               'r') as myfile:
#         data = myfile.read()
#     r = server.store(xmlrpclib.Binary(data))
#     res = json.loads(r.data)
#     assert res['numExamples'] == 0
#     assert res['numGeneralizations'] == 1
#
#     with open(os.path.join(os.path.dirname(os.path.abspath(__file__)),'data/action14_1.json'),
#               'r') as myfile:
#         data = myfile.read()
#     r = server.query(xmlrpclib.Binary(data))
#     res = json.loads(r.data)
#     assert len(res['matches']) == 1
#     assert ["rMove", "Obj14B", ["rRight", "Obj14B", "Obj14A"]] in res['matches']
#
#     # no match for other object
#     with open(os.path.join(os.path.dirname(os.path.abspath(__file__)),'data/action14_2.json'),
#               'r') as myfile:
#         data = myfile.read()
#     r = server.query(xmlrpclib.Binary(data))
#     res = json.loads(r.data)
#     assert res['matches'] == None
#
    out.kill() # do we need to do anything more?
