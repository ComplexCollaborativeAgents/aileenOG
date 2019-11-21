import json
import xmlrpclib


def test_square():
    num = 7
    assert num*num == 49


# Functional test that creates a generalization and then matches against it
def test_concept_learner_server(port=8000):
    server = xmlrpclib.ServerProxy('http://dubs:'+str(port)+'/ConceptLearner')

    r = server.create_reasoning_symbol(xmlrpclib.Binary(json.dumps({'symbol':'RRed'})))
    res = json.loads(r.data)
    assert res['gpool'] == 'RRedMt'
    assert res['numSymbols'] == 1
    gpool = res['gpool']

    r = server.create_reasoning_predicate(xmlrpclib.Binary(json.dumps({'predicate':'rOn',
                                                                       'arity':2})))
    res = json.loads(r.data)
    assert res['gpool'] == 'rOnMt'
    assert res['numPredicates'] == 1

    data = {"facts":[["isa","O1","CVRed"],["isa","O1","RRed"],["isa","O1","CVCylinder"]],
            "context":"Test1",
            "gpool":gpool}
    r = server.add_case_to_gpool(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)
    assert res['numExamples'] == 1
    assert res['numGeneralizations'] == 0

    data = {"facts":[["isa","O2","CVRed"],["isa","O2","RRed"],["isa","O2","CVSphere"]],
            "context":"Test2",
            "gpool":gpool}
    r = server.add_case_to_gpool(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)
    assert res['numExamples'] == 0
    assert res['numGeneralizations'] == 1

    data = {"facts":[["isa","O3","CVRed"],["isa","O3","CVSphere"]],
            "context":"Test3",
            "gpool":gpool,
            "prevqueries":[],
            "pattern":["isa","?Obj","RRed"]}
    r = server.filter_scene_by_expression(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)
    assert len(res['matches']) == 1