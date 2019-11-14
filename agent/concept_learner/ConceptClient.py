import json
import xmlrpclib



def test_concept_learner_server(port=8000):
    server = xmlrpclib.ServerProxy('http://dubs:'+str(port)+'/ConceptLearner')
    test_reasoning_symbols(server)
    test_generalization(server)

def test_reasoning_symbols(server):
    r = server.create_reasoning_symbol(xmlrpclib.Binary(json.dumps({'symbol':'RRed'})))
    res = json.loads(r.data)
    assert res['gpool'] == 'RRedMt'

    r = server.create_reasoning_predicate(xmlrpclib.Binary(json.dumps({'predicate':'rOn'})))
    res = json.loads(r.data)
    assert res['gpool'] == 'rOnMt'

def test_generalization(server):
    data = {"facts":[["isa","O1","CVRed"],["isa","O1","RRed"],["isa","O1","CVCylinder"]],
            "context":"Test1",
            "gpool":"RRedMt"}
    r = server.add_case_to_gpool(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)
    assert res['numExamples'] == 1
    assert res['numGeneralizations'] == 0

    data = {"facts":[["isa","O2","CVRed"],["isa","O2","RRed"],["isa","O2","CVSphere"]],
            "context":"Test2",
            "gpool":"RRedMt"}
    r = server.add_case_to_gpool(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)
    assert res['numExamples'] == 0
    assert res['numGeneralizations'] == 1

    data = {"facts":[["isa","O3","CVRed"],["isa","O3","CVSphere"]],
            "context":"Test3",
            "gpool":"RRedMt",
            "pattern":["isa","O3","RRed"]}
    r = server.match_case_against_gpool(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)
    assert len(res['matches']) == 1

test_concept_learner_server()