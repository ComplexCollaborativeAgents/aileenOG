import json
import xmlrpclib
from settings import CONCEPT_LEARNER_HOST


def test_concept_learner_server(port=8085):
    server = xmlrpclib.ServerProxy("http://" + CONCEPT_LEARNER_HOST + ":" + str(port) + "/ConceptLearner")
    test_reasoning_symbols(server)
    test_generalization(server)
    test_relational_generalization(server)
    test_describe(server)

def test_reasoning_symbols(server):
    r = server.create_reasoning_symbol(xmlrpclib.Binary(json.dumps({'symbol':'r_red1'})))
    res = json.loads(r.data)
    # assert res['gpool'] == 'RRedMt'

    # r = server.create_reasoning_predicate(xmlrpclib.Binary(json.dumps({'predicate':'rOn'})))
    # res = json.loads(r.data)
    # assert res['gpool'] == 'rOnMt'

    r = server.create_reasoning_predicate(xmlrpclib.Binary(json.dumps({'predicate':'r_rightOf1'})))
    r = server.create_reasoning_predicate(xmlrpclib.Binary(json.dumps({'predicate':'r_near1'})))

def test_generalization(server):

    # RED EXAMPLES
    data = {"facts":[["isa","O1","CVRed"],["isa","O1","r_red1"],["isa","O1","CVCylinder"],["size", "O1", ".01"]],
            "context":"Test1",
            "gpool":"r_red1"}
    r = server.add_case_to_gpool(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)
    print(res)
    # assert res['numExamples'] == 1
    # assert res['numGeneralizations'] == 0

    data = {"facts":[["isa","O2","CVRed"],["isa","O2","r_red1"],["isa","O2","CVSphere"],["size", "O2", ".01"]],
            "context":"Test2",
            "gpool":"r_red1"}
    r = server.add_case_to_gpool(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)
    # assert res['numExamples'] == 0
    # assert res['numGeneralizations'] == 1

    data = {"facts":[["isa","O3","CVRed"],["isa","O3","r_red1"],["isa","O3","CVSphere"],["size", "O3", ".01"]],
            "context":"Test3",
            "gpool":"r_red1"}
    r = server.add_case_to_gpool(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)
    # assert len(res['matches']) == 1


def test_relational_generalization(server):


    # RIGHT OF EXAMPLES
    data = {"facts":[["isa","O1","CVRed"],["isa","O1","CVCylinder"], ["size", "O1", ".01"],
                     ["isa","O2","CVGreen"],["isa","O2","CVCone"], ["size", "O2", ".01"],
                     ["distance", ".11", "O1", "O2"],
                     ["rightOf", "O1", "O2"], ["r_rightOf1", "O1", "O2"]],
            "context":"Test4",
            "gpool":"r_rightOf1"}
    r = server.add_case_to_gpool(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)

    data = {"facts":[["isa","O3","CVRed"],["isa","O3","CVCone"], ["size", "O3", ".01"],
                     ["isa","O4","CVGreen"],["isa","O4","CVCylinder"], ["size", "O4", ".01"],
                     ["distance", ".11", "O3", "O4"],
                     ["rightOf", "O3", "O4"], ["r_rightOf1", "O3", "O4"]],
            "context":"Test5",
            "gpool":"r_rightOf1"}
    r = server.add_case_to_gpool(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)

    data = {"facts":[["isa","O5","CVBlue"],["isa","O5","CVCylinder"], ["size", "O5", ".01"],
                     ["isa","O6","CVGreen"],["isa","O6","CVSphere"], ["size", "O6", ".01"],
                     ["distance", ".11", "O5", "O6"],
                     ["rightOf", "O5", "O6"], ["r_rightOf1", "O5", "O6"]],
            "context":"Test6",
            "gpool":"r_rightOf1"}
    r = server.add_case_to_gpool(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)


    # NEAR EXAMPLES
    data = {"facts":[["isa","O1","CVRed"], ["isa","O1","CVCylinder"], ["size", "O1", ".01"],
                     ["isa","O2","CVGreen"], ["isa","O2","CVCone"], ["size", "O2", ".01"],
                     ["distance", ".11", "O1", "O2"], ["r_near1", "O1", "O2"]],
            "context":"Test7",
            "gpool":"r_near1"}
    r = server.add_case_to_gpool(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)

    data = {"facts":[["isa","O3","CVRed"],["isa","O3","CVCone"],["size", "O3", ".01"],
                     ["isa","O4","CVGreen"],["isa","O4","CVCylinder"], ["size", "O4", ".01"],
                     ["distance", ".11", "O3", "O4"], ["r_near1", "O3", "O4"]],
            "context":"Test8",
            "gpool":"r_near1"}
    r = server.add_case_to_gpool(xmlrpclib.Binary(json.dumps(data)))

    data = {"facts":[["isa","O5","CVRed"],["isa","O5","CVCylinder"], ["size", "O5", ".01"],
                     ["isa","O6","CVGreen"],["isa","O6","CVSphere"], ["size", "O6", ".01"],
                     ["distance", ".11", "O5", "O6"], ["r_near1", "O5", "O6"]],
            "context":"Test9",
            "gpool":"r_near1"}
    r = server.add_case_to_gpool(xmlrpclib.Binary(json.dumps(data)))
   


def test_describe(server):

    data = {"facts":[["isa","O1","CVRed"], ["isa","O1","CVCylinder"], ["size", "O1", ".01"]]}
    r = server.describe(xmlrpclib.Binary(json.dumps(data)))
    res = json.loads(r.data)
    print("red test: ", res)

    # data = {"facts":[["isa","O11","CVRed"], ["isa","O11","CVCylinder"], ["size", "O11", ".01"],
    #                  ["isa","O22","CVGreen"], ["isa","O22","CVSphere"], ["size", "O22", ".01"],
    #                  ["distance", ".11", "O11", "O22"], ["rightOf", "O11", "O22"]]}
    # r = server.describe(xmlrpclib.Binary(json.dumps(data)))
    # res = json.loads(r.data)
    # print("rightOf test: ", res)
    #
    # data = {"facts":[["isa","O55","CVRed"], ["isa","O55","CVCylinder"], ["size", "O55", ".01"],
    #                  ["isa","O66","CVGreen"], ["isa","O66","CVCone"], ["size", "O66", ".01"],
    #                  ["distance", ".11", "O55", "O6"]]}
    # r = server.describe(xmlrpclib.Binary(json.dumps(data)))
    # res = json.loads(r.data)
    # print("near test: ", res)


