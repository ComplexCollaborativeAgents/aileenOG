from instructor.aileen_object import Color, Size, AileenObject


def test_ne1():
    o1 = AileenObject(shape='box', color=Color('red', [1, 0, 0]), size=Size('large', [.15,.15,.15]))
    o2 = AileenObject(shape='cube', color=Color('red', [1, 0, 0]), size=Size('large', [.15,.15,.15]))
    assert o1 != o2


def test_ne2():
    o1 = AileenObject(shape='box', color=Color('red', [1, 0, 0]), size=Size('large', [.15,.15,.15]))
    o2 = AileenObject(shape='box', color=Color('blue', [1, 0, 0]), size=Size('large', [.15,.15,.15]))
    assert o1 != o2


def test_ne3():
    """Regression test: In Python 2 the `__ne__` has to be defined."""
    o1 = AileenObject(shape='box', color=Color('red', [1, 0, 0]), size=Size('large', [.15,.15,.15]))
    o2 = AileenObject(shape='box', color=Color('red', [1, 0, 0]), size=Size('large', [.15,.15,.15]))
    assert not (o1 != o2)

def test_ne4():
    o1 = AileenObject(shape='box', color=Color('red', [1,0,0]), size=Size('small', [.05,.05,.05]))
    o2 = AileenObject(shape='box', color=Color('red', [1,0,0]), size=Size('medium', [.1,.1,.1]))
    assert o1 != o2

def test_ne5():
    o1 = AileenObject(shape='box', color=Color('red', [1,0,0]), size=Size('small', [.05,.05,.05]))
    o2 = AileenObject(shape='box', color=Color('red', [1,0,0]), size=Size('small', [.04,.04,.04]))
    assert not (o1 != o2)

def test_eq1():
    o1 = AileenObject(shape='box', color=Color('red', [1, 0, 0]), size=Size('large', [.15,.15,.15]))
    o2 = AileenObject(shape='box', color=Color('red', [1, 0, 0]), size=Size('large', [.15,.15,.15]))
    assert o1 == o2


def test_eq2():
    o1 = AileenObject(shape='box', color=Color('red', [1, 0, 0]), size=Size('large', [.15,.15,.15]))
    o2 = AileenObject(shape='box', color=Color('red', [0.8, 0, 0]), size=Size('large', [.15,.15,.15]))
    assert o1 == o2

def test_eq3():
    o1 = AileenObject(shape='box', color=Color('red', [1,0,0]), size=Size('small', [.05,.05,.05]))
    o2 = AileenObject(shape='box', color=Color('red', [1,0,0]), size=Size('small', [.09,.09,.09]))
    assert o1 == o2

def test_contains1():
    o1 = AileenObject(shape='box', color=Color('red', [1, 0, 0]), size=Size('large', [.15,.15,.15]))
    o2 = AileenObject(shape='box', color=Color('red', [1, 0, 0]), size=Size('large', [.15,.15,.15]))
    objs = [o1]
    assert o2 in objs


def test_contains2():
    o1 = AileenObject(shape='box', color=Color('red', [1, 0, 0]), size=Size('large', [.15,.15,.15]))
    o2 = AileenObject(shape='box', color=Color('blue', [1, 0, 0]), size=Size('large', [.15,.15,.15]))
    o3 = AileenObject(shape='cone', color=Color('red', [1, 0, 0]), size=Size('large', [.15,.15,.15]))
    objs = [o1, o2]
    assert o3 not in objs
