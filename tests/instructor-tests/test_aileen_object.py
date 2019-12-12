from instructor.aileen_object import Color, AileenObject


def test_ne1():
    o1 = AileenObject(shape='box', color=Color('red', [1, 0, 0]))
    o2 = AileenObject(shape='cube', color=Color('red', [1, 0, 0]))
    assert o1 != o2


def test_ne2():
    o1 = AileenObject(shape='box', color=Color('red', [1, 0, 0]))
    o2 = AileenObject(shape='box', color=Color('blue', [1, 0, 0]))
    assert o1 != o2


def test_ne3():
    """Regression test: In Python 2 the `__ne__` has to be defined."""
    o1 = AileenObject(shape='box', color=Color('red', [1, 0, 0]))
    o2 = AileenObject(shape='box', color=Color('red', [1, 0, 0]))
    assert not(o1 != o2)


def test_eq1():
    o1 = AileenObject(shape='box', color=Color('red', [1, 0, 0]))
    o2 = AileenObject(shape='box', color=Color('red', [1, 0, 0]))
    assert o1 == o2


def test_eq2():
    o1 = AileenObject(shape='box', color=Color('red', [1, 0, 0]))
    o2 = AileenObject(shape='box', color=Color('red', [0.8, 0, 0]))
    assert o1 == o2
