from instructor.curriculum import Curriculum


def test_visual():
    curriculum = Curriculum(
        [
            {"lesson": "visual",
             "description": "red box",
             "color": "red",
             "shape": "box",
             "signal": "verify"},
            {"lesson": "visual",
             "description": "blue cone",
             "color": "blue",
             "shape": "cone",
             "position": [0.456, 0.45, -0.123],
             "signal": "verify"},
            {"lesson": "visual",
             "description": "red cylinder",
             "color": "red",
             "shape": "cylinder",
             "distractors": 4,
             "signal": "verify"},
        ])

    lesson = next(curriculum)
    assert lesson["interaction"]["content"] == "red box"
    assert lesson["interaction"]["signal"] == "verify"

    lesson = next(curriculum)
    assert lesson["interaction"]["content"] == "blue cone"
    assert lesson["interaction"]["signal"] == "verify"

    lesson = next(curriculum)
    assert lesson["interaction"]["content"] == "red cylinder"
    assert len(lesson["scene"]) == 5


def test_spatial():
    curriculum = Curriculum(
        [
            {"lesson": "spatial",
             "description": "red box left of blue cylinder",
             "language": [
                 {"color": "red", "shape": "box"},
                 "left-of",
                 {"color": "blue", "shape": "cylinder"}
             ],
             "signal": "verify"},
        ])

    lesson = next(curriculum)
    assert len(lesson["scene"]) == 2
