from instructor.curriculum import Curriculum


def test_visual():
    curriculum = Curriculum(
        [
            {"lesson-type": "visual-word",
             "content": "red box",
             "description": {"color": "red",
                             "shape": "box"},
             "signal": "verify"},
            {"lesson-type": "visual-word",
             "content": "blue cone",
             "description": {"color": "blue",
                             "shape": "cone",
                             "position": [0.456, 0.45, -0.123]},
             "signal": "verify"},
            {"lesson-type": "visual-word",
             "content": "red cylinder",
             "description": {"color": "red",
                             "shape": "cylinder"},
             "distractors": 4,
             "signal": "verify"},
            {
                "lesson-type": "visual-word",
                "description": {"color": "red",
                                "shape": "box"},
                "signal": "inform",
                "is_positive": "False"
            }
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

    lesson = next(curriculum)
    assert lesson["interaction"]["content"] != "red box"


def test_spatial():
    curriculum = Curriculum(
        [
            {
                "lesson-type": "spatial-word",
                "content": "blue cone left of red cylinder",
                "description": {
                    "objects": [{"color": "blue", "shape": "cone"},
                                {"color": "red", "rgb": [1, 0, 0], "shape": "cylinder"}],
                    "relation": "left-of"},
                "signal": "inform"
            },
        ])

    lesson = next(curriculum)
    assert len(lesson["scene"]) == 2
