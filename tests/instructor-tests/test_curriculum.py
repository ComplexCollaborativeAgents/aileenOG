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
            },
            {
                "lesson-type": "visual-word",
                "description": {"color": "red",
                                "shape": "box"},
                "signal": "inform",
                "is_positive": "False",
                "distractors": 2
            }
        ])

    lesson = next(curriculum)['object'].generate_lesson()
    assert lesson["interaction"]["content"] == "red box"
    assert lesson["interaction"]["signal"] == "verify"

    lesson = next(curriculum)['object'].generate_lesson()
    assert lesson["interaction"]["content"] == "blue cone"
    assert lesson["interaction"]["signal"] == "verify"

    lesson = next(curriculum)['object'].generate_lesson()
    assert lesson["interaction"]["content"] == "red cylinder"
    assert len(lesson["scene"]) == 5

    lesson = next(curriculum)['object'].generate_lesson()
    assert lesson["interaction"]["content"] != "red box"

    lesson = next(curriculum)['object'].generate_lesson()
    assert lesson["interaction"]["content"] != "red box"
    assert len(lesson["scene"]) == 3


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

    lesson = next(curriculum)['object'].generate_lesson()
    assert len(lesson["scene"]) == 2

def test_spatial():
    curriculum = Curriculum(
        [
            {
                "lesson-type": "spatial-word",
                "description": {
                    "objects": [{"color": "blue", "shape": "cone"},
                                {"color": "red", "rgb": [1, 0, 0], "shape": "cylinder"}],
                    "relation": "left-of"},
                "signal": "verify",
                "is_positive": "False"
            },
        ])
    lesson = next(curriculum)['object'].generate_lesson()
    print lesson

def test_action():
    curriculum = Curriculum(
        [{
            "comment": "preload visual concepts used in this example",
            "lesson-type": "action-word",
            "content": "move blue cone right of red cylinder",
            "description": {
                "objects": ["<obj1>", "<obj2>"],
                "relations": ["right-of"],
                "action": "move",
                "initial_configuration": [],
                "trace-actions": [
                    {"name": "pick-up", "argument": "<obj1>"},
                    {"name": "place", "argument1": "<obj1>", "argument2": "<obj2>", "relation": "right-of"}],
                "<obj1>": {"color": "blue", "shape": "cone"},
                "<obj2>": {"color": "red", "shape": "cylinder"}
            },
            "signal": "inform",
            "distractors": 0
            }]
    )
    lesson = next(curriculum)
    lobject = lesson['object']
    lobject.generate_lesson()
    assert len(lesson['object'].get_next_segment()['scene']) == 2