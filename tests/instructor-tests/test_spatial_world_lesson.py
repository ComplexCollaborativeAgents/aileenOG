from instructor.spatial_word_lesson import SpatialWordLesson

def test_spatial_word_lesson_negative_sample():
    lesson_object = SpatialWordLesson(
        is_positive=False,
        signal='verify',
        description={"objects": [{"color": "blue", "shape": "cone"},
                                {"color": "red", "rgb": [1, 0, 0], "shape": "cylinder"}],
                    "relation": "left-of"},
        distractors=0,
        content=None
    )
    assert lesson_object._spatial_configuration_def != lesson_object._spatial_configurations_set[lesson_object._spatial_configuration]

def test_spatial_word_lesson_positive_sample():
    lesson_object = SpatialWordLesson(
        is_positive=True,
        signal='verify',
        description={"objects": [{"color": "blue", "shape": "cone"},
                                {"color": "red", "rgb": [1, 0, 0], "shape": "cylinder"}],
                    "relation": "left-of"},
        distractors=0,
        content=None
    )
    assert lesson_object._spatial_configuration_def == lesson_object._spatial_configurations_set[lesson_object._spatial_configuration]



