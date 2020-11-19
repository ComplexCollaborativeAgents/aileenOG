from experiments.experiment.generator import Generator

def test_generate_training_gamut_visual_word_all():
    generator = Generator(
        lesson_type="visual-word",
        num_episodes_per_concept=2,
        max_distractors=5,
        exam_length=7
    )

    gamut = generator.generate_inform_training_gamut()
    assert len(gamut) is 24*2

    for lesson in gamut:
        if lesson['signal'] is not 'inform':
            assert False
        if lesson['lesson-type'] != "visual-word":
            assert False
        if lesson['is_positive'] is not 'True':
            assert False
        assert 'distractors' not in lesson.keys()


def test_generate_test_gamut_visual_word_color_generality_all():
    generator = Generator(
        lesson_type="visual-word",
        num_episodes_per_concept=4,
        max_distractors=2,
        exam_length=7
    )
    gamut = generator.generate_verify_testing_gamut_generality()
    assert len(gamut) is 7
    print gamut
    for lesson in gamut:
        if lesson['signal'] is not 'verify':
            assert False
        if lesson['lesson-type'] != "visual-word":
            assert False
        if lesson['is_positive'] is not 'True':
            assert False
        if lesson['description']['shape'] is not 'box':
            assert False
        assert lesson['distractors'] >= 0
        assert lesson['distractors'] <= 2

def test_generate_training_gamut_visual_word_color():
    generator = Generator(
        lesson_type="visual-word",
        experiment_concept="red",
        num_episodes_per_concept=3,
        max_distractors=2,
        exam_length=5
    )
    gamut = generator.generate_inform_training_gamut()
    assert len(gamut) is 3
    for lesson in gamut:
        if lesson['signal'] is not 'inform':
            assert False
        if lesson['lesson-type'] != "visual-word":
            assert False
        if lesson['is_positive'] is not 'True':
            assert False
        if lesson['description']['color'] is not 'red':
            assert False
        assert 'distractors' not in lesson.keys()

def test_generate_training_gamut_visual_word_color():
    generator = Generator(
        lesson_type="visual-word",
        experiment_concept="box",
        num_episodes_per_concept=7,
        max_distractors=2,
        exam_length=3
    )
    gamut = generator.generate_inform_training_gamut()
    assert len(gamut) is 7
    print gamut
    for lesson in gamut:
        if lesson['signal'] is not 'inform':
            assert False
        if lesson['lesson-type'] != "visual-word":
            assert False
        if lesson['is_positive'] is not 'True':
            assert False
        if lesson['description']['shape'] is not 'box':
            assert False
        assert 'distractors' not in lesson.keys()

def test_generate_test_gamut_visual_word_color_generality():
    generator = Generator(
        lesson_type="visual-word",
        experiment_concept="box",
        num_episodes_per_concept=3,
        max_distractors=2,
        exam_length=7
    )
    gamut = generator.generate_verify_testing_gamut_generality()
    assert len(gamut) is 7
    print gamut
    for lesson in gamut:
        if lesson['signal'] is not 'verify':
            assert False
        if lesson['lesson-type'] != "visual-word":
            assert False
        if lesson['is_positive'] is not 'True':
            assert False
        if lesson['description']['shape'] is not 'box':
            assert False
        assert lesson['distractors'] >= 0
        assert lesson['distractors'] <= 2

def test_generate_test_gamut_visual_word_color_generality():
    generator = Generator(
        lesson_type="visual-word",
        experiment_concept="box",
        num_episodes_per_concept=3,
        max_distractors=2,
        exam_length=7
    )
    gamut = generator.generate_verify_testing_gamut_specificity()
    assert len(gamut) is 7
    print gamut
    for lesson in gamut:
        if lesson['signal'] is not 'verify':
            assert False
        if lesson['lesson-type'] != "visual-word":
            assert False
        if lesson['is_positive'] is not 'False':
            assert False
        if lesson['description']['shape'] is not 'box':
            assert False
        assert lesson['distractors'] >= 0
        assert lesson['distractors'] <= 2

def test_generate_training_gamut_spatial_word():
    generator = Generator(
        lesson_type="spatial-word",
        experiment_concept="behind-disjunctive",
        num_episodes_per_concept=4,
        max_distractors=3
    )

    training_gamut = generator.generate_inform_training_gamut()
    assert len(training_gamut) == 4

    for lesson in training_gamut:
        if lesson['signal'] is not 'inform':
            assert False
        if lesson['lesson-type'] != 'spatial-word':
            assert False
        if lesson['is_positive'] is not 'True':
            assert False
        if lesson['description']['relation'] is not 'behind-disjunctive':
            assert False
        assert 'distractors' not in lesson.keys()

def test_generate_test_gamut_spatial_word_generality():
    generator = Generator(
        lesson_type="spatial-word",
        experiment_concept="behind-disjunctive",
        num_episodes_per_concept=4,
        exam_length=7,
        max_distractors=3
    )

    test_gamut = generator.generate_verify_testing_gamut_generality()

    print test_gamut

    assert len(test_gamut) == 7

    for lesson in test_gamut:
        if lesson['signal'] is not 'verify':
            assert False
        if lesson['lesson-type'] != 'spatial-word':
            assert False
        if lesson['is_positive'] is not 'True':
            assert False
        if lesson['description']['relation'] is not 'behind-disjunctive':
            assert False
        assert lesson['distractors'] >= 0
        assert lesson['distractors'] <= 3

def test_generate_test_gamut_spatial_word_specificity():
    generator = Generator(
        lesson_type="spatial-word",
        experiment_concept="behind-disjunctive",
        num_episodes_per_concept=4,
        exam_length=7,
        max_distractors=3
    )

    test_gamut = generator.generate_verify_testing_gamut_generality()

    print test_gamut

    assert len(test_gamut) == 7

    for lesson in test_gamut:
        if lesson['signal'] is not 'verify':
            assert False
        if lesson['lesson-type'] != 'spatial-word':
            assert False
        if lesson['is_positive'] is not 'True':
            assert False
        if lesson['description']['relation'] is not 'behind-disjunctive':
            assert False
        assert lesson['distractors'] >= 0
        assert lesson['distractors'] <= 3


