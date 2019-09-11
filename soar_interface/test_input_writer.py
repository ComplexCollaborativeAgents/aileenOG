import sys # This seems absurd to have to do it everytime
sys.path.append("strands_qsr_lib/qsr_lib/src") # Add QSR Lib Submodule to path

import pytest
from inputwriter import InputWriter

def test_qsr_input_writer():
    iw = InputWriter(None,None)
    objects = [{'orientation': [1.0, -5.75539615965681e-17, 3.38996313371214e-17, 5.75539615965681e-17, 1.0, 2.98427949019241e-17, -3.38996313371214e-17, -2.98427949019241e-17, 1.0], 'bounding_object': 'Box', 'held': 'false', 'bounding_box': [0.721, 0.3998037998119487, -0.249, 0.8210000000000001, 0.49980379981194867, -0.14900000000000002], 'position': [0.771, 0.4498037998119487, -0.199], 'id': 397}, {'orientation': [1.0, 4.8853319907279786e-17, -4.193655877514327e-17, -4.8853319907279786e-17, 1.0, -1.80524117148876e-16, 4.193655877514327e-17, 1.80524117148876e-16, 1.0], 'bounding_object': 'Cylinder', 'held': 'false', 'bounding_box': [0.369851, 0.39992295234206066, 0.067742, 0.46985099999999996, 0.49992295234206063, 0.167742], 'position': [0.419851, 0.44992295234206064, 0.117742], 'id': 403}]
    res = iw.create_qsrs(objects)
    assert len(res)==2
    assert len(res['403']['397']) == 2
