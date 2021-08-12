from ikpy.chain import Chain
from ikpy.link import OriginLink, URDFLink
from ikpy.utils.geometry import *
import numpy as np

def test_chain():
    chain = Chain(links=[
                        OriginLink(),
                        URDFLink(name='shoulder_pan_joint',
                                 translation_vector=[0,0,.1273],
                                 orientation=[0,0,0],
                                 rotation=[0,0,1]),
                        URDFLink(name='shoulder_lift_joint',
                                translation_vector=[0, .220941, 0],
                                orientation=[0, 1.57, 0],
                                rotation=[0,1,0]),
                        URDFLink(name='elbow_joint',
                                translation_vector=[0, -.1719, .612],
                                orientation=[0,0,0],
                                rotation=[0,1,0]),
                        URDFLink(name='wrist_1_joint',
                                translation_vector=[0, 0, .5723],
                                orientation=[0, 1.57,0],
                                rotation=[0,1,0]),
                        URDFLink(name='wrist_2_joint',
                                translation_vector=[0, .1149, 0],
                                orientation=[0,0,0],
                                rotation=[0,0,1]),
                        URDFLink(name='wrist_3_joint',
                                translation_vector=[0,0,.1157],
                                orientation=[0,0,0],
                                rotation=[0,1,0]),
                        URDFLink(name='ee',
                                translation_vector=[0, .0922,0],
                                orientation=[0,0,1.57],
                                rotation=[0,0,1])],
                        active_links_mask=[True, True, True, True, True, True, True, True])

    sol = None
    try:
        sol = chain.inverse_kinematics(target_position = [.5, .5, .5], target_orientation = [0,1,0] , orientation_mode = 'Z')
    except:
        pass
    #print('sol is {}'.format(sol))
    assert sol is not None
