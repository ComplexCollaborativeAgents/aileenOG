import time

import settings
from world.controllers.aileen_supervisor import AileenSupervisor
from world_server import AileenWorldServer


def create_and_run_aileen_world_server(controller):
    server = AileenWorldServer(controller, port=settings.WORLD_PORT)
    server.run_in_background()


if __name__ == '__main__':
    aileen_supervisor = AileenSupervisor()
    # aileen_supervisor.run_in_background()
    create_and_run_aileen_world_server(aileen_supervisor)
    while True:
        time.sleep(0.001)
