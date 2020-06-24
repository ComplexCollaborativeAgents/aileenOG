# IKPY setup

These are instructions for installing IKPY into an existing AILEEN environment.

## Install Instructions

Activate aileen environment

`conda activate aileen`

Install ikpy

`pip install ikpy`

Switch branch to the ur10_action_sim

`git checkout kae/ur10_action_sim`

There will be a Ur10e.proto file in the main aileen directory that needs to be placed in the webots directory.

`sudo cp ./Ur10e.proto /usr/local/webots/projects/robots/universal_robots/protos`

The webots_headless script should be pointed at the correct world file, but I think it's useful to just go ahead and check.  Open webots and open the world entitled:

`aileen_world_ur10.wbt`

That should be it!
